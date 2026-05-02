## =============================================================================
## Banco Mundial - Herramienta de Microsimulación
## Country: República Dominicana 2026
## Authors: Maynor Cabrera, Renato Vargas
## Script:  05_DOM26_ConsolidaEscenarios.R (refactorizado)
## E-mail:  mynorvc@gmail.com
## Dependencias: 0_GTM23WBN_Master.R
## Input files
## -- data/pipeline/r_params/param_csv/*.csv (escenarios solo si no viene del runner)
## -- Procesados
## ---- DOM_simcompensacion.rds
##
## Output files:
## -- DOM_resultados.rds
## -- DOM_resumen.rds
##
## Fecha de creación:         5 Mar 2026 - 15:02
## Version:                   1.3
## Fecha de modificación:     2026-04-09


# ==============================================================================
# 1. CONFIGURACIÓN E IMPORTACIÓN
# ==============================================================================

if (!exists("fparam_csv", inherits = FALSE)) {
  stop("Se requiere fparam_csv (directorio data/pipeline/r_params/param_csv/).")
}
DOM_results <- readRDS(paste0(fdbmod, "DOM_simcompensacion.rds"))

# --- Parámetros globales ------------------------------------------------------

# Estratos según PPA (línea de pobreza internacional, actualizado al PPP 2021)
PPP_FACTOR  <- 23.135822 * (174.88908 / 148.47661)
estratos    <- PPP_FACTOR * 365 * c(8.3, 17, 98)

# Escenarios activos (la app local puede fijar `escenarios` en el entorno y
# poner option(dom.skip_escenarios_reload = TRUE) para no releer el Excel).
if (!isTRUE(getOption("dom.skip_escenarios_reload", FALSE))) {
  escenarios <- read_param_csv("escenarios", fparam_csv) %>%
    filter(activo == 1)
}

esc_inc <- c(0, unique(escenarios$escenario))

# Valores macroeconómicos (extraídos una sola vez)
macro <- list(
  pib = parm_glob$valor[str_detect(parm_glob$descripcion, "PIB")],
  itb = parm_glob$valor[str_detect(parm_glob$descripcion, "total ITBIS")],
  isr = parm_glob$valor[str_detect(parm_glob$descripcion, "renta")],
  sub = parm_glob$valor[str_detect(parm_glob$descripcion, "subsidios")]
)

# ==============================================================================
# 2. FUNCIONES DE INDICADORES (Gini, Theil, Palma, FGT) ------------------------
# ==============================================================================
# Fuente: https://github.com/wbEPL/devCEQ/blob/main/R/fct_gini_poverty.R

#' Limpieza común de vectores antes de calcular indicadores
#' @return lista con x y w limpios, o NA_real_ si no es posible
limpiar_vectores <- function(x, w = NULL, na.rm = TRUE, 
  drop_zero_and_less = TRUE) {
  if (!na.rm && any(is.na(x))) return(NA_real_)
  if (is.null(w)) w <- rep(1, length(x))

  if (na.rm) {
    validos <- !is.na(x)
    x <- x[validos]
    w <- w[validos]
  }

  if (drop_zero_and_less) {
    positivos <- x > 0
    x <- x[positivos]
    w <- w[positivos]
  }

  list(x = x, w = w)
}


#' Coeficiente de Gini
calc_gini <- function(x, w = rep(1, length(x)), na.rm = TRUE,
                      drop_zero_and_less = TRUE) {
  v <- limpiar_vectores(x, w, na.rm, drop_zero_and_less)
  if (is.numeric(v) && is.na(v)) return(NA_real_)

  x <- sort(v$x)
  w <- v$w[order(v$x)] / sum(v$w)

  w_cum  <- cumsum(w)
  xw_cum <- cumsum(w * x)
  xw_cum <- xw_cum / xw_cum[length(x)]

  as.numeric(t(xw_cum[-1]) %*% w_cum[-length(x)] -
               t(xw_cum[-length(x)]) %*% w_cum[-1])
}

#' Índice de Concentración
#' @param y Variable a medir (ej. impuesto, gasto)
#' @param x Variable de ranking (ej. ingreso)
#' @param w Pesos/ponderadores
calc_concentration <- function(y, x, w = rep(1, length(y)), na.rm = TRUE,
                               drop_zero_and_less = FALSE) {
  
  if (is.null(w)) w <- rep(1, length(y))
  
  # Limpieza manual (sin limpiar_vectores, que no soporta dos vectores)
  if (na.rm) {
    validos <- !is.na(y) & !is.na(x) & !is.na(w)
    y <- y[validos]
    x <- x[validos]
    w <- w[validos]
  }
  
  if (drop_zero_and_less) {
    positivos <- x > 0
    y <- y[positivos]
    x <- x[positivos]
    w <- w[positivos]
  }
  
  # Ordenar por X (ingreso de referencia)
  orden  <- order(x)
  y_ord  <- y[orden]
  w_ord  <- w[orden] / sum(w)
  
  w_cum  <- cumsum(w_ord)
  yw_cum <- cumsum(y_ord * w_ord)
  yw_cum <- yw_cum / yw_cum[length(y_ord)]
  
  ic <- as.numeric(
    t(yw_cum[-1]) %*% w_cum[-length(y_ord)] -
      t(yw_cum[-length(y_ord)]) %*% w_cum[-1]
  )
  
  return(ic)
}

#' Índice de Kakwani
#' @param tax Variable de impuesto o gasto
#' @param income Variable de ingreso
#' @param w Pesos
calc_kakwani <- function(tax, income, w = rep(1, length(income))) {
  
  # 1. Gini del Ingreso
  gini_income <- calc_gini(x = income, w = w)
  
  # 2. Índice de Concentración del Impuesto (ordenado por ingreso)
  ic_tax <- calc_concentration(y = tax, x = income, w = w)
  
  # 3. Kakwani = IC - Gini
  kakwani <- ic_tax - gini_income
  
  return(list(
    kakwani = kakwani,
    gini_income = gini_income,
    concentration_index = ic_tax
  ))
}


#' Índice de Theil
calc_theil <- function(x, w = rep(1, length(x)), na.rm = TRUE,
                       drop_zero_and_less = TRUE) {
  v <- limpiar_vectores(x, w, na.rm, drop_zero_and_less)
  if (is.numeric(v) && is.na(v)) return(NA_real_)

  w_ratio  <- v$w / sum(v$w, na.rm = na.rm)
  x_media  <- sum(v$x * w_ratio, na.rm = na.rm) / sum(w_ratio, na.rm = na.rm)
  x_ratio  <- v$x / x_media
  sum(w_ratio * x_ratio * log(x_ratio), na.rm = na.rm)
}


#' Índice de Palma (ratio D10 / D1-D4)
calc_palma <- function(x, w = rep(1, length(x)), d = rep(1, length(x)),
                       na.rm = TRUE, drop_zero_and_less = TRUE) {
  if (!na.rm && any(is.na(x))) return(NA_real_)
  if (is.null(w)) w <- rep(1, length(x))

  if (na.rm) {
    validos <- !is.na(x)
    x <- x[validos]; w <- w[validos]; d <- d[validos]
  }
  if (drop_zero_and_less) {
    positivos <- x > 0
    x <- x[positivos]; w <- w[positivos]; d <- d[positivos]
  }

  sum(x[d == 10] * w[d == 10], na.rm = na.rm) /
    sum(x[d < 5]  * w[d < 5],  na.rm = na.rm)
}


#' FGT (Foster-Greer-Thorbecke)
#' @param alpha 0 = incidencia, 1 = brecha, 2 = severidad
calc_pov_fgt <- function(x, pl, alpha = 0, w = rep(1, length(x)),
                         na.rm = TRUE, ...) {
  if (is.null(w)) w <- rep(1, length(x))
  if (na.rm) {
    validos <- !is.na(x) & !is.na(w) & !is.na(pl)
    x <- x[validos]; w <- w[validos]; pl <- pl[validos]
  }
  sum(w * ifelse(x < pl, ((pl - x) / pl)^alpha, 0), na.rm = TRUE) /
    sum(w, na.rm = TRUE)
}

# 2b. FUNCIÓN DE ESTIMACIÓN DE KAKWANI 
#' Estima índices de Kakwani para ISR, ITBIS, subsidios y compensaciones
#' en escenario base y cada escenario de simulación,
#' siempre usando yd0_pc como ingreso de referencia para el ranking.
#'
#' @param data data.frame con microdatos
#' @param escenarios_nums vector numérico con los números de escenario activos
estima_kakwani <- function(data, escenarios_nums) {
  
  w <- data$factor_expansion_anual
  income_ref <- data$yd0_pc
  gini_ref <- calc_gini(x = income_ref, w = w)
  
  instrumentos <- list(
    isr  = list(base = "dtx_isr0_pc", esc_prefix = "dtx_isr",
      esc_suffix = "_pc", tipo = "impuesto"),
    itb  = list(base = "itx_itb0_pc", esc_prefix = "itx_itb",
      esc_suffix = "_pc", tipo = "impuesto"),
    sub  = list(base = "sub_ele0_pc", esc_prefix = "sub_ele",
      esc_suffix = "_pc", tipo = "transferencia"),
    comp = list(base = NULL,          esc_prefix = "comp_",
      esc_suffix = "_pc", tipo = "transferencia")
  )
  
  # Función auxiliar para calcular Kakwani según tipo
  calc_kk <- function(ic, tipo) {
    if (tipo == "impuesto") ic - gini_ref else gini_ref - ic
  }
  
  purrr::map_dfr(names(instrumentos), function(inst_name) {
    inst <- instrumentos[[inst_name]]
    
    # --- Escenario base -------------------------------------------------------
    fila_base <- if (!is.null(inst$base) && inst$base %in% names(data)) {
      ic <- calc_concentration(y = data[[inst$base]], x = income_ref, w = w)
      tibble(instrumento         = inst_name,
             tipo                = inst$tipo,
             escenario           = 0L,
             kakwani             = calc_kk(ic, inst$tipo),
             gini_income         = gini_ref,
             concentration_index = ic)
    } else {
      tibble(instrumento         = inst_name,
             tipo                = inst$tipo,
             escenario           = 0L,
             kakwani             = NA_real_,
             gini_income         = gini_ref,
             concentration_index = NA_real_)
    }
    
    # --- Escenarios de simulación ---------------------------------------------
    filas_esc <- purrr::map_dfr(escenarios_nums, function(num) {
      col_name <- paste0(inst$esc_prefix, num, inst$esc_suffix)
      
      if (col_name %in% names(data)) {
        ic <- calc_concentration(y = data[[col_name]], x = income_ref, w = w)
        tibble(instrumento         = inst_name,
               tipo                = inst$tipo,
               escenario           = as.integer(num),
               kakwani             = calc_kk(ic, inst$tipo),
               gini_income         = gini_ref,
               concentration_index = ic)
      } else {
        tibble(instrumento         = inst_name,
               tipo                = inst$tipo,
               escenario           = as.integer(num),
               kakwani             = NA_real_,
               gini_income         = NA_real_,
               concentration_index = NA_real_)
      }
    })
    
    bind_rows(fila_base, filas_esc)
  })
}

# ==============================================================================
# 3. FUNCIONES DE BRECHA DE POBREZA --------------------------------------------
# ==============================================================================

#' Brecha de pobreza por hogar (mensual) - para cada escenario
calc_br <- function(x, pl, w = rep(1, length(x)), h = rep(1, length(x)),
                    r = rep(1, length(x)), na.rm = TRUE, ...) {
  if (is.null(w)) w <- rep(1, length(x))
  if (na.rm) {
    validos <- !is.na(x) & !is.na(w) & !is.na(pl) & !is.na(h)
    x <- x[validos]; w <- w[validos]; pl <- pl[validos]
    h <- h[validos]; r  <- r[validos]
  }
  br <- if_else(pl > x, (pl - x) * h / 12, 0)
  weighted.mean(ifelse(r == 1, br, NA), na.rm = TRUE, w = w)
}


#' Brecha para pobres antes de reforma (solo pobres en escenario base)
calc_br0 <- function(x, pl, w = rep(1, length(x)), h = rep(1, length(x)),
                     y0 = rep(1, length(x)), r = rep(1, length(x)),
                     na.rm = TRUE, ...) {
  if (is.null(w)) w <- rep(1, length(x))
  if (na.rm) {
    validos <- !is.na(x) & !is.na(w) & !is.na(pl) & !is.na(h) &
      !is.na(y0) & (y0 < pl)  # solo pobres en escenario base
    x <- x[validos]; w <- w[validos]; pl <- pl[validos]
    h <- h[validos]; y0 <- y0[validos]; r <- r[validos]
  }
  weighted.mean(ifelse(r == 1, (pl - x) * h / 12, NA), na.rm = TRUE, w = w)
}

#' Brecha para nuevos pobres después de reforma
calc_br1 <- function(x, pl, w = rep(1, length(x)), h = rep(1, length(x)),
                     y0 = rep(1, length(x)), r = rep(1, length(x)),
                     na.rm = TRUE, ...) {
  if (is.null(w)) w <- rep(1, length(x))
  if (na.rm) {
    validos <- !is.na(x) & !is.na(w) & !is.na(pl) & !is.na(h) &
      !is.na(y0) & (y0 >= pl) & (x < pl)  # eran no-pobres y ahora son pobres
    x <- x[validos]; w <- w[validos]; pl <- pl[validos]
    h <- h[validos]; y0 <- y0[validos]; r <- r[validos]
  }
  weighted.mean(ifelse(r == 1, (pl - x) * h / 12, NA), na.rm = TRUE, w = w)
}

#' Brecha de nuevos pobres como proporción del PIB
calc_br2 <- function(x, pl, w = rep(1, length(x)), h = rep(1, length(x)),
                     y0 = rep(1, length(x)), r = rep(1, length(x)),
                     na.rm = TRUE, pib_valor, ...) {
  if (is.null(w)) w <- rep(1, length(x))
  if (na.rm) {
    validos <- !is.na(x) & !is.na(w) & !is.na(pl) & !is.na(h) &
      !is.na(y0) & (y0 >= pl) & (x < pl)
    x <- x[validos]; w <- w[validos]; pl <- pl[validos]
    h <- h[validos]; y0 <- y0[validos]; r <- r[validos]
  }
  sum((pl - x) * w, na.rm = TRUE) / (pib_valor * 10000)
}

# ==============================================================================
# 4. ESTIMACIÓN DE POBREZA -----------------------------------------------------
# ==============================================================================

#' Estima indicadores de pobreza para todos los escenarios de ingreso
#' @param data data.frame con los microdatos
#' @param pline nombre de la variable de línea de pobreza
#' @param r, h, y0 nombres de variables de relación, tamaño hogar, ingreso base
#' @param pib_valor valor del PIB para calc_br2
estima_pov <- function(data, pline, r, h, y0, pib_valor) {

  data_sel <- data %>%
    ungroup() %>%
    select(
      yd0_pc,
      any_of(matches("y[dczis][0-9]_pc")),
      !!sym(pline), !!sym(r), !!sym(h), !!sym(y0),
      factor
    )

  # Guardar yd0_pc para reintroducirlo después del pivot

  yd0_pc_vals <- data_sel$yd0_pc

  resultado <- data_sel %>%
    pivot_longer(names_to = "var", cols = starts_with("y")) %>%
    group_by(var) %>%
    mutate(yd0_pc = yd0_pc_vals) %>%
    summarise(
      rate      = calc_pov_fgt(value, !!sym(pline), 0, factor) * 100,
      headcount = round(calc_pov_fgt(value, !!sym(pline), 0, factor) *
                          sum(factor, na.rm = TRUE)),
      gap       = calc_pov_fgt(value, !!sym(pline), 1, factor) * 100,
      gap_lcuhh = calc_br(value, !!sym(pline), factor, !!sym(h), !!sym(r)),
      gap_lcupo = calc_br0(value, !!sym(pline), factor, !!sym(h), yd0_pc,
        !!sym(r)),
      gap_lcupon    = calc_br1(value, !!sym(pline), factor, !!sym(h), yd0_pc,
        !!sym(r)),
      gap_lcupopib  = calc_br2(value, !!sym(pline), factor, !!sym(h), yd0_pc,
        !!sym(r), pib_valor = pib_valor),
      severity  = calc_pov_fgt(value, !!sym(pline), 2, factor) * 100,
      .groups = "drop"
    ) %>%
    pivot_longer(
      names_to = "parameter",
      cols = c(rate, headcount, gap, gap_lcuhh, gap_lcupo,
               gap_lcupon, gap_lcupopib, severity),
      values_to = "Value"
    ) %>%
    filter(var != "yd_hh") %>%
    mutate(
      escenario = as.numeric(if_else(str_sub(var, 3, 3) == "", "0",
                                     str_sub(var, 3, 3))),
      var = factor(str_sub(var, 1, 2),
                   levels = c("yd", "yc", "yz", "yi", "ys"))
    ) %>%
    arrange(var)

  return(resultado)
}

# ==============================================================================
# 5. MATRICES DE RESULTADOS (pobreza y desigualdad) ----------------------------
# ==============================================================================

#' Función auxiliar para extraer una fila de un data.frame filtrado
extraer_fila <- function(df, cols, fill_na = FALSE) {
  cols_ok <- cols[cols %in% names(df)]
  if (length(cols_ok) < length(cols)) {
    warning("Columnas no encontradas: ",
            paste(setdiff(cols, names(df)), collapse = ", "))
  }
  resultado <- df %>% select(all_of(cols_ok)) %>% unlist(use.names = FALSE)
  if (fill_na) resultado <- replace(resultado, is.na(resultado), 0)
  resultado
}


#' Matriz de pobreza por escenario
pov_matriz <- function(data, suffix) {
  stopifnot(grepl("^_", suffix))

  p1 <- data %>%
    filter(parameter != "severity") %>%
    pivot_wider(names_from = c("var", "escenario"), names_sep = "_",
                values_from = Value) %>%
    mutate(dyd_0 = 0) %>%
    mutate(
      across(matches(paste0("^(yd|yc|yz|yi|ys)", suffix, "$")),
             ~ . - get(sub(suffix, "_0", cur_column())),
             .names = "d{.col}")
    )

  vars_income <- c("yd", "yc", "yz", "yi", "ys")
  cols_base <- c("yd_0", paste0(vars_income, suffix))
  cols_diff <- c("dyd_0", paste0("d", vars_income, suffix))

  result <- rbind(
    extraer_fila(filter(p1, parameter == "rate"),         cols_base),
    extraer_fila(filter(p1, parameter == "rate"),         cols_diff),
    extraer_fila(filter(p1, parameter == "headcount"),    cols_diff),
    extraer_fila(filter(p1, parameter == "gap"),          cols_base),
    extraer_fila(filter(p1, parameter == "gap_lcuhh"),    cols_base),
    extraer_fila(filter(p1, parameter == "gap_lcupo"),    cols_base),
    extraer_fila(filter(p1, parameter == "gap_lcupon"),   cols_base,
      fill_na = TRUE),
    extraer_fila(filter(p1, parameter == "gap_lcupopib"), cols_base,
      fill_na = TRUE)
  ) %>%
    apply(2, round, digits = 4)

  rownames(result) <- c("rate", "drate", "headcount", "gap",
                        "gap_lcuhh", "gap_lcupo", "gap_lcupon", "gap_lcupopib")
  colnames(result) <- c("Base", paste0(vars_income, suffix))
  return(result)
}

#' Matriz de desigualdad por escenario
inq_matriz <- function(data, suffix) {
  stopifnot(grepl("^_", suffix))

  vars_income <- c("yd", "yc", "yz", "yi", "ys")

  p1 <- data %>%
    pivot_wider(names_from = c("var", "escenario"), names_sep = "_",
                values_from = Value) %>%
    mutate(dyd_0 = 0) %>%
    mutate(
      across(matches(paste0("^(yd|yc|yz|yi|ys)", suffix, "$")),
             ~ get(sub(suffix, "_0", cur_column())) - .,
             .names = "d{.col}")
    )

  cols_base <- c("yd_0", paste0(vars_income, suffix))
  cols_diff <- c("dyd_0", paste0("d", vars_income, suffix))

  result <- rbind(
    extraer_fila(filter(p1, parameter == "Gini"),  cols_base),
    extraer_fila(filter(p1, parameter == "Gini"),  cols_diff),
    extraer_fila(filter(p1, parameter == "Palma"), cols_base),
    extraer_fila(filter(p1, parameter == "Theil"), cols_base)
  ) %>%
    apply(2, round, digits = 4)

  rownames(result) <- c("Gini", "dGini", "Palma", "Theil")
  colnames(result) <- c("Base", paste0(vars_income, suffix))
  return(result)
}

#' Matriz de Kakwani por instrumento y escenario
#' Filas: instrumento x parámetro  |  Columnas: Base + escenarios
kakwani_matriz <- function(kakwani_data, escenarios_nums) {
  
  params <- c("concentration_index", "kakwani")
  
  wide <- kakwani_data %>%
    select(instrumento, escenario, all_of(params)) %>%
    mutate(esc_label = if_else(escenario == 0L, "Base",
                               paste0("Esc_", escenario))) %>%
    pivot_wider(
      id_cols     = instrumento,
      names_from  = esc_label,
      values_from = all_of(params),
      names_glue  = "{.value}__{esc_label}"
    )
  
  # Orden explícito basado en los escenarios presentes, no en outer
  esc_labels  <- paste0("Esc_", escenarios_nums)
  col_order   <- c(
    paste0(params, "__Base"),
    as.vector(outer(params, esc_labels,
                    function(p, e) paste0(p, "__", e)))
  )
  col_order <- col_order[col_order %in% names(wide)]
  
  result <- wide %>%
    select(all_of(col_order)) %>%
    mutate(across(everything(), ~ round(., 4))) %>%
    as.matrix()
  
  rownames(result) <- wide$instrumento
  
  # Nombres de columnas legibles
  colnames(result) <- c(
    "ic_Base", "kakwani_Base",
    as.vector(outer(c("ic", "kakwani"), escenarios_nums,
                    function(p, e) paste0(p, "_Esc", e)))
  )
  
  return(result)
}

## =============================================================================
## 6. ANÁLISIS POR DECILES / GRUPOS (funciones unificadas) ---------------------
## =============================================================================

#' Nombres dinámicos de columnas para un escenario dado
#' Centraliza la construcción de nombres para evitar repetirla en cada función
nms_escenario <- function(suffix) {
  list(
    isr   = paste0("dtx_isr",  suffix, "_pc"),
    itb   = paste0("itx_itb",  suffix, "_pc"),
    sub   = paste0("sub_ele",  suffix, "_pc"),
    comp  = paste0("comp_",    suffix, "_pc"),
    difr  = paste0("ddtx_isr", suffix, "_pc"),
    dif1  = paste0("ditx_itb", suffix, "_pc"),
    dif2  = paste0("dsub_ele", suffix, "_pc"),
    dif3  = paste0("dcomp",    suffix, "_pc"),
    neto2 = paste0("neto",     suffix, "_pc"),
    neto  = paste0("nitx",     suffix, "_pc")
  )
}

#' Prepara los microdatos UNA SOLA VEZ por escenario × grupo:
#' aplica mutates de netos y diferencias, luego agrega por grupo.
#'
#' Devuelve una lista con:
#'   $base   — resultado agrupado con divisor = 1 (para con e inc)
#'   $millones — resultado agrupado con divisor = 1e6 (para pib y sum)
#'   $nms    — lista de nombres de columnas
#'   $vars_to_sum — vector de columnas sumadas
#'   $incpc  — vector de ingreso disponible por grupo (para inc)
preparar_deciles_una_vez <- function(data, suffix, weight_var, rank) {
  
  nms <- nms_escenario(suffix)
  
  vars_to_sum <- c(
    "dtx_isr0_pc", "itx_itb0_pc", "sub_ele0_pc", "net0_pc",
    nms$isr, nms$itb, nms$sub, nms$neto2, nms$comp,
    nms$difr, nms$dif1, nms$dif2, nms$dif3, nms$neto
  )
  
  # FIX: filter + mutate se ejecutan UNA sola vez
  data_mut <- data %>%
    filter(!is.na({{ rank }})) %>%
    mutate(
      net0_pc          = dtx_isr0_pc + itx_itb0_pc - sub_ele0_pc,
      !!sym(nms$neto2) := !!sym(nms$isr) + !!sym(nms$itb) - !!sym(nms$sub),
      !!sym(nms$comp)  := !!sym(nms$comp) * -1,
      !!sym(nms$difr)  := !!sym(nms$isr)  - dtx_isr0_pc,
      !!sym(nms$dif1)  := !!sym(nms$itb)  - itx_itb0_pc,
      !!sym(nms$dif2)  := !!sym(nms$sub)  - sub_ele0_pc,
      !!sym(nms$dif3)  := !!sym(nms$comp),
      !!sym(nms$neto)  := !!sym(nms$difr) + !!sym(nms$dif1) -
        !!sym(nms$dif2) + !!sym(nms$dif3)
    )
  
  # FIX: group_by + summarise se ejecutan UNA sola vez (divisor = 1)
  # Se agrega incpc junto con las demás variables en un solo summarise
  result_base <- data_mut %>%
    group_by({{ rank }}) %>%
    summarise(
      incpc = sum(yd_pc * !!sym(weight_var), na.rm = TRUE),
      across(all_of(vars_to_sum),
             ~ sum(.x * !!sym(weight_var), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange({{ rank }}) %>%
    janitor::adorn_totals("row")
  
  # FIX: versión en millones derivada del resultado base (sin re-summarise)
  result_millones <- result_base %>%
    mutate(across(where(is.numeric), ~ .x / 1e6))
  
  list(
    base     = result_base,
    millones = result_millones,
    nms      = nms,
    vars_to_sum = vars_to_sum
  )
}

#' Concentración: % del total que va a cada grupo
#' FIX: recibe prep pre-computado en vez de llamar preparar_deciles internamente
decile_con <- function(prep) {
  result <- prep$base %>% select(-incpc)
  nms    <- prep$nms
  
  num_cols <- result %>% select(where(is.numeric)) %>% names()
  totales  <- result %>% slice_tail(n = 1) %>% select(all_of(num_cols))
  
  result %>%
    mutate(across(all_of(num_cols),
                  ~ round(. * 100 / totales[[cur_column()]], 1))) %>%
    mutate(across(where(is.numeric), ~ if_else(is.na(.), 0, .))) %>%
    mutate(
      !!sym(nms$difr) := !!sym(nms$isr) - dtx_isr0_pc,
      !!sym(nms$dif1) := !!sym(nms$itb) - itx_itb0_pc,
      !!sym(nms$dif2) := !!sym(nms$sub) - sub_ele0_pc
    )
}

#' Incidencia fiscal: variable / ingreso disponible del grupo
#' FIX: recibe prep pre-computado en vez de llamar preparar_deciles internamente
decile_inc <- function(prep) {
  result <- prep$base
  
  result %>%
    mutate(across(all_of(prep$vars_to_sum),
                  ~ round((.x * 100) / incpc, 2))) %>%
    select(-incpc)
}

#' Totales en millones
#' FIX: recibe prep pre-computado, usa result_millones ya calculado
decile_sum <- function(prep) {
  prep$millones %>%
    select(-incpc) %>%
    mutate(across(where(is.numeric), ~ round(., 1)))
}

#' Totales como % del PIB
#' FIX: recibe prep pre-computado, deriva de result_millones sin re-summarise
decile_pib <- function(prep, v_pib, f_isr, f_itb, f_sub) {
  nms      <- prep$nms
  vars_base <- c("dtx_isr0_pc", "itx_itb0_pc", "sub_ele0_pc", "net0_pc",
                 nms$isr, nms$itb, nms$sub, nms$neto2, nms$comp)
  
  prep$millones %>%
    select(-incpc) %>%
    # Escalar a % del PIB
    mutate(
      dtx_isr0_pc      = dtx_isr0_pc      / f_isr * 100,
      itx_itb0_pc      = itx_itb0_pc      / f_itb * 100,
      sub_ele0_pc      = sub_ele0_pc      / f_sub * 100,
      net0_pc          = net0_pc          / v_pib * 100,
      !!sym(nms$isr)   := !!sym(nms$isr)   / f_isr * 100,
      !!sym(nms$itb)   := !!sym(nms$itb)   / f_itb * 100,
      !!sym(nms$sub)   := !!sym(nms$sub)   / f_sub * 100,
      !!sym(nms$neto2) := !!sym(nms$neto2) / v_pib * 100,
      !!sym(nms$comp)  := !!sym(nms$comp)  / v_pib * 100
    ) %>%
    # Recalcular diferencias post-escalado (igual que antes)
    mutate(
      net0_pc          = dtx_isr0_pc + itx_itb0_pc - sub_ele0_pc,
      !!sym(nms$difr)  := !!sym(nms$isr)  - dtx_isr0_pc,
      !!sym(nms$dif1)  := !!sym(nms$itb)  - itx_itb0_pc,
      !!sym(nms$dif2)  := !!sym(nms$sub)  - sub_ele0_pc,
      !!sym(nms$dif3)  := !!sym(nms$comp),
      !!sym(nms$neto)  := !!sym(nms$difr) + !!sym(nms$dif1) -
        !!sym(nms$dif2) + !!sym(nms$dif3)
    ) %>%
    mutate(across(where(is.numeric), ~ round(., 2)))
}


# ==============================================================================
# 7. RESUMEN POR ESCENARIO -----------------------------------------------------
# ==============================================================================

GRUPOS_ANALISIS <- list(
  dec  = list(var = quote(decyd),        prefijo = "dec"),
  estr = list(var = quote(gr_estrato0),  prefijo = "estr"),
  urb  = list(var = quote(urban),        prefijo = "urb"),
  sex  = list(var = quote(sjefe),        prefijo = "sex"),
  cat  = list(var = quote(cathhd),       prefijo = "cat"),
  dep  = list(var = quote(macro_region), prefijo = "dep")
)

#' Resumen completo para un escenario
#' FIX: preparar_deciles_una_vez se llama una sola vez por grupo,
#'      y sus resultados se pasan a decile_con, decile_pib y decile_inc
sum_escenarios <- function(esc_num, data, pov_gral, pov_ext, pov_int,
                           ineq_data, kakwani_data, escenarios_nums,
                           v_pib, f_isr, f_itb, f_sub) {
  suff <- paste0("_", as.character(esc_num))
  
  resultado <- list(
    pov_gral = pov_matriz(pov_gral, suff),
    pov_ext  = pov_matriz(pov_ext,  suff),
    pov_int  = pov_matriz(pov_int,  suff),
    ineq     = inq_matriz(ineq_data, suff),
    kakwani  = {
      km <- kakwani_matriz(
        filter(kakwani_data, escenario %in% c(0, esc_num)),
        escenarios_nums = esc_num
      )
      km[is.nan(as.matrix(km))] <- NA
      km
    }
  )
  
  for (g in GRUPOS_ANALISIS) {
    rank_var <- g$var
    pref     <- g$prefijo
    
    # FIX: una sola llamada al trabajo pesado por grupo
    prep <- preparar_deciles_una_vez(data, esc_num, "factor", !!rank_var)
    
    resultado[[paste0(pref, "con")]] <- decile_con(prep)
    resultado[[paste0(pref, "sum")]] <- decile_pib(prep, v_pib, f_isr, f_itb,
      f_sub)
    resultado[[paste0(pref, "inc")]] <- decile_inc(prep)
  }
  
  resultado
}

# ==============================================================================
# 8. ETIQUETAS (labels) --------------------------------------------------------
# ==============================================================================

#' Configuración de etiquetas para cada variable de agrupación
LABELS_CONFIG <- list(
  decyd = list(
    var_label = "Decil de ingreso disponible",
    val_labels = c("Decil 1" = "1", "Decil 2" = "2", "Decil 3" = "3",
                   "Decil 4" = "4", "Decil 5" = "5", "Decil 6" = "6",
                   "Decil 7" = "7", "Decil 8" = "8", "Decil 9" = "9",
                   "Decil 10" = "10", "Total" = "Total")
  ),
  gr_estrato0 = list(
    var_label = "Estrato socioeconómico",
    val_labels = c("Pobres" = "1", "Vulnerables" = "2",
                   "Clase media" = "3", "Residual" = "4", "Total" = "Total")
  ),
  urban = list(
    var_label = "Área geográfica",
    val_labels = c("Rural" = "FALSE", "Urbano" = "TRUE", "Total" = "Total")
  ),
  sjefe = list(
    var_label = "Sexo del jefe de hogar",
    val_labels = c("Masculino" = "1", "Femenino" = "2", "Total" = "Total")
  ),
  cathhd = list(
    var_label = "Categoría del hogar",
    val_labels = c("Sin niños y adultos mayores" = "1",
                   "Con niños y sin adultos mayores" = "2",
                   "Sin niños y con adultos mayores" = "3",
                   "Con niños y adultos mayores" = "4", "Total" = "Total")
  ),
  macro_region = list(
    var_label = "Región macroeconómica",
    val_labels = c("Ozama" = "1", "Norte" = "2", "Sur" = "3",
                   "Este" = "4", "Total" = "Total")
  )
)

#' Aplica etiquetas a todos los data.frames de un escenario
aplicar_labels <- function(escenario) {
  targets <- names(escenario)[grepl("(con|inc|sum)$", names(escenario))]

  escenario[targets] <- lapply(targets, function(nombre) {
    df <- escenario[[nombre]]
    primera_col <- names(df)[1]

    if (primera_col %in% names(LABELS_CONFIG)) {
      cfg <- LABELS_CONFIG[[primera_col]]
      labelled::var_label(df[[primera_col]])  <- cfg$var_label
      labelled::val_labels(df[[primera_col]]) <- cfg$val_labels
    }
    df
  })

  escenario
}

# ==============================================================================
# 9. EJECUCIÓN PRINCIPAL -------------------------------------------------------
# ==============================================================================

# --- Valores de referencia base -----------------------------------------------
vs_isr <- fsum(DOM_results$dtx_isr0_pc,
               w = DOM_results$factor_expansion_anual, na.rm = TRUE) / 1e6
vs_itb <- fsum(DOM_results$itx_itb0_pc,
               w = DOM_results$factor_expansion_anual, na.rm = TRUE) / 1e6
vs_sub <- fsum(DOM_results$sub_ele0_pc,
               w = DOM_results$factor_expansion_anual, na.rm = TRUE) / 1e6

# Factores de escala para % del PIB
f_isr <- vs_isr / (macro$isr / macro$pib)
f_itb <- vs_itb / (macro$itb / macro$pib)
f_sub <- vs_sub / (macro$sub / macro$pib)

# --- Indicadores de desigualdad -----------------------------------------------
ineq <- DOM_results %>%
  mutate(factor = factor_expansion_anual) %>%
  ungroup() %>%
  select(any_of(matches("y[dczis][0-9]_pc")), factor) %>%
  summarise(
    across(starts_with("y"), ~ calc_gini(., factor),  .names = "Gini__{.col}"),
    across(starts_with("y"), ~ calc_theil(., factor), .names = "Theil__{.col}"),
    across(starts_with("y"),
           ~ calc_palma(., factor, DOM_results$decyd), .names = "Palma__{.col}")
  ) %>%
  pivot_longer(everything(), names_to = "var", values_to = "Value") %>%
  separate(var, into = c("parameter", "var"), sep = "__") %>%
  separate(var, into = c("var", "escenario"), sep = "_") %>%
  mutate(
    escenario = as.numeric(if_else(str_sub(var, 3, 3) == "", "0",
                                   str_sub(var, 3, 3))),
    var = str_sub(var, 1, 2)
  )

# --- Indicadores de pobreza ---------------------------------------------------
DOM_results <- DOM_results %>%
  mutate(
    factor    = factor_expansion_anual,
    pline_830 = 365 * 8.30 * PPP_FACTOR
  )

pov_gral <- estima_pov(DOM_results, "pline_mod", "relation", "hsize", "yd0_pc",
                       pib_valor = macro$pib)
pov_ext  <- estima_pov(DOM_results, "pline_ext", "relation", "hsize", "yd0_pc",
                       pib_valor = macro$pib)
pov_int  <- estima_pov(DOM_results, "pline_830", "relation", "hsize", "yd0_pc",
                       pib_valor = macro$pib)

# --- Preparar variables adicionales -------------------------------------------
DOM_results <- DOM_results %>%
  mutate(tot_ = 1, urban = (zona == 1)) %>%
  rename(gr_estrato0 = estrato0_pc)

saveRDS(DOM_results, paste0(fdbmod, "DOM_simulaciones"))

# --- Ejecutar todos los escenarios --------------------------------------------
variable_values <- escenarios$escenario

# --- Índices de Kakwani (calculados una sola vez para todos los escenarios) ---
kakwani_data <- estima_kakwani(DOM_results, escenarios_nums = variable_values)

resultados_escenarios <- sapply(
  setNames(variable_values, paste0("escenario_", variable_values)),
  function(esc_num) {
    sum_escenarios(esc_num, DOM_results, pov_gral, pov_ext, pov_int,
                   ineq, kakwani_data, variable_values,
                   macro$pib, f_isr, f_itb, f_sub)
  },
  simplify = FALSE
)

# --- Aplicar etiquetas --------------------------------------------------------
resultados_escenarios <- lapply(resultados_escenarios, aplicar_labels)

# --- Guardar resultados -------------------------------------------------------
saveRDS(DOM_results,            paste0(fdbmod, "DOM_resultados.rds"))
saveRDS(resultados_escenarios,  paste0(fdbmod, "DOM_resumen.rds"))

# --- Limpiar memoria ----------------------------------------------------------
rm(DOM_results, esc_inc, estratos, macro, vs_isr, vs_itb, vs_sub,
   f_isr, f_itb, f_sub, ineq, pov_ext, pov_gral, pov_int,
   resultados_escenarios, GRUPOS_ANALISIS, LABELS_CONFIG, 
   subratios, variable_values, kakwani_data)
rm(decile_pib, preparar_deciles_una_vez, estima_kakwani)
