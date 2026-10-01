## Banco Mundial
## Herramienta de Microsimulación
## Country: Dominican Republic, 2026
## Authors: Maynor Cabrera, Renato Vargas
## Script: Maynor Cabrera
## 03_DOM26_subsidios.R
## E-email:       mynorvc@gmail.com
##  Dependencias:  00_DOM26_Master.R
##
## Input files
## -- DOM_parametros_simulaciones.xlsx
## -- ENGHI 20180
## ---- DOMCEQ_DIncome.rds
## ---- elec.rds
## ---- DOM26_gtotal.rds
## ---- DOM26_gtotals.rds
## -- ENCFT 2024
## ---- DOM26_SDIncome.dta
##
## Output files:
## -- DOM_simsubsidio.rds
##
##
## Fecha de creación:         5 Mar 2026 - 15:02
## Version:    	  			      2.1  -- CORREGIDO: bug de pmin(na.rm=TRUE) en
##                                        imputación de missing vs cero legítimo
## Fecha de modificación:     2026-07-10

# 0. Importar archivos y definir funciones -------------------------------------

## 0.1 Importar archivos -------------------------------------------------------

### 0.1.1 Subsidio eléctrico  --------------------------------------------------

sim_sub_esc <- sort(unique(c(0, escenarios$sim_sub)))

sim_sub <- read_excel(fparams, col_names = TRUE, sheet = "sim_sub",
                      range = "A1:BF11") %>%
  janitor::clean_names() %>%
  filter(activo == 1 | sim_sub == 0) %>%
  filter(sim_sub %in% sim_sub_esc)

# Prefijos y nombres de cada matriz
matrices_sim <- list(
  basesur  = "bsur",
  block    = "block",
  basenor  = "bnorte",
  baseest  = "beste",
  tarifest = "teste",
  tarifsur = "tsur",
  tarifnor = "tnorte"
)

matric_sim <- list(
  oteste   = "oteste",
  otsur    = "otsur",
  otnorte  = "otnorte"
)

for (nombre in names(matrices_sim)) {
  assign(nombre, sim_sub %>%
           select(starts_with(matrices_sim[[nombre]])) %>%
           select(where(~ !all(is.na(.)))) %>%
           as.matrix()
  )
}

for (nombre in names(matric_sim)) {
  assign(nombre, sim_sub %>%
           select(starts_with(matric_sim[[nombre]])) %>%
           select(where(~ !all(is.na(.)))) %>%
           as.matrix()
  )
}

elec <- readRDS(paste0(fpresim, "elec.RDS"))

### 0.1.2 Subsidio combustibles ------------------------------------------------

# obtener escenarios activos 
sim_sgas_esc <- c(0, sort(unique(escenarios$sim_sgas)))

# parámetros de escenarios activos 
sim_sgas <- read_excel(fparams, col_names = TRUE, sheet = "sim_sgas",
                       range = "A1:GA11") %>%
  janitor::clean_names() %>%
  filter(activo == 1 | sim_sgas == 0) %>%
  filter(sim_sgas %in% sim_sgas_esc)

# Función auxiliar para extraer matrices limpias
extraer_matriz <- function(df, prefijo, excluir = NULL, 
                           solo_primera_fila = FALSE) {
  patron <- paste0("^", prefijo, "(_|\\d|$)")
  res <- df %>% 
    select(matches(patron)) %>% 
    select(where(~ !all(is.na(.))))
  
  if (!is.null(excluir)) res <- res %>% select(!contains(excluir))
  if (solo_primera_fila) res <- head(res, 1)
  
  as.matrix(res)
}

## Generar matrices simgas base (estacionalidad del consumo)
mats_simgas_prefix <- c(
  gasoil_s           = "gasoil",
  gasolina_regular_s = "gasolina_regular",
  gasolina_premium_s = "gasolina_premium",
  glp_s              = "glp"
)

mats_simgas <- map(mats_simgas_prefix, ~ 
                     extraer_matriz(sim_sgas, .x, c("precio", "_sub_"), 
                                    solo_primera_fila = TRUE))

## Generar matrices de precios y subsidios por escenario
gases <- c("gasoil_premium", "gasoil_regular", "gasolina_premium", 
           "gasolina_regular", "glp")

mats_por_nivel <- expand.grid(gas = gases, lev = sim_sgas_esc, 
                              stringsAsFactors = FALSE) %>%
  pmap(function(gas, lev) {
    df_escenario <- sim_sgas %>% filter(sim_sgas == lev)
    a1 <- extraer_matriz(df_escenario, paste0(gas, "_precio"))
    a2 <- extraer_matriz(df_escenario, paste0(gas, "_sub"))
    list(nombre = paste0(gas, "_p_", lev), matriz = rbind(a1, a2))
  }) %>%
  { set_names(map(., "matriz"), map_chr(., "nombre")) }

## Generar matrices de variables mensuales (m_0, m_1, etc.) por escenario
mats_m <- map(sim_sgas_esc, function(lev) {
  sim_sgas %>%
    filter(sim_sgas == lev) %>%
    select(all_of(paste0("m", 1:12))) %>%
    as.matrix()
}) %>% set_names(paste0("m_", sim_sgas_esc))

stopifnot(all(paste0("m_", sim_sgas_esc) %in% names(mats_m)))

combustibles <- readRDS(paste0(fpresim, "combustibles.RDS"))

### 0.1.3 Otros --------------------------------------------------

## Ingreso disponible 2024
base2024 <- readRDS(paste0(fpresim, "DOM26_SDIncome.RDS")) %>%
  select(hhid, hhin, trimestre, factor_expansion_anual, yd_pc, zona, 
         tipo_alumbrado) %>%
  arrange(hhid, hhin)

## Ingreso disponible CEQ 2023
dispinc2018 <- readRDS(paste0(finput, "DOMCEQ_DIncome.RDS")) %>%
  select(hhid, yd_pc, ym_pc, relation, weight, urban, pondera_2023) %>%
  arrange(hhid)

mip_data  <- readRDS(paste0(fpresim, "mip2013_rd.RDS"))

itbis_base <- read_excel(fparams, col_names = TRUE, sheet = "itbis_base") %>%
  janitor::clean_names() %>%
  select(-cod_arancelario)

gastos_presim <-  readRDS(paste0(fpresim, "DOM26_gtotals.RDS")) %>%
  select(hhid, id_variedad, monto_pagado_hogar_mensual, 
         factor_expansion, monto_total_mensual, id_tipo_establecimiento) %>%
  arrange(hhid, id_variedad)

run_esc1 = 0

## 0.2 FUNCIONES ---------------------------------------------------------------

percap <- function(data, varlist) {
  for (x in varlist) {
    sufijo <- substr(x, nchar(x) - 1, nchar(x))
    if (sufijo == "in") {
      col_hh <- sub("in$", "hh", x)
      col_pc <- sub("in$", "pc", x)
      data <- data %>%
        group_by(hhid) %>%
        mutate(!!col_hh := sum(.data[[x]], na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(!!col_pc := .data[[col_hh]] / hsize)
    }
    if (sufijo == "hh") {
      col_pc <- sub("hh$", "pc", x)
      data <- data %>%
        mutate(!!col_pc := .data[[x]] / hsize)
    }
  }
  return(data)
}

# ── Función auxiliar: pago por bloques tarifarios ────────────────────────────
calcular_pago_bloques <- function(consumo, base, tarif, ot, acum, blck, tt, to) {
  consumo_r <- round_stata(consumo, 0)
  valido    <- !is.na(consumo)
  pago      <- rep(0, length(consumo))
  
  # Bloques 1 y 2 (consumo <= 200): tarifa subsidiada ot
  m1 <- valido & consumo_r <= blck[2] & consumo > 0
  pago[m1] <- base[1] + ot[1] * consumo[m1]
  
  m2 <- valido & consumo_r > blck[2] & consumo_r <= blck[3]
  pago[m2] <- base[2] + ot[1] * blck[2] + ot[1] * (consumo[m2] - blck[2])
  
  # Bloques 3..tt: tarifa normal, acum ya incorpora tarif[1] y tarif[2]
  for (ij in 3:tt) {
    m <- valido & consumo_r > blck[ij] & consumo_r <= blck[ij + 1]
    pago[m] <- acum[ij - 1] + (consumo[m] - blck[ij]) * tarif[ij]
  }
  
  # Bloque final
  mf <- valido & consumo_r > blck[to] & !is.na(consumo_r)
  pago[mf] <- acum[tt] + (consumo[mf] - blck[to]) * tarif[to]
  
  return(pago)
}

#' Función para convertir todas los ratios a valores suavizados usando
#' regresión local polinómica
#'
#' @param data      data frame  con datos
#' @param group_col grupo (urbano)
#' @param xcol      valores x para el suavizamiento
#' @param degree    grado del polinomio local utilizado
#' @param gridsize  número de puntos de cuadrícula para estimar la función
#'
#'
suav_subs_ur <- function(data,
                         group_col = "urban",
                         xcol = "centil",
                         degree = 0,
                         gridsize = 100) {
  
  # Verificar columna de agrupación
  if (!group_col %in% names(data)) {
    stop(paste("La columna", group_col, "no existe en el data frame"))
  }
  
  # Identificar columnas rsub
  rsub_cols <- grep("^rsub", names(data), value = TRUE)
  
  if (length(rsub_cols) == 0) {
    return("No se encontraron columnas que comiencen con 'ritx' en dataframe")
  }
  
  # Procesar por grupo
  resultado <- data %>%
    group_by(across(all_of(group_col))) %>%
    group_modify(~ {
      
      # CORRECCIÓN: usar .y en lugar de cur_group()
      grupo_actual <- .y[[1]]  # .y contiene los valores de las variables de agrupación
      message(paste("\n=== Procesando grupo:", grupo_actual, "==="))
      
      data_grupo <- .x
      
      for (col in rsub_cols) {
        new_col <- gsub("^rsub", "psub", col)
        
        x_values <- data_grupo[[xcol]]
        y_values <- data_grupo[[col]]
        
        valid_indices <- which(!is.na(x_values) & !is.na(y_values))
        x_valid <- x_values[valid_indices]
        y_valid <- y_values[valid_indices]
        
        if (length(valid_indices) == 0) {
          warning(paste("Grupo", grupo_actual, "- Sin datos válidos en", col))
          next
        }
        
        bw <- try(dpill(x_valid, y_valid), silent = TRUE)
        
        if (inherits(bw, "try-error") || is.na(bw) || bw <= 0) {
          bw <- 0.2 * diff(range(x_valid))
        }
        
        ajuste <- try(locpoly(
          x = x_valid,
          y = y_valid,
          degree = degree,
          bandwidth = bw,
          kernel = "epanech",
          gridsize = gridsize,
          range.x = c(1, 100)  # forzar rango exacto 1-100 como Stata
        ), silent = TRUE)
        
        if (inherits(ajuste, "try-error")) {
          warning(paste("Grupo", grupo_actual, "- Error en regresión para", col))
          next
        }
        
        predictions <- rep(NA, nrow(data_grupo))
        valid_predictions <- approx(
          x = ajuste$x,
          y = ajuste$y,
          xout = x_valid,
          rule = 2
        )$y
        
        predictions[valid_indices] <- valid_predictions
        data_grupo[[new_col]] <- predictions
        
        message(paste("Grupo", grupo_actual, "- Procesado:", col))
      }
      
      return(data_grupo)
    }) %>%
    ungroup()
  
  return(resultado)
}

# ── Helpers de diagnóstico "seguros" ──────────────────────────────────────
# Se ejecutan solo si SE CUMPLEN LAS DOS CONDICIONES:
#   1) run == 1 (interruptor manual)
#   2) la columna existe en el data frame
# Si falla cualquiera de las dos, se omite (sin error).

safe_sum <- function(run = 1, df, col, w_col = NULL, mask = NULL) {
  existe <- col %in% names(df)
  ejecutar <- isTRUE(as.logical(run)) && existe
  
  if (!ejecutar) {
    motivo <- if (!isTRUE(as.logical(run))) "run = 0" else "columna no existe"
    cat(sprintf("  [omitido] %s (%s)\n", col, motivo))
    return(invisible(NA))
  }
  
  # Si mask es texto, evaluarlo como expresión lógica dentro de df
  if (is.character(mask)) {
    mask <- eval(parse(text = mask), envir = df)
  }
  
  x <- df[[col]]
  w <- if (!is.null(w_col)) df[[w_col]] else NULL
  if (!is.null(mask)) {
    x <- x[mask]
    if (!is.null(w)) w <- w[mask]
  }
  val <- if (is.null(w)) fsum(x, na.rm = TRUE) else fsum(x, w = w, na.rm = TRUE)
  cat(sprintf("  %s: %s\n", col, comma(val)))
  invisible(val)
}

safe_mean <- function(run = 1, df, col, w_col = NULL, mask = NULL) {
  existe <- col %in% names(df)
  ejecutar <- isTRUE(as.logical(run)) && existe
  
  if (!ejecutar) {
    motivo <- if (!isTRUE(as.logical(run))) "run = 0" else "columna no existe"
    cat(sprintf("  [omitido] %s (%s)\n", col, motivo))
    return(invisible(NA))
  }
  
  # Si mask es texto, evaluarlo como expresión lógica dentro de df
  if (is.character(mask)) {
    mask <- eval(parse(text = mask), envir = df)
  }
  
  x <- df[[col]]
  w <- if (!is.null(w_col)) df[[w_col]] else NULL
  if (!is.null(mask)) {
    x <- x[mask]
    if (!is.null(w)) w <- w[mask]
  }
  val <- if (is.null(w)) fmean(x, na.rm = TRUE) else fmean(x, w = w, na.rm = TRUE)
  cat(sprintf("  %s: %s\n", col, comma(val)))
  invisible(val)
}


safe_sum_multi <- function(run = 1, df, cols, w_col = NULL) {
  if (!isTRUE(as.logical(run))) {
    cat(sprintf("  [omitido] %s (run = 0)\n", paste(cols, collapse = ", ")))
    return(invisible(NA))
  }
  
  cols_ok <- cols[cols %in% names(df)]
  faltantes <- setdiff(cols, cols_ok)
  if (length(faltantes) > 0) {
    cat(sprintf("  [omitido] %s no existe(n)\n", paste(faltantes, collapse = ", ")))
  }
  if (length(cols_ok) == 0) return(invisible(NA))
  
  val <- if (is.null(w_col)) fsum(df[cols_ok]) else fsum(df[cols_ok], w = df[[w_col]])
  print(val)
  invisible(val)
}

safe_summary <- function(run = 1, df, col, mask = NULL) {
  existe <- col %in% names(df)
  ejecutar <- isTRUE(as.logical(run)) && existe
  
  if (!ejecutar) {
    motivo <- if (!isTRUE(as.logical(run))) "run = 0" else "columna no existe"
    cat(sprintf("  [omitido] %s (%s)\n", col, motivo))
    return(invisible(NULL))
  }
  
  if (is.character(mask)) {
    mask <- eval(parse(text = mask), envir = df)
  }
  
  x <- if (is.null(mask)) df[[col]] else df[[col]][mask]
  print(format(summary(x), scientific = FALSE, digits = 3))
}

# 1. Estimación de escenarios electricidad ------------------------------------

# Precalcular fuera del loop los valores que no cambian entre
# escenarios (b2, t2, n_bloques, tt, to), y precalcular sin_contpub una sola vez.
n_bloques <- ncol(tarifnor)
tt        <- n_bloques - 1
to        <- n_bloques

# Tarifas y bases promedio Edesur/Edeeste (escenario base "0") — fijas
b2 <- (basesur  + baseest) / 2
t2 <- (tarifsur + tarifest) / 2
otse <- (otsur  + oteste) / 2

# Condición sin contador: se calcula una sola vez
elec <- elec %>%
  mutate(sin_contpub = sub_sincontador == 1 & a219 == 1)

# Máscaras de distribuidora — se calculan una sola vez
mask_norte <- elec$a219 == 1 &  elec$estrato %in% c(21, 22)
mask_sures <- elec$a219 == 1 & !elec$estrato %in% c(21, 22)

for (s in sim_sub_esc) {
  cat(sprintf("\n========== Escenario %d ==========\n", s))
  st <- s + 1
  
  ## 1.1. Gasto acumulado máximo según bloque-----------------------------------
  
  # Acumulado Edenorte
  acunor    <- numeric(tt)
  acunor[1] <- basenor[st, 2] + block[st, 2] * tarifnor[st, 1] 
  for (ij in 2:(tt)) {
    acunor[ij] <- acunor[ij - 1] + (block[st, ij + 1] - block[st, ij ]) * 
      tarifnor[st, ij]
  }
  
  
  # Acumulado Edesur/Edeeste
  acuses    <- numeric(tt)
  acuses[1] <- b2[st, 2] +  block[st, 2] * t2[st,1]
  for (ij in 2:(tt)) {
    acuses[ij] <- acuses[ij - 1] + (block[st, ij + 1 ] - block[st, ij ]) * 
      t2[st, ij ]
  }
  
  assign(paste0("acunor_", s), acunor)
  assign(paste0("acuses_", s), acuses)
  
  
  ## 1.2. Estimar pago conforme a tarifas---------------------------------------
  
  pago_col <- paste0("pago_e_", s)
  
  pago_vals <- rep(0, nrow(elec))
  pago_vals[mask_norte] <- calcular_pago_bloques(
    elec$consumo_e[mask_norte], basenor[st, ], tarifnor[st, ], otnorte[st, ],
    acunor, block[st, ], tt, to
  )
  pago_vals[mask_sures] <- calcular_pago_bloques(
    elec$consumo_e[mask_sures], b2[st, ], t2[st, ], otse[st, ], 
    acuses, block[st,], tt, to
  )
  elec[[pago_col]] <- pago_vals
  
  ##
  # 1.3. Precio medio-----------------------------------------------------------
  ##
  precio_col <- paste0("precio_me_", s)
  elec[[precio_col]] <- elec[[pago_col]] / elec$consumo_e
  
  ## 1.4. Costos----------------------------------------------------------------
  
  costo_col  <- paste0("costo_ekwh_", s)
  cnorte_val <- as.numeric(sim_sub$costnorte[sim_sub$sim_sub == s])
  csur_val   <- as.numeric(sim_sub$costsur[sim_sub$sim_sub == s]) 
  ceste_val  <- as.numeric(sim_sub$costeste[sim_sub$sim_sub == s]) 
  costo_sures <- (csur_val + ceste_val) / 2
  
  elec[[costo_col]] <- ifelse(elec$estrato %in% c(21, 22), cnorte_val, costo_sures)
  
  
  
  ## 1.5. Subsidio para quienes reportaron pagos--------------------------------
  
  sub_col  <- paste0("subsidio_e_",  s)
  subt_col <- paste0("subsidio_et_", s)
  
  
  diff1 <- elec[[costo_col]] - elec[[precio_col]]
  elec[[sub_col]] <- ifelse(is.na(diff1), NA_real_, pmax(0, diff1))
  elec[[subt_col]] <-  pmax(0, elec$consumo_e * elec[[costo_col]] - 
                              elec[[pago_col]], na.rm = TRUE)
  
  ## consumo por decil
  elec_filt      <- fsubset(elec, a219 == 1 & sub_sincontador != 1 & consumo_e > 0)
  elec_filt$sub_temp <- elec_filt[[sub_col]]
  elec_stats <- elec_filt %>%
    fgroup_by(decyd) %>%
    fsummarise(
      mean_consumo  = fmean(consumo_e, w = weight),
      mean_subsidio = fmean(sub_temp,  w = weight)
    )
  
  # usar match() en vez de left_join + select dentro del loop
  idx_decil      <- match(elec$decyd, elec_stats$decyd)
  mean_consumo_v <- elec_stats$mean_consumo[idx_decil]
  mean_sub_v     <- elec_stats$mean_subsidio[idx_decil]
  
    ### 5b. Subsidio para quienes no reportaron pagos (sin contador)--------------
  
  subs1_col  <- paste0("subs1_",  s)
  subs2_col  <- paste0("subs2_",  s)
  subsa2_col <- paste0("subsa2_", s)
  
  elec[[subs1_col]]  <- ifelse(!elec$sin_contpub, elec[[subt_col]], 0)
  elec[[subs2_col]]  <- ifelse( elec$sin_contpub,
                                mean_consumo_v * elec[[costo_col]], 0)
  elec[[subsa2_col]] <- ifelse( elec$sin_contpub,
                                mean_consumo_v * mean_sub_v, 0)
    
  ## 1.6. Pago estimado para quienes no reportaron pago pero sí consumo---------
  
  pa4o_col  <- paste0("pa4o_e_", s)
  consumo2  <- ifelse(elec$sin_contpub, mean_consumo_v, elec$consumo_e)
  
  pa4o_vals <- elec[[pago_col]]
  pa4o_vals[mask_norte] <- calcular_pago_bloques(
    consumo2[mask_norte], basenor[st, ], tarifnor[st, ], otnorte[st, ],
    acunor, block[st, ],  tt, to
  )
  pa4o_vals[mask_sures] <- calcular_pago_bloques(
    consumo2[mask_sures], b2[st, ], t2[st, ], otse[st, ], 
    acuses, block[st, ], tt, to
  )
  
  elec[[pa4o_col]] <- pa4o_vals
  
}

rm(list = c("acunor_0", "acunor_1", "acuses", "acuses_0", "acuses_1", "b2",
            "ceste_val", "cnorte_val", "csur_val", "ij",     
            "mean_eff", "n_bloques", "nombre", 
            "perc99", "s", "st", "t2", "to", "tt", "x", "z"))

# 2. Guardar datos subsidio electrico  simulados ------------------------------

cols_keep <- c("hhid", "miembro", "FPOBRE6N4", "weight", "consumo_e",
               "icv_met", "sub_sincontador", "hsize", "a219", "electricos",
               "estrato",
               grep("^subs[12]_|^subsa|^pago_e_|^pa4o_e_",
                    names(elec), value = TRUE))

subs1_cols <- grep("^subs1_",  cols_keep, value = TRUE)
subs2_cols <- grep("^subs2_",  cols_keep, value = TRUE)
max_vars   <- grep("^pago_e_|^subsa|^pa4o_e_", cols_keep, value = TRUE)

# CORRECCIÓN: eliminar fsubset(!is.na(consumo_e)) — Stata no filtra por consumo_e
# en el keep, incluye todos los hogares igual que Stata
elec_sub <- elec[, cols_keep]

for (col in subs1_cols) {
  elec_sub[[paste0("sub_ele1_hh_", col)]] <- fmax(elec_sub[[col]],
                                                  g   = elec_sub$hhid,
                                                  TRA = "replace")
}
for (col in subs2_cols) {
  elec_sub[[paste0("sub_ele2_hh_", col)]] <- fmax(elec_sub[[col]],
                                                  g   = elec_sub$hhid,
                                                  TRA = "replace")
}

sub_ele_cols <- grep("^sub_ele", names(elec_sub), value = TRUE)

subelec <- elec_sub %>%
  fgroup_by(hhid) %>%
  fsummarise(
    across(all_of(sub_ele_cols), fmax),
    across(all_of(max_vars),     fmax),
    consumo_e       = fmax(consumo_e),
    sub_sincontador = fmax(sub_sincontador),
    estrato         = fmax(estrato),
    electricos      = fmax(electricos),
    a219            = fmax(a219),
    hsize           = fmean(hsize)
  )

rm(elec_sub)

rename_map <- setNames(
  c(paste0("sub_ele1_hh_subs1_", sim_sub_esc),
    paste0("sub_ele2_hh_subs2_", sim_sub_esc),
    paste0("subsa2_",            sim_sub_esc)),
  c(paste0("sub_ela", sim_sub_esc, "_hh"),
    paste0("sub_elb", sim_sub_esc, "_hh"),
    paste0("sub_elc", sim_sub_esc, "_hh"))
)
subelec <- rename(subelec, all_of(rename_map))

subelec <- percap(subelec, grep("sub_el.*_hh$", names(subelec), value = TRUE))

extra_cols <- lapply(sim_sub_esc, function(s) {
  tibble(
    !!paste0("sub_ele", s, "_pc") :=
      subelec[[paste0("sub_ela", s, "_pc")]] +
      subelec[[paste0("sub_elb", s, "_pc")]],
    !!paste0("sub_ele", s, "_hh") :=
      subelec[[paste0("sub_ela", s, "_hh")]] +
      subelec[[paste0("sub_elb", s, "_hh")]]
  )
}) %>% bind_cols()

subelec <- bind_cols(subelec, extra_cols)

# Verificación
cat("N subelec:         ", nrow(subelec), "\n")           # debe dar 8,892
cat("Sum sub_ele0_pc:  ",  format(round(sum(subelec$sub_ele0_pc,
                                            na.rm=TRUE), 0), big.mark=","),"\n")
safe_sum(run_esc1, subelec, "sub_ele1_pc")
safe_sum(run_esc1, subelec, "sub_ele2_pc")

cat("Mean sub_ele0_pc:  ", format(round(mean(subelec$sub_ele0_pc,
                                             na.rm=TRUE), 2), big.mark=","), "\n")
safe_mean(run_esc1, subelec, "sub_ele1_pc")
safe_mean(run_esc1, subelec, "sub_ele2_pc")
                                             
#cat("Mean sub_ele2_pc:  ", format(round(mean(subelec$sub_ele2_pc, 
#  na.rm=TRUE), 2), big.mark=","), "\n")

## DIAGNÓSTICO NUEVO: conteo de ceros vs missing por escenario
# Corre esto y revisa si el escenario 2 tiene muchos más "ceros" que 0/1.
# Esto confirma si el sesgo hacia arriba viene de una mayor focalización
# (más ceros legítimos) mal tratada como missing más adelante.
for (s in sim_sub_esc) {
  col <- paste0("sub_ele", s, "_hh")
  cat(sprintf("Escenario %d: N=%d, N ceros=%d (%.1f%%), N NA=%d\n",
              s, sum(!is.na(subelec[[col]])),
              sum(subelec[[col]] == 0, na.rm = TRUE),
              100 * mean(subelec[[col]] == 0, na.rm = TRUE),
              sum(is.na(subelec[[col]]))))
}

# 3. Estimar efectos directos subsidio combustibles ---------------------------


for (s in sim_sgas_esc) {
  cat("Escenario combustibles:", s, "\n")
  
  # Factores de precio (ratio nivel2/nivel1) y estacionalidad mensual: 
  # estos NO dependen de la fila del hogar, son vectores de largo 12
  precios <- list(
    gapr = mats_por_nivel[[paste0("gasolina_premium_p_", s)]],
    gare = mats_por_nivel[[paste0("gasolina_regular_p_", s)]],
    gopr = mats_por_nivel[[paste0("gasoil_premium_p_",   s)]],
    gore = mats_por_nivel[[paste0("gasoil_regular_p_",   s)]],
    glpe = mats_por_nivel[[paste0("glp_p_",              s)]]
  )
  
  col_base <- c(
    gapr = "gasolinapr_", gare = "gasolinare_",
    gopr = "gasoilpr_",   gore = "gasoilre_",   glpe = "glp_"
  )
  
  m_vec <- as.numeric(mats_m[[paste0("m_", s)]])  # 12 pesos mensuales
  
  # Para cada combustible: construir las 12 columnas mensuales + la suma,
  # todo en memoria (listas), sin tocar `combustibles` hasta el final
  nuevas_cols <- imap(col_base, function(prefijo_col, comb) {
    factor_precio <- precios[[comb]][2, ] / precios[[comb]][1, ]  # largo 12
    factor_total  <- factor_precio * m_vec                         # largo 12
    
    cols_mes <- map(1:12, function(i) {
      combustibles[[paste0(prefijo_col, i)]] * factor_total[i]
    })
    names(cols_mes) <- paste0("sub_", comb, s, "_hh_", 1:12)
    
    # Suma de los 12 meses -> columna de resumen del combustible
    resumen <- reduce(cols_mes, ~ coalesce(.x, 0) + coalesce(.y, 0))
    cols_mes[[paste0("sub_", comb, s, "_hh")]] <- resumen
    
    cols_mes
  }) %>% 
  unname() %>% 
  flatten()
  
  # Una sola reasignación de `combustibles` por escenario, no 13
  combustibles <- bind_cols(combustibles, as_tibble(nuevas_cols))
}

# Verificación de totales y ratios

# Suma ponderada de consumo regular original
sum_reg <- combustibles %>%
  summarise(across(starts_with("gasolinare_"), 
                   ~ sum(.x * factor_expansion, na.rm = TRUE))) %>%
  rowSums() 

# Suma ponderada de subsidios estimados en gasolina regular
sum_reg2 <- combustibles %>%
  summarise(across(matches("^sub_gare.*_hh$"), 
                   ~ sum(.x * factor_expansion, na.rm = TRUE))) %>%
  rowSums()

ratio_reg <- (sum_reg2 / sum_reg)*100

cat("Suma Regular Original:", comma(sum_reg), "\n")
cat("Suma Subsidio Regular:", comma(sum_reg2), "\n")
cat("Ratio Subsidio/Consumo:", ratio_reg, "\n")

rm(sum_reg, sum_reg2)

# combdir <- combustibles %>%
#    select(hhid, starts_with("sub") & ends_with("hh")) 

combdir <- combustibles %>%
  select(hhid, starts_with("sub") & ends_with("hh")) %>%
  group_by(hhid) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop")


# Para cada sufijo, crear una columna con la suma de esas variables
for (s in sim_sgas_esc) {
  patron <- paste0("^sub.*", s, "_hh$")
  combdir <- combdir %>%
    mutate(!!paste0("sub_cmd", s,"_hh") := rowSums(across(matches(patron)), 
                                                   na.rm = TRUE))
}

combdir <- combdir %>%
  select(-matches("gapr|gore|glpe|gopr|gare"))

safe_sum(run_esc1, combdir, "sub_cmd1_hh")
## [1] "1,119,257

## combustibles guarda datos de subsidios directos 

# 4. Estimar efectos indirectos subsidio combustibles -------------------------

###
# 1. Parámetros / shares fijos 
### 
A <- mip_data %>%
  select(-mipcat) %>%
  as.matrix()

share_fuel <- 0.31 + 0.28 + 0.12   # gasolina + gasoil + glp (COU 2018-2020)

sh_gasregular    <- 0.131
sh_gaspremium    <- 0.339
sh_gasoilregular <- 0.198
sh_gasoilpremium <- 0.137
sh_glp           <- 0.195

###
# 2. Efectos indirectos de precios por escenario
#    (equivalente al primer "foreach sim of global simsgas" en Mata)
###

d_resultados <- list()

for (s in sim_sgas_esc) {
  print(s)
  
  # Vector "mes" con los pesos mensuales m1_s ... m12_s
  mes <- as.numeric(mats_m[[paste0("m_", s)]])   # largo 12
  
  gsre <- mats_por_nivel[[paste0("gasolina_regular_p_", s)]]
  gspr <- mats_por_nivel[[paste0("gasolina_premium_p_", s)]]
  gore <- mats_por_nivel[[paste0("gasoil_regular_p_",   s)]]
  gopr <- mats_por_nivel[[paste0("gasoil_premium_p_",   s)]]
  glp  <- mats_por_nivel[[paste0("glp_p_",   s)]]
  
  res_gsre <- sum((gsre[2, ] / gsre[1, ]) * mats_simgas$gasolina_regular_s * mes)
  res_gspr <- sum((gspr[2, ] / gspr[1, ]) * mats_simgas$gasolina_premium_s * mes)
  res_gore <- sum((gore[2, ] / gore[1, ]) * mats_simgas$gasoil_s * mes)
  res_gopr <- sum((gopr[2, ] / gopr[1, ]) * mats_simgas$gasoil_s * mes)
  res_glp  <- sum(( glp[2, ] /  glp[1, ]) * mats_simgas$glp_s    * mes)
  
  d_resultados[[as.character(s)]] <- list(
    d_gsre = res_gsre,
    d_gspr = res_gspr,
    d_gore = res_gore,
    d_gopr = res_gopr,
    d_glp  = res_glp
  )
}

###
# 3. Tasa efectiva de subsidio por escenario
#    (equivalente al segundo "foreach sim of global simsgas")
###

sgas_rate <- sapply(sim_sgas_esc, function(s) {
  d <- d_resultados[[as.character(s)]]
  print(d)
  (
    d$d_gsre * sh_gasregular +
      d$d_gspr * sh_gaspremium +
      d$d_gore * sh_gasoilregular +
      d$d_gopr * sh_gasoilpremium +
      d$d_glp  * sh_glp              
  ) * share_fuel
})
names(sgas_rate) <- as.character(sim_sgas_esc)

### 
# 4. Modelo cost-push: shock de precios y pass-through
#    (equivalente al bloque dp_sim / gamma / alpha / V / deltaptilda / deltap)
### 

Nsectors <- nrow(A)

fix_price        <- 21   # sector con precio fijo (admón pública)
price_shock_fuel <- 7    # sector combustibles

resultados_precios <- list()

gamma <- matrix(0, nrow = Nsectors, ncol = Nsectors)
gamma[fix_price, fix_price] <- 1
alpha <- diag(Nsectors) - gamma
V     <- solve(diag(Nsectors) - alpha %*% A)   # se calcula una vez


for (s in sim_sgas_esc) {
  s_chr <- as.character(s)
  cat("Sectores gravados en", s, ":", fix_price, "\n")
  
  # Paso 1: vector de shock de precios
  dp_sim <- matrix(0, nrow = 1, ncol = Nsectors)
  dp_sim[1, price_shock_fuel] <- sgas_rate[[s_chr]]
  
  # Paso 2: matriz gamma (sectores con precio fijo)
  #gamma <- matrix(0, nrow = Nsectors, ncol = Nsectors)
  #gamma[fix_price, fix_price] <- 1
  
  # Paso 3: pass-through de precios (cost-push model)
  #alpha <- diag(Nsectors) - gamma
  # V     <- solve(diag(Nsectors) - alpha %*% A)
  
  deltaptilda <- dp_sim %*% A %*% V
  deltap      <- (dp_sim %*% gamma) + (deltaptilda + dp_sim) %*% alpha
  
  resultados_precios[[s_chr]] <- list(
    dp_sim      = dp_sim,
    gamma       = gamma,
    alpha       = alpha,
    V           = V,
    deltaptilda = deltaptilda,
    deltap      = deltap
  )
  
  cat("\n========================================\n")
  cat("RESULTADOS PARA ESCENARIO:", s, "\n")
  cat("========================================\n")
  cat("\nShock de precios simulado:\n");            print(dp_sim)
  cat("\nSectores con precio fijo (gamma):\n");      print(gamma)
  cat("\nEfectos indirectos (precio embebido):\n");  print(deltaptilda)
  cat("\nEfecto total sobre precios:\n");            print(deltap)
}

###
# 5. Tabla larga: efecto de precio por sector y escenario
#    (equivalente a "foreach sector of numlist 1/Nsectors: gen ptot... pind...")
###

efectos_precio <- map_dfr(sim_sgas_esc, function(s) {
  s_chr <- as.character(s)
  tibble(
    sim        = s,
    codigo_mip = 1:Nsectors,
    ptot       = as.numeric(resultados_precios[[s_chr]]$deltap),
    pind       = as.numeric(resultados_precios[[s_chr]]$deltaptilda)
  )
})

efectos_precio <- efectos_precio %>%
  pivot_wider(names_from = sim, values_from = c(ptot, pind))

### 
# 6. Merge con tasas de IVA por variedad
#    (equivalente al merge con tasas_itx1.dta)
### 

tasas_subgas <- itbis_base %>%
  inner_join(efectos_precio, by = "codigo_mip") %>%
  select(starts_with("cod"), starts_with("id"), starts_with("p"))

###
# 7. Merge con consumo del hogar y estimación de efecto indirecto
#    (equivalente a DOM26_gtotals.dta + merge + gen sub_cmbi`sim'_hh)
###

gasto_total <- readRDS(paste0(fpresim, "DOM26_gtotal.RDS")) %>%
  arrange(hhid)

base_hogar <- gastos_presim %>%
  left_join(tasas_subgas, by = "id_variedad") %>%   
  filter(cod_grupo != 20)                            

# a <- as_tibble(fsum(base_hogar$monto_pagado_hogar_mensual, base_hogar$hhid))
# b <- as_tibble(fsum(gastos_presim$monto_pagado_hogar_mensual, gastos_presim$hhid))

# bind_cols(a, b)  %>%
#  janitor::clean_names()  %>%
#  mutate(dif = value_1 - value_2) %>%
#  arrange(desc(dif))
  
for (s in sim_sgas_esc) {
  base_hogar <- base_hogar %>%
    mutate(!!paste0("sub_cmi", s, "_hh") := round(monto_pagado_hogar_mensual * 
                                             .data[[paste0("pind_", s)]], 2))
}

safe_sum(run_esc1, base_hogar, "sub_cmi1_hh")
safe_sum(run_esc1, base_hogar, "monto_pagado_hogar_mensual")

### 
# 8. Colapsar a nivel hogar x grupo de gasto
### 

subgas_hogar <- base_hogar %>%
  group_by(hhid, cod_grupo) %>%
  summarise(
    across(starts_with("sub_") & ends_with("_hh"), \(x) sum(x, na.rm = TRUE)),
    monto_pagado_hogar_mensual = sum(monto_pagado_hogar_mensual, na.rm = TRUE),
    factor_expansion = mean(factor_expansion, na.rm = TRUE),
    .groups = "drop"
  )

# 
tabla_ponderada <- subgas_hogar %>%
  summarise(across(starts_with("sub_") & ends_with("_hh"),
                   \(x) sum(x * factor_expansion, na.rm = TRUE)))

safe_sum(run_esc1, tabla_ponderada, "sub_cmi1_hh")
## [1] "393,702,089"

# 5. Consolidar efectos directos e indirectos combustibles  -------------------

ponderah <- dispinc2018 %>%  
  filter(!(yd_pc==0 & ym_pc==0)) %>%
  arrange(hhid) %>%
  mutate(pondera_2023 = round_stata(pondera_2023, 0)) %>%
  group_by(hhid) %>%
  summarise(pondera_2023 = mean(pondera_2023))

subgas_hogar <- subgas_hogar %>%
  left_join(ponderah, by = c("hhid")) %>%
  select(-factor_expansion) %>%
  filter(!is.na(pondera_2023)) %>%
  mutate(hweight = round_stata(pondera_2023, 0)) %>%
  select(-pondera_2023) %>%
  mutate(across(matches("^sub.*hh$"), ~ round_stata(.x, 2)))


cols <- grep("^sub_cmi.*_hh$", names(subgas_hogar), value = TRUE)
comma(fsum(subgas_hogar[, cols ], w = subgas_hogar$hweight))
comma(fsum(subgas_hogar[c("monto_pagado_hogar_mensual")], 
           w = subgas_hogar$hweight))           
##  "96,411,447,674"

comma(fsum(subgas_hogar[c("hweight")]))           
## "32,386,140"

# sub_cmbi0_hh  sub_cmbi1_hh 
# "0" "415,545,126  415,541

# > sum(subgas_hogar$hweight)
# [1] "32,386,140"  32,379,781
# > sum(subgas_hogar$sub_cmi1_hh)
# [1] 960074.3

subgas_hogar <- subgas_hogar %>%
  group_by(hhid) %>%
  summarise(
    across(starts_with("sub_") & ends_with("_hh"), \(x) sum(x, na.rm = TRUE)),
    monto_pagado_hogar_mensual = sum(monto_pagado_hogar_mensual, na.rm = TRUE),
    hweight = mean(hweight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(combdir, by=c("hhid")) 

fsum(subgas_hogar$monto_pagado_hogar_mensual)
# [1] 133 651 512
safe_sum(run_esc1, subgas_hogar, "sub_cmd1_hh")
# [1] 1 119 053
safe_sum(run_esc1, subgas_hogar, "sub_cmi1_hh")
# [1] "960,074"

# 6. Imputar resultados --------------------------------------------------------

gtotal <- readRDS(paste0(fpresim, "DOM26_gtotal.RDS")) %>%
  select(hhid, gasto_total)

cod_gasto <- read_excel(fparams, col_names = TRUE, sheet = "itbis_base") %>%
  janitor::clean_names() %>%
  select(-cod_arancelario) %>%
  select(id_variedad, cod_grupo)

gtotals <- readRDS(paste0(fpresim, "DOM26_gtotals.RDS")) %>%
  select(hhid, id_variedad, monto_total_mensual) %>%
  left_join(cod_gasto, by = "id_variedad") %>%
  filter(cod_grupo != 20) %>%
  group_by(hhid) %>%
  summarise(monto_total = sum(monto_total_mensual, na.rm = TRUE))

fsum(gtotals$monto_total)
# [1] 295479125

base <- readRDS(paste0(finput, "DOMCEQ_DIncome.RDS"))
base <- base %>%
  left_join(subelec, by = "hhid") %>%
  left_join(gtotal,  by = "hhid") %>%
  left_join(gtotals, by = "hhid") %>%
  left_join(subgas_hogar, by = "hhid") %>%
  filter(!(yd_pc == 0 & ym_pc == 0)) %>%
  select(-starts_with("sub_ela"), -starts_with("sub_elb"),
         -starts_with("sub_elc")) %>%
  mutate(across(starts_with("sub"), ~ replace_na(.x, 0)))

fsum(base$monto_total)
## [1] 1 065 514 345
fsum(base$monto_pagado_hogar_mensual)
# [1] 503 665 236
safe_sum(run_esc1, base, "sub_cmd1_hh")
# [1]   4 021 345
safe_sum(run_esc1, base, "sub_cmi1_hh")
# [1] "3,437,859
safe_sum(run_esc1, base, "sub_cmd1_hh", mask = "relation == 1")
# [1] "960,074"

# Replicar exactamente:
base <- base %>%
  mutate(
    pondera_2023 = round_stata(pondera_2023, 0),         
    hweight      = round_stata(pondera_2023 / hsize, 0) 
  )                                                     

# Verificar que hweight coincide con Stata:
cat("Mean hweight R:    ", mean(base$hweight, na.rm=TRUE), "\n")
cat("Mean pondera R:    ", mean(base$pondera_2023, na.rm=TRUE), "\n")
# Stata: summ hweight pondera_2023

sub_hh_vars <- grep("^sub_.*_hh$", names(base), value = TRUE)
mask_jefes  <- base$relation == 1

###
# CORREGIDO: el código original usaba pmin(x, y, na.rm = TRUE) en dos lugares.
# Con na.rm = TRUE, pmin() NO preserva NA cuando uno de los dos vectores tiene
# NA -- en su lugar devuelve el otro valor no-faltante (aquí, el percentil 99).
# Esto convertía todo valor faltante en el percentil 99 (un valor ALTO) en vez
# de dejarlo como NA para luego imputarle el promedio (mean_eff). El bug se
# agravaba en escenarios más focalizados (más hogares con subsidio = 0
# legítimo), inflando el promedio en vez de bajarlo.
#
# La corrección distingue explícitamente:
#   - x_is_na / eff_is_na  -> dato realmente faltante (se imputa mean_eff)
#   - eff_col == 0         -> hogar sin subsidio, valor legítimo (se conserva)
###

for (x in sub_hh_vars) {
  z       <- substr(x, 5, 8)
  pc_col  <- sub("hh$", "pc", x)
  eff_col <- paste0("eff_", z)
  
  # Missing REAL: hogar no encontrado en el merge con subelec (antes del replace_na)
  x_is_na <- is.na(base[[x]])																		
  # replace x = 0 if x == .
  base[[x]] <- replace_na(base[[x]], 0)
  
  # gen double eff_z = x / monto_total
  base[[eff_col]] <- base[[x]] / base$monto_total
  
  
  # eff missing real: sin match original, o monto_total NA/0 (produce NA/NaN)
  eff_is_na <- x_is_na | is.na(base[[eff_col]]) | is.nan(base[[eff_col]])
  
  # qui summ eff_z [w=hweight] if relation==1 & !missing(eff_z) & eff_z!=0, detail
  # local mean_eff = r(mean)
  mask_validos  <- mask_jefes & base[[eff_col]] != 0 & !eff_is_na
  eff_validos   <- base[[eff_col]][mask_validos]
  pesos_validos <- base$hweight[mask_validos]
  mean_eff      <- weighted.mean(eff_validos, pesos_validos, na.rm = TRUE)
  print(mean_eff)
  
  # _pctile eff_z if relation==1 & !missing(eff_z) & eff_z!=0, p(99)
  perc99 <- quantile(eff_validos, probs = 0.99, type = 2, na.rm = TRUE)
  
  # replace eff_z = r(r1) if eff_z > r(r1) & !missing(eff_z)
  mask_gt <- !eff_is_na & base[[eff_col]] > perc99
  base[[eff_col]][mask_gt] <- perc99
  
  # replace eff_z = mean_eff if eff_z == . (SOLO missing real, no cero legítimo)
  base[[eff_col]] <- ifelse(eff_is_na, mean_eff, base[[eff_col]])
  base[[eff_col]] <- if_else(is.na(base[[eff_col]]), 0, base[[eff_col]])
  
  # gen pc = yd_pc * eff_z
  base[[pc_col]] <- base$yd_pc * base[[eff_col]]
  
  # resguardo de consistencia (ya no debería activarse tras el fix de arriba)
  base[[pc_col]] <- ifelse(is.na(base[[pc_col]]),
                           base$yd_pc * mean_eff,
                           base[[pc_col]])
  
  cat(sprintf("%-20s p99=%.6f  mean_eff=%.6f  max=%.6f\n",
              eff_col, perc99, mean_eff,
              max(base[[eff_col]][mask_jefes], na.rm = TRUE)))
}

base %>%
  filter(hhid == 100191) %>%
  select(monto_total, sub_ele0_hh, eff_ele0, sub_ele0_pc) %>%
  mutate(across(everything(), ~ formatC(., format = "f", digits = 7)))

# Verificación — debe coincidir con Stata:
format(summary(base$eff_ele0[mask_jefes]), scientific=FALSE, digits=3)

format(summary(base$eff_ele0[base$relation == 1]), scientific = FALSE,
       digits = 3)
format(summary(base$eff_ele0), scientific = FALSE, digits = 3)

format(summary(base$eff_ele0[base$relation == 1]), scientific = FALSE,
       digits = 3)
format(summary(base$eff_ele1[base$relation == 1 
]), scientific = FALSE,
digits = 3)
# format(summary(base$eff_ele2[base$relation == 1]), scientific = FALSE,
#        digits = 3)

format(summary(base$eff_cmi1[base$relation == 1 
]), scientific = FALSE,
digits = 3)

## 6.1 Ratio sobre ingreso disponible ------------------------------------------

base <- base %>%
  mutate(across(starts_with("sub") & ends_with("pc"),
                ~ .x / yd_pc,
                .names = "r{.col}"))

summary(base[base$relation == 1, ])
safe_sum(run_esc1, base, "sub_cmd1_pc", "relation==1")
safe_sum(run_esc1, base, "sub_cmi1_pc", "relation==1")

format(summary(base$rsub_ele0_pc), scientific = FALSE, digits = 3)

base2 <- base %>%
  rename_with(
    # Selecciona solo las que cumplen el patrón
    .cols = matches("^sub_ele[0-9]+_(hh|pc)$"),  
    .fn   = ~ str_replace_all(., c("_hh$" = "_h2", "_pc$" = "_p2"))
  )


# 7. Estimar ratios  ----------------------------------------------------
base <- base %>%
  mutate(
    decil  = xtile(yd_pc, wt = pondera_2023, n = 10),
    centil = xtile(yd_pc, wt = pondera_2023, n = 100)
  )

weighted.mean(base$rsub_ele0_pc, base$pondera_2023)

base %>%
  group_by(decil) %>%
  summarise(rsub0 = weighted.mean(rsub_ele0_pc, pondera_2023))

# Calcular p99 por variable — en un solo mutate
base <- base %>%
  mutate(across(
    starts_with("rsub_") & ends_with("_pc"),
    ~ {
      q99 <- Hmisc::wtd.quantile(.x, weights = pondera_2023,
                                 
                                 probs = 0.99, na.rm = TRUE)
      ifelse(!is.na(.x) & .x > q99, q99, .x)
    }
  ))
format(summary(base$rsub_ele0_pc), scientific = FALSE, digits = 3)

sub_percentil <- base %>%
  group_by(centil, urban) %>%
  summarise(across(starts_with("rsub_") & ends_with("_pc"),
                   ~ weighted.mean(.x, round_stata(pondera_2023, 0),
                                   na.rm = TRUE)),
            .groups = "drop")

## 7.1 Suavizado locpoly ------------------------------------------------------

subratiose <- base %>%
  filter(electricos != 0 | pago_e_0 != 0) %>%
  group_by(centil, urban) %>%
  summarise(across(starts_with("rsub_ele"),
                   ~ weighted.mean(., w = round_stata(pondera_2023, 0))),
            .groups = "drop")

subratiosg <- base %>%                       # sin el filter()
  group_by(centil, urban) %>%
  summarise(across(starts_with("rsub_c"),
                   ~ weighted.mean(., w = round_stata(pondera_2023, 0))),
            .groups = "drop")

sub_percentil <- suav_subs_ur(
  data = subratiose,
  group_col = "urban",  
  xcol = "centil",
  degree = 0,
  gridsize = 100
)

sub_percentilg <- suav_subs_ur(
  data = subratiosg,
  group_col = "urban",  
  xcol = "centil",
  degree = 0,
  gridsize = 100
)

# Verificar que los ratios ahora coinciden con Stata:
cat("=== Ratios suavizados corregidos ===\n")
sub_percentil %>%
  group_by(urban) %>%
  summarise(
    mean_susub = mean(coalesce(rsub_ele0_pc, psub_ele0_pc), na.rm = TRUE))
## 0.0424 0.0399
# Debe dar: rural ≈ 0.0423, urbano ≈ 0.0399  (valores Stata)

cat("=== Ratios suavizados corregidos escenario 1 ===\n")
sub_percentil %>%
  group_by(urban) %>%
  summarise(
    mean_susub = {
      # pick(everything()) devuelve un tibble con todas las columnas del grupo actual
      if ("rsub_ele1_pc" %in% names(pick(everything()))) {
        mean(coalesce(rsub_ele1_pc, psub_ele1_pc), na.rm = TRUE)
      } else {
        NA_real_
      }
    }
  )

plot(
  sub_percentil$centil[sub_percentil$urban == 1],
  sub_percentil$rsub_ele0_pc[sub_percentil$urban == 1],
  pch  = 18, col  = "lightblue",
  xlab = "Centil", ylab = "Ratio", main = "Urbano"
)
lines(sub_percentil$centil[sub_percentil$urban == 1],
      sub_percentil$psub_ele0_pc[sub_percentil$urban == 1],
      col = "blue", lwd = 2)

# 8. Imputación de subsidios en ENCFT ---------------------------------

base2024 <- base2024 %>%
  mutate(
    centil = xtile(round(yd_pc,0), wt = round(factor_expansion_anual, 0), 
                   n = 100),
    urban  = as.integer(zona == 1)
  ) %>%
  left_join(sub_percentil, by = c("urban", "centil")) %>%
  left_join(sub_percentilg, by = c("urban", "centil"))

base2024 %>%
  group_by(centil) %>%
  summarise(n = n())

# OPTIMIZACIÓN: construir todas las columnas sub_ele*_pc en un solo mutate
sub_cols_2024 <- map_dfc(sim_sub_esc, function(s) {
  # Nombres dinámicos
  ele_name <- paste0("sub_ele", s, "_pc")
  cmi_name <- paste0("sub_cmi", s, "_pc")
  cmd_name <- paste0("sub_cmd", s, "_pc")
  com_name <- paste0("sub_gas", s, "_pc")
  
  # Valores calculados
  cmi_val <- base2024$yd_pc * base2024[[paste0("psub_cmi", s, "_pc")]]
  cmd_val <- base2024$yd_pc * base2024[[paste0("psub_cmd", s, "_pc")]]
  
  tibble(
    !!ele_name := base2024$yd_pc * base2024[[paste0("psub_ele", s, "_pc")]] * 
      (base2024$tipo_alumbrado == 1),
    !!cmi_name := cmi_val,
    !!cmd_name := cmd_val,
    !!com_name := cmi_val + cmd_val
  )
})

base2024 <- bind_cols(base2024, sub_cols_2024)

# Sintaxis correcta de fsum con pesos en collapse:
fsum(base2024$sub_ele0_pc, w = base2024$factor_expansion_anual, na.rm = TRUE)
fsum(base2024$yd_pc, w = base2024$factor_expansion_anual, na.rm = TRUE)/1000


# 6. Guardar resultados  -------------------------------------------------------

base2024 <- base2024 %>%
  select(hhid, hhin, trimestre, starts_with("sub_"), factor_expansion_anual)

safe_sum(run_esc1, base2024, "sub_cmi1_pc")
# [1] "46,225,502 vs 42 995 880

safe_sum(run_esc1, base2024, "sub_cmd1_pc")
# [1] "51,677,194" vs 48 176 634

safe_sum(run_esc1, base2024, "sub_cmi1_pc", 
  w_col = "factor_expansion_anual")
# [1] "8,105,794,515" vs  7,465,820,380
safe_sum(run_esc1, base2024, "sub_cmd1_pc", 
  w_col = "factor_expansion_anual")
# [1] "9,091,513,286" vs  8,394,661,300

base2024 <- base2024 %>%
    select(-factor_expansion_anual)
    
saveRDS(base2024, paste0(fdbmod, "DOM_simsubele.rds"))

rm(list = ls(pattern = "base"))
rm(list = ls(pattern = "elec"))
rm(list = ls(pattern = "tarif"))
rm(list = ls(pattern = "dispinc"))
rm(list = ls(pattern = "block"))
rm(list = ls(pattern = "cod_gasto"))
rm(list = ls(pattern = "extra_cols"))
rm(list = ls(pattern = "gtotal"))
rm(list = ls(pattern = "matrices_sim"))
rm(list = ls(pattern = "smooth_"))
rm(list = ls(pattern = "sub_"))
rm(list = ls(pattern = "tram_"))
rm(list = ls(pattern = "dat_u"))
rm(list = ls(pattern = "col"))
rm(list = ls(pattern = "sh"))

rm(list = c("acunor", "consumo2", "costo_sures", "eff_validos", 
            "idx_decil",
            "mask_jefes", "mask_norte", "mask_sures", "mask_validos", 
            "max_vars", "mean_consumo_v",
            "pa4o_vals", "pago_vals", "pesos_validos", "rename_map", 
            "suav_subs_ur","combdir","A","alpha","combustibles",
            "d_resultados","deltap", "deltaptilda"))
rm(list = c("calcular_pago_bloques"))

rm(dp_sim, efectos_precio, gamma, gasto_total, gastos_presim, glp, gopr,
   gore, gspr, gsre, matric_sim, mip_data, oteste, otnorte, otse, otsur,
   ponderah, precios, resultados_precios, subratiose, subratiosg, 
   tabla_ponderada, tasas_subgas, diff1, eff_is_na, 
   gases, m_vec, mask_gt, mats_simgas_prefix
)

# # Ver qué bandwidth usa R para cada grupo
# for (grp in c(0, 1)) {
#   x <- subratios$centil[subratios$urban == grp]
#   y <- subratios$rsub_ele0_pc[subratios$urban == grp]
#   bw <- dpill(x, y)
#   cat("Urban =", grp, "| BW dpill:", round(bw, 4), "\n")
# }
# 
# for (grp in c(0, 1)) {
#   x <- subratios$centil[subratios$urban == grp]
#   y <- subratios$rsub_cmi1_pc[subratios$urban == grp]
#   bw <- dpill(x, y)
#   cat("Urban =", grp, "| BW dpill:", round(bw, 4), "\n")
# }