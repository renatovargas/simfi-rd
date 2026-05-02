## Banco Mundial
## Herramienta de Microsimulación
## Country: Dominican Republic, 2026
## Authors: Maynor Cabrera, Renato Vargas
## Script: Maynor Cabrera
## 01_DOM26_itbis.R
## E-email:       mynorvc@gmail.com
##  Dependencias:  00_DOM26_Master.R
##
## Input files
## -- data/pipeline/r_params/param_csv/*.csv
## -- mip2013_rd.rds
## -- ENGHI 2018
## ---- DOM26_gtotals.rds
## ---- DOM26_gtotal.rds
## ---- DOMCEQ_DIncome.rds
## ---- urban.rds
## ---- tipo_establecimiento.rds

## -- ENCFT 2024
## ---- DOM26_SDIncome.rds
##
## Output files:
## -- DOM_simitbis.rds
## -- DOM_ingdisponible.rds
##
##
## Fecha de creación:         2 Mar 2026 - 15:02
## Version:    	  			      1.0
## Fecha de modificación:     9 Abr 2026

# 0. Importar archivos y definir funciones -------------------------------------

## 0.1 Importar archivos -------------------------------------------------------

## Estima sim_itbis en escenarios solo en aquellos que están activos
sim_itbis_esc <- sort(unique(escenarios$sim_itbis))

## Simulaciones del ITBIS (parámetros generales) — CSV en fparam_csv
sim_itbis <- read_param_csv("sim_itbis", fparam_csv) %>%
  select(sim_itbis, activo, tasa_itbis, exentos_gravados, uniforma_tasa,
         nom_itbis, base_formal) %>%
  filter(activo == 1) %>%
  filter(sim_itbis %in% sim_itbis_esc) ## filas elegidas vía escenario (UI / escenarios)

## Tasas detalladas para escenarios del itbis
detalle_itbis <- read_param_csv("itbis_sim", fparam_csv)

## Tabla por variedad definida en la app (sustituye sim_itbis + filas del CSV)
if (exists("detalle_itbis_override", inherits = FALSE) &&
    is.data.frame(detalle_itbis_override) && nrow(detalle_itbis_override) > 0) {
  detalle_itbis <- detalle_itbis_override
}

## Base de tasas del itbis
itbis_base <- read_param_csv("itbis_base", fparam_csv) %>%
  select(-cod_arancelario)

## Base de compras según la ENCOVI
gastos_presim <-  readRDS(paste0(fpresim, "DOM26_gtotals.RDS")) %>%
  select(hhid, id_variedad, monto_pagado_hogar_mensual, 
         factor_expansion, monto_total_mensual, id_tipo_establecimiento) %>%
  arrange(hhid, id_variedad)

gasto_total <- readRDS(paste0(fpresim, "DOM26_gtotal.RDS")) %>%
  arrange(hhid)

.path_domceq_dincome <- paste0(finput, "DOMCEQ_DIncome.RDS")
if (!file.exists(.path_domceq_dincome)) {
  .alt_domceq <- paste0(fpresim, "DOMCEQ_DIncome.RDS")
  if (file.exists(.alt_domceq)) .path_domceq_dincome <- .alt_domceq
}
dispinc2018 <- readRDS(.path_domceq_dincome) %>%
  select(hhid, hhin, yd_pc, ym_pc, relation, weight, urban, pondera_2023,
         pline_mod, pline_ext) %>%
  arrange(hhid)

dispinc2024 <- readRDS(paste0(fpresim, "DOM26_SDIncome.RDS")) %>%
  select(hhid, hhin, yd_pc, trimestre, factor_expansion_anual, zona) %>%
  mutate(no_hogar = hhid, urban = if_else(zona == 1, 1, 0)) %>%
  arrange(hhid)

gastos_presim %>%
  select(starts_with("monto")) %>% fsum(na.rm = TRUE)

## Residencia urbano / rural 
urban <- readRDS(paste0(fpresim, "urban.RDS"))

## Matriz insumo producto 2013
mip_data  <- readRDS(paste0(fpresim, "mip2013_rd.RDS"))

## 0.2 FUNCIONES --------------------------------------------------------------

#' Estima el itbis según distintas opciones
#'
#' @param var   numeric vector  variable que contiene datos de gastos de hogares
#' @param escenario numeric escenario de simulación
#' @param itbis_estandar tasa itbis del escenario 
#'
#'
estima_itbis <- function(var, escenario, itbis_estandar) {
  
  ## número del escenario
  nn <- escenario
  
  ## tasa general para el escenario 
  tasa_itbis_estandar <- {{itbis_estandar}}
  
  ## Crear un vector con los nombres esperados
  variable_names <- paste0(c("pt", "pi", "tasa", "gravado"), nn)
  # print(variable_names)
  
  ## Verificar si todas las columnas existen
  ver <- all(variable_names %in% colnames(base_itbis0), na.rm = FALSE)
  # print("revisar si las variables están incluidas")
  stopifnot( ver == TRUE)
  
  ## variable del itbis para cada escenario
  nombre_columna <- paste0("itx_itb", nn, "_hh")
  # print(nombre_columna)
  
  x <- base_itbis0 %>%
    mutate(
      pag = case_when(
        urban == 1 & pag_u == 1 ~ 1,
        urban == 0 & pag_r == 1 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      # Inicializar variable
      !!sym(nombre_columna) := case_when(
        # ---
        # CASO 1: PRODUCTOS EXENTOS (gravado == 0)
        # Solo efectos indirectos (embedded VAT)
        # ---
        !!sym(paste0("gravado", nn)) == 0 ~ 
          {{ var }} * !!sym(paste0("pi", nn)),
        # ---
        # CASO 2: PRODUCTOS GRAVADOS CON EVASIÓN (gravado == 1, pag == 0)
        # Solo efectos indirectos, ajustados por tasa
        # ---
        !!sym(paste0("gravado", nn)) == 1 & pag == 0 ~ 
          {{ var }} * !!sym(paste0("pi", nn)) * 
          (!!sym(paste0("tasa", nn)) / tasa_itbis_estandar),
        # ---
        # CASO 3: PRODUCTOS GRAVADOS SIN EVASIÓN (gravado == 1, pag == 1)
        # Pago completo del IVA
        # ---
        !!sym(paste0("gravado", nn)) == 1 & pag == 1 ~ 
          ({{ var }} / (1 + tasa0 / 100)) * (!!sym(paste0("tasa", nn)) / 100),
        # Default
        TRUE ~ 0
      )
    ) %>%
    mutate(
      # ---
      # CASOS ESPECIALES: TASAS REDUCIDAS O TRANSICIONES
      # ---
      
      !!sym(nombre_columna) := case_when(
        
        # Caso A: Gravado SIN evasión, tasa reducida (menor que estándar)
        # Mantiene efectos indirectos base + nueva tasa
        !!sym(paste0("gravado", nn)) == 1 & pag == 1 &
          !!sym(paste0("tasa", nn)) > 0 &
          !!sym(paste0("tasa", nn)) < tasa_itbis_estandar ~
          {{ var }} * pi0 +
          {{ var }} * ((!!sym(paste0("tasa", nn)) / 100) / (1 + tasa0 / 100)),
        
        # Caso B: Transición de exento a gravado (tasa0 = 0)
        # Efectos indirectos base + nueva tasa completa
        !!sym(paste0("gravado", nn)) == 1 & pag == 1 &
          tasa0 == 0 &
          !!sym(paste0("tasa", nn)) > 0 ~
          {{ var }} * (pi0 + (!!sym(paste0("tasa", nn)) / 100)),
        
        # Caso C: Gravado CON evasión, tasa reducida
        # Solo efectos indirectos del nuevo escenario
        !!sym(paste0("gravado", nn)) == 1 & pag == 0 &
          !!sym(paste0("tasa", nn)) > 0 &
          !!sym(paste0("tasa", nn)) < tasa_itbis_estandar ~
          {{ var }} * !!sym(paste0("pi", nn)),
        
        # Mantener valor calculado anteriormente
        TRUE ~ !!sym(nombre_columna)
      )
    ) %>%
    mutate(
      # Crear variables de consumo por tipo
      !!sym(paste0("cons_exento", nn)) :=
        if_else(!!sym(paste0("gravado", nn)) == 0 & pag == 1, {{ var }}, NA_real_),
      
      !!sym(paste0("cons_exenev", nn)) :=
        if_else(!!sym(paste0("gravado", nn)) == 0 & pag == 0, {{ var }}, NA_real_),
      
      !!sym(paste0("cons_gravto", nn)) :=
        if_else(!!sym(paste0("gravado", nn)) == 1 & pag == 1, {{ var }}, NA_real_),
      
      !!sym(paste0("cons_gravev", nn)) :=
        if_else(!!sym(paste0("gravado", nn)) == 1 & pag == 0, {{ var }}, NA_real_)
    )
  
  x
}

#' Función para convertir todas los ratios a valores suavizados usando
#' regresión local polinómica
#'
#' @param data      data frame  con datos
#' @param xcol      valores x para el suavizamiento
#' @param degree    grado del polinomio local utilizado
#' @param gridsize  número de puntos de cuadrícula para estimar la función
#'
#'
suav_series <- function(data,
                        xcol = "centil",
                        degree = 0,
                        gridsize = 100) {
  # Identificar todas las columnas que comienzan con "ritx"
  ritx_cols <- grep("^ritb", names(data), value = TRUE)
  
  # Si no hay columnas ritx, devolver un mensaje
  if (length(ritx_cols) == 0) {
    return("No se encontraron columnas que comiencen con 'ritx' en dataframe")
  }
  
  # Para cada columna ritx, crear una columna pitx correspondiente
  for (col in ritx_cols) {
    # Crear el nombre de la nueva columna (reemplazar ritx por pitx)
    new_col <- gsub("^ritb", "pitb", col)
    
    # Obtener los valores x e y
    x_values <- data[[xcol]]
    y_values <- data[[col]]
    
    # Descartar los NA para el cálculo
    valid_indices <- which(!is.na(x_values) & !is.na(y_values))
    x_valid <- x_values[valid_indices]
    y_valid <- y_values[valid_indices]
    
    # Si no hay datos válidos, continuar con la siguiente columna
    if (length(valid_indices) == 0) {
      warning(paste("La columna", col, "no tiene datos válidos. Se omite."))
      next
    }
    
    # Calcular el ancho de banda óptimo
    bw <- try(dpill(x_valid, y_valid), silent = TRUE)
    
    # Si hay un error al calcular el ancho de banda, usar valor predeterminado
    if (inherits(bw, "try-error") || is.na(bw) || bw <= 0) {
      bw <- 0.2 * diff(range(x_valid))
      warning(
        paste(
          "No se pudo calcular el ancho de banda para",
          col,
          ". Se usa un valor predeterminado."
        )
      )
    }
    
    # Ajustar la regresión local
    ajuste <- try(locpoly(
      x = x_valid,
      y = y_valid,
      degree = degree,
      bandwidth = bw,
      kernel = "epanech",
      gridsize = gridsize,
      range.x = c(1, 100)  # forzar rango exacto 1-100 como Stata
    ),
    silent = TRUE)
    
    # Si hay un error en la regresión local, continuar con la siguiente columna
    if (inherits(ajuste, "try-error")) {
      warning(paste("Error en la regresión local para", col))
      next
    }
    
    # Interpolar para obtener predicciones
    predictions <- rep(NA, length(x_values))  # Inicializar con NA
    
    # Obtener predicciones solo para valores válidos
    valid_predictions <- approx(
      x = ajuste$x,
      y = ajuste$y,
      xout = x_valid,
      rule = 2
    )$y
    
    # Asignar las predicciones a las posiciones originales
    predictions[valid_indices] <- valid_predictions
    
    # Añadir las predicciones al dataframe
    data[[new_col]] <- predictions
    
    # Informar sobre la columna procesada
    message(paste("Columna procesada :", col, "-> Columna creada:", new_col))
  }
  
  return(data)
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
suav_series_ur <- function(data,
                           group_col = "urban",
                           xcol = "centil",
                           degree = 0,
                           gridsize = 100) {
  
  # Verificar columna de agrupación
  if (!group_col %in% names(data)) {
    stop(paste("La columna", group_col, "no existe en el data frame"))
  }
  
  # Identificar columnas ritx
  ritx_cols <- grep("^ritb", names(data), value = TRUE)
  
  if (length(ritx_cols) == 0) {
    return("No se encontraron columnas que comiencen con 'ritx' en dataframe")
  }
  
  # Procesar por grupo
  resultado <- data %>%
    group_by(across(all_of(group_col))) %>%
    group_modify(~ {
      
      # CORRECCIÓN: usar .y en lugar de cur_group()
      # .y contiene los valores de las variables de agrupación
      grupo_actual <- .y[[1]]  
      message(paste("\n=== Procesando grupo:", grupo_actual, "==="))
      
      data_grupo <- .x
      
      for (col in ritx_cols) {
        new_col <- gsub("^ritb", "pitb", col)
        
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

#' Grafico compara serie vs suavizada
#'
#' @param ratio  serie ratio impuestos a ingreso corriente
#' @param suav   serie suavizada
#' @param tit    título del gráfico
#' @note usa plot en vez de ggplot porque requiere menos transformaciones
#'
plot_s <- function(data, ratio_col, suav_col, tit) {
  plot(
    data$centil,
    data[[ratio_col]],
    pch = 16,
    col = "gray",
    xlab = "Centil",
    ylab = "ratio",
    main = tit
  )
  lines(data$centil, data[[suav_col]], col = "blue", lwd = 2)
}

#' Grafico compara serie vs suavizada para urbano y rural en un solo gráfico
#'
plot_s_ur <- function(data, ratio_col, suav_col, group_col = "urban", tit) {
  
  # Separar datos
  data_urbano <- data[data[[group_col]] == 1, ]
  data_rural <- data[data[[group_col]] == 0, ]
  
  # Determinar rango y
  y_range <- range(c(data[[ratio_col]], data[[suav_col]]), na.rm = TRUE)
  
  # Crear gráfico base
  plot(
    data_urbano$centil,
    data_urbano[[ratio_col]],
    pch = 16,
    col = "lightblue",
    xlab = "Centil",
    ylab = "Ratio",
    main = tit,
    ylim = y_range
  )
  
  # Agregar puntos rurales
  points(data_rural$centil, data_rural[[ratio_col]], pch = 17, col = "lightgreen")
  
  # Líneas suavizadas
  lines(data_urbano$centil, data_urbano[[suav_col]], col = "blue", lwd = 2)
  lines(data_rural$centil, data_rural[[suav_col]], col = "darkgreen", lwd = 2)
  
  # Leyenda
  legend("topleft",
         legend = c("Urbano observado", "Rural observado", 
                    "Urbano suavizado", "Rural suavizado"),
         col = c("lightblue", "lightgreen", "blue", "darkgreen"),
         pch = c(16, 17, NA, NA),
         lty = c(NA, NA, 1, 1),
         lwd = c(NA, NA, 2, 2),
         bty = "n")
}

round_stata <- function(x, digits) {
  powr <- 10^digits
  floor(x * powr + 0.5) / powr
}


# 1. Tasas y lugares de compra -------------------------------------------------

## 1.1  Lugares de compra formales ---------------------------------------------
#- Según consultas MEPYD, DGII
lugares_formales <- read_param_csv("tipo_establecimiento", fparam_csv) 

colnames(lugares_formales)
# [1] "id_tipo_establecimiento"  "des_tipo_establecimiento"
# [3] "pag1_u"                   "pag1_r"                  
# [5] "pag2_u"                   "pag2_r"                  
# [7] "pag3_u"                   "pag3_r"                  
# [9] "pag4_u"                   "pag4_r"  

itb_formal <- sort(unique(sim_itbis$base_formal))  
itb_formal
## [1] 4

# Si itb_formal = c(1, 4)
columnas_deseadas <- paste0("pag", rep(itb_formal, each = 2), 
                            c("_u", "_r"))
# Resultado: c("pag1_u", "pag1_r", "pag4_u", "pag4_r")

lugares_formales <- lugares_formales %>%
  select(id_tipo_establecimiento, des_tipo_establecimiento, 
         all_of(columnas_deseadas))

## 1.2. Guardar tasas del itbis para cada escenario -----------------------------

## Tasas del itbis en el escenario base
tasas_itx <- itbis_base %>%
  rename(tasa0 = tasa) %>%
  select(-des_subclase, -des_clase, -des_subgrupo, -des_variedad, 
         -des_articulo, -cod_subgrupo, -cod_clase, 
         -cod_subclase) %>%
  arrange(id_variedad) %>%
  mutate(gravado0 = ifelse(tasa0 > 0 & !is.na(tasa0), 1, 0)) 

## Tasas del itbis en los escenarios de simulación
tasa1 <- detalle_itbis %>%
  rename_with(~ str_replace(., "^itbis_alt", "sim_"), 
              starts_with("itbis_alt")) %>%
  select(starts_with("cod"), c("variedad"), starts_with("sim")) %>%
  rename(id_variedad = cod_variedad) %>%
  rename_with(~ gsub("^sim_", "tasa", .), starts_with("sim"))

tasas_itx1 <- tasas_itx %>%
  select(-cod_articulo) %>%
  left_join(tasa1, by=c("id_variedad"))

for (i in sim_itbis_esc) {
  # Solo procesar escenarios de simulación (1, 2, ...), no el base (0)
  if (i > 0) {
    nom_it <- paste0("tasa", i)
    uniforma <- sim_itbis$uniforma_tasa[i] == 1
    gravar_exentos <- sim_itbis$exentos_gravados[i] == 1
    nueva_tasa <- sim_itbis$tasa_itbis[i]
    
    tasas_itx1 <- tasas_itx1 %>%
      mutate(
        !!sym(nom_it) := case_when(
          !is.na(!!sym(nom_it)) ~ !!sym(nom_it),
          tasa0 == 0 & gravar_exentos ~ nueva_tasa,
          tasa0 == 0 & !gravar_exentos ~ 0,
          tasa0 != 0 & uniforma ~ nueva_tasa,
          tasa0 != 0 & !uniforma ~ tasa0,
          TRUE ~ !!sym(nom_it)
        )
      )
  }
}

columnas_deseadas <- paste0("tasa", rep(sim_itbis_esc))
# Resultado: c("pag1_u", "pag1_r", "pag4_u", "pag4_r")

tasas_itx1 <- tasas_itx1 %>%
  select(id_variedad, sector2, gravado0, grupo, des_grupo, cod_grupo,
         tasa0, all_of(columnas_deseadas))

tasas_itx1 <- tasas_itx1 %>%
  # elimina columnas con NA
  select(where(~ !all(is.na(.)))) %>%
  # crea columna sobre si el producto está gravado en el escenario
  mutate(across(starts_with("tasa"), ~ if_else(. == 0, 0, 1),
                .names = "g_{col}")) %>%
  # `tasa0` suele excluirse antes del join; `g_tasa0` puede no existir (p. ej. solo tasa1).
  select(-any_of("g_tasa0")) %>%
  rename_with(~ gsub("g_tasa", "gravado", .),
              starts_with("g_tasa")) %>%
  arrange(id_variedad)

fixsectors0 <- tasas_itx1 %>%
  mutate(normal = if_else(grupo == "normal",sector2,NA)) %>%
  filter(!is.na(normal)) %>%
  pull(normal) %>%
  unique() %>%
  sort()


# 2. Efectos indirectos usando MIP --------------------------------------------

## 2.1 Matriz aumentada -------------------------------------------------------

## cantidad de filas que tienen sectores
n_sectors <- mip_data %>%
  select(starts_with("p")) %>%
  ncol()

## fila con totales de la mip
fila_totales <- n_sectors + 1

## Matriz de coeficientes técnicos nacional
ct <- mip_data[1:n_sectors, 2:fila_totales]

## 50% - 50% informal - formal 
formality_matr <- matrix(0.5, nrow = nrow(ct) , ncol = 1)

## Incluir 0 para tener `ct_0` y poder simular efectos indirectos de línea base
## (`pt0`/`pi0`) cuando solo hay filas `sim_itbis` con índice ≥ 1.
bsim_itbis_esc <- sort(unique(c(0L, as.integer(sim_itbis_esc))))

## se ponderan los coeficientes técnicos para sector gravado / no gravado
#- según el grado de formalidad de cada sector
a_ <- list()

for (i in bsim_itbis_esc) {
  j <- i + 1
  
  # Multiplicar toda la matriz por 0.5 (replicando Stata: ct_2 = ct :* 0.5)
  ct_half <- as.matrix(ct * 0.5)
  
  ## Matriz con el doble de filas
  ## Cada fila original se convierte en dos filas idénticas
  ct_2x1 <- matrix(NA, nrow = nrow(ct) * 2, ncol = ncol(ct))
  
  # Duplicar cada fila (replicando el loop de Stata)
  for (row in 1:nrow(ct)) {
    ct_2x1[2*row - 1, ] <- ct_half[row, ]  # Fila impar (formal)
    ct_2x1[2*row, ]     <- ct_half[row, ]  # Fila par (informal)
  }
  
  ## Matriz con el doble de columnas
  ## Duplicar las columnas (replicando Stata: ct_8 = (ct_4, ct_4))
  ct_2x2 <- cbind(ct_2x1, ct_2x1)
  
  # Pero necesitamos intercalar las columnas como lo hace Stata
  ct_final <- matrix(NA, nrow = nrow(ct_2x1), ncol = ncol(ct_2x1) * 2)
  
  for (col in 1:ncol(ct_2x1)) {
    ct_final[, 2*col - 1] <- ct_2x1[, col]  # Columna impar (formal)
    ct_final[, 2*col]     <- ct_2x1[, col]  # Columna par (informal)
  }
  
  ## Enviar los resultados a la matriz a_
  a_[[paste0("ct_", i)]] <- as.matrix(ct_final)
}

rm(ct_2x1, ct_2x2, ct_half, ct_final)

## 2.2 Definir sectores gravados en formato MIP aumentada ----------------------

## Cantidad de sectores en matriz A aumentada
n_sectors <- nrow(a_[[1]])  # Debe ser 48

## Tasa general del itbis en escenario base
t_itbis <- as.numeric(parm_glob$valor[grepl("Tasa", parm_glob$descripcion)] 
                      / 100)

## Tasa efectiva del itbis s/ el precio final
tasa_itbis_base <- t_itbis / (1 + t_itbis)

## Crear sectores que están exentos o gravados, sin duplicados
get_unique_sectors <-
  function(data, gravado_col, value) {
    data %>%
      filter({{ gravado_col }} == value) %>%
      select(sector2) %>%
      unique() %>%
      arrange(sector2)
  }

## Sectores gravados y exentos en el escenario base
g_itbisg0 <- get_unique_sectors(tasas_itx1, gravado0, 1)
g_itbise0 <- get_unique_sectors(tasas_itx1, gravado0, 0)

## Crear columna 'codigo' si no existe (sector original antes de aumentar)
if (!"codigo" %in% names(tasas_itx1)) {
  # Asumir que sector2 es el código original (1 a 24)
  tasas_itx1 <- tasas_itx1 %>%
    group_by(sector2) %>%
    mutate(codigo = first(sector2)) %>%
    ungroup()
}

## Sectores gravados en ley (gravado0), alineados al mismo tratamiento que `a_vector`
## en el bucle. Si el bucle no incluye i==0, sin esto `fix_price_vat0` queda vacío,
## `d` queda vacío y fallan gamma / subset ("undefined columns selected").
a0_for_fix <- get_unique_sectors(tasas_itx1, gravado0, 1) %>%
  mutate(sector2 = ifelse(sector2 %% 2 == 0, sector2 - 1, sector2)) %>%
  distinct() %>%
  arrange(sector2)
fix_price_vat0 <- pull(a0_for_fix, sector2)

## Incluir 0 para generar `pt0`/`pi0` y poder usar `tasa0` en `estima_itbis()`.
variable_values <- sort(unique(c(0L, as.integer(c(sim_itbis$sim_itbis)))))
maxtsim <- tasas_itx1 %>%
  group_by(sector2) %>%
  summarise(across(starts_with("tasa"), max, na.rm = TRUE)) %>%
  mutate(sector2 = ifelse(sector2 %% 2 == 0, sector2 - 1, sector2)) %>%
  group_by(sector2) %>%
  summarise(across(starts_with("tasa"), max, na.rm = TRUE)) 
mattsim <- setNames(data.frame(1:48), "sector2") %>%
  left_join(maxtsim, by=c("sector2"))  %>%
  mutate(across(everything(), ~replace_na(., 0)))

## 2.3 Iterar sobre escenarios para estimar efectos indirectos -----------------

for (i in variable_values) {
  # usar el nombre de la columna gravadoX
  gravado_col <- sym(paste0("gravado", i))
  
  ##  Obtener sectores gravados y no gravados (códigos originales)
  a_tibble <- get_unique_sectors(tasas_itx1, !!gravado_col, 1) # sectores gravados
  
  # Convertir pares a impares y eliminar duplicados
  a_tibble <- a_tibble %>%
    mutate(sector2 = ifelse(sector2 %% 2 == 0, sector2 - 1, sector2)) %>%
    distinct() %>%
    arrange(sector2)
  
  # Convertir a vector
  a_vector <- a_tibble %>% pull(sector2)
  
  # b = todos los sectores de 1 a 48 que NO están en a
  b_vector <- setdiff(1:n_sectors, a_vector)
  
  ## asignar valores a nombres específicos s/ escenario
  assign(paste0("g_itbisg", i), a_tibble)
  assign(paste0("g_itbise", i), tibble(sector2 = b_vector))
  
  ## Identificar nuevos sectores gravados (códigos originales)
  # CORRECCIÓN: usar a_vector en lugar de a
  c <- as.numeric(a_vector[!is.element(a_vector, g_itbisg0$sector2)])
  assign(paste0("g_itbisd", i), c)
  
  ## Actualizar sectores gravados en la MIP AUMENTADA
  if (i == 0) {
    # Escenario base: usar directamente los sectores de a_vector
    d <- a_vector
    fix_price_vat0 <- d
  } else {
    # Simulaciones: incluir nuevos sectores gravados
    if (length(c) > 0) {
      d <- sort(unique(c(fix_price_vat0, c)))
    } else {
      d <- fix_price_vat0
    }
  }
  
  assign(paste0("fix_price_vat", i), d)
  
  # Verificación de seguridad
  cat("\n=== Escenario", i, "===\n")
  cat("Sectores gravados (a_vector):", a_vector, "\n")
  cat("Sectores en d:", d, "\n")
  cat("Max sector:", max(d), "| n_sectors:", n_sectors, "\n")
  
  if (max(d) > n_sectors) {
    stop(paste("ERROR: max(d) =", max(d), "> n_sectors =", n_sectors))
  }
  
  ## Tasa efectiva
  titbisp <- (mattsim[[paste0("tasa", i)]]/100)/
    (1+mattsim[[paste0("tasa", i)]]/100)
  
  ## Crear matriz dp (cambio de precios)
  dp       <- matrix(0, nrow = 1, ncol = n_sectors)
  ##dp[1, d] <- titbisp
  dp  <- titbisp
  assign(paste0("dp_sim", i), dp)
  
  ## Crear matriz gamma
  gamma <- matrix(0, nrow = n_sectors, ncol = n_sectors)
  gamma[cbind(d, d)] <- 1
  
  ### Cálculos de efectos indirectos
  i_dt    <- diag(1, nrow = n_sectors)
  alpha   <- i_dt - gamma
  v_      <- solve(i_dt - alpha %*% a_[[paste0("ct_", i)]])
  deltapt <- dp %*% a_[[paste0("ct_", i)]] %*% v_
  deltap  <- (dp %*% gamma) + (deltapt + dp) %*% alpha
  
  # Guardar resultados
  assign(paste0("deltaptilda_", i), deltapt)
  assign(paste0("deltap_", i), deltap)
}

## 2.4 Limpiar y revisar efectos indirectos ------------------------------------

# Obtener nombres de matrices de efectos indirectos
matrices <- c(
  ls(pattern = "^deltap_"),
  ls(pattern = "^deltaptilda_")
)

# Crear data frame de efectos indirectos en un pipeline
i_effects <- matrices %>%
  map(~ setNames(as.data.frame(t(get(.x))), .x)) %>%
  bind_cols() %>%
  rename_with(~ str_replace_all(.x, c("^deltap_" = "pt", 
                                      "^deltaptilda_" = "pi")))

variable_values <- unique(variable_values)
## Data frame con tasas para cada uno de los sectores
for (i in variable_values) {
  # Cada una de las filas corresponde al número del sector
  ieff_filt <- i_effects %>%
    mutate(sector2 = row_number()) %>%
    select(sector2, ends_with(paste0(i)))
  
  # Cambiar los nombres de cada sector según cada escenario
  tasas_itx1 <- tasas_itx1 %>%
    left_join(ieff_filt, by = "sector2") %>%
    mutate(sectorx = sector2) %>%
    rename_with(~ paste0("sector2_", i), .cols = "sectorx")
}

## Diagnóstico: `pt0`/`pi0` solo existen si se simuló el índice 0; con un solo
## `sim_itbis` activo suele haber `pt1`, `pi1`, etc.
if (all(c("pt0", "pi0") %in% names(i_effects))) {
  print(summary(i_effects[, c("pt0", "pi0"), drop = FALSE]))
}

## eliminar variables de la memoria
rm(
  a_, c, tasas_itx, tasa1, 
  gamma, i_dt, alpha, v_, a_tibble, ct, formality_matr, 
  ieff_filt, maxtsim, mattsim, 
  a_vector, b_vector, detalle_itbis, mip_data
)

rm(list = ls(pattern = "delta"))
rm(list = ls(pattern = "dp"))
rm(list = ls(pattern = "g_"))

# 3. Simular itbis --------------------------------------------------------------

## 3.1 Limpieza inicial de la base de compras ---------------------------------

base_itbis0 <- gastos_presim %>%
  mutate(monto_pagado_hogar_mensual = 
           replace_na(monto_pagado_hogar_mensual, 0)) %>%
  left_join(tasas_itx1, by = "id_variedad") %>%
  left_join(lugares_formales, by=c("id_tipo_establecimiento")) %>%
  rename_with(~ str_remove(., "\\d+"), 
              starts_with("pag")) %>%
  left_join(urban, by = "hhid")  %>%
  filter(cod_grupo !=20)

base_itbis0 %>% select(starts_with("monto")) %>%
  fsum(na.rm = TRUE)

## 3.2 Estima itbis  -----------------------------------------------------------

for (i in variable_values) {
  nombre_columna <- paste0("sh_itbis", i)
  # variable de compras = monto_pagado_hogar_mensual, escenario = i
  # print(i)
  tasa_ <- ifelse(i == 0, t_itbis * 100, sim_itbis$tasa_itbis[i])
  base_itbis0 <- estima_itbis(monto_pagado_hogar_mensual, escenario = i,
                              tasa_) %>%
    mutate(!!sym(nombre_columna) :=
             !!sym(paste0("itx_itb", i, "_hh")) / monto_pagado_hogar_mensual)
}

## 3.3 Revisar resultados  ----------------------------------------------------

summary(base_itbis0[!is.na(base_itbis0$sh_itbis0), c("sh_itbis0")])

# sh_itbis0       
# Min.   :0.003463  
# 1st Qu.:0.034546  
# Median :0.152542  
# Mean   :0.097372  
# 3rd Qu.:0.152542  
# Max.   :0.188668 

base_itbis0 %>%
  summarise(across(starts_with("itx_itb"), sum, na.rm = TRUE)) %>%
  mutate(across(starts_with("itx"), ~ comma(. )))

base_itbis0 %>%
  select(des_grupo, starts_with("itx_itb")) %>%
  fgroup_by(des_grupo) %>%
  fsum(na.rm = TRUE) %>%
  mutate(across(starts_with("itx"), ~ comma(. )))

## 3.4 Crear resultados a nivel de hogar  -------------------------------------

# Copia  con data.table (menor tiempo de procesamiento)
base_itbish <- 
  as.data.table(
    base_itbis0)[, 
                 c(
                   .(monto_total_mensual        = 
                       sum(monto_total_mensual, na.rm = TRUE),
                     monto_pagado_hogar_mensual = 
                       sum(monto_pagado_hogar_mensual,
                           na.rm = TRUE),
                     factor_expansion           = 
                       mean(factor_expansion, na.rm = TRUE)),
                   lapply(.SD, sum, na.rm = TRUE)
                 ),
                 by = hhid,
                 .SDcols = patterns("^cons|^itx_itb")
    ]  

# Crear columnas s_*
itx_cols <- names(base_itbish)[grepl("^itx_itb", names(base_itbish))]
base_itbish[, 
            (paste0("s_", itx_cols)) := 
              lapply(.SD, function(x) x / monto_pagado_hogar_mensual),
            .SDcols = itx_cols
]
cols_to_fix <- names(base_itbish)[grepl("^itx_itb|^s_itx", names(base_itbish))]
setnafill(base_itbish, fill = 0, cols = cols_to_fix)
base_itbish <- as_tibble(base_itbish) 

summary(base_itbish$s_itx_itb0_hh)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.06967 0.08077 0.08147 0.09259 0.16537 

rm(gastos_presim, matrices)
rm(list = ls(pattern = "gravado"))
rm(list = ls(pattern = "fix"))

## 3.5 Ajustes finales usando ENGHI 2018  -------------------------------------


dinc2018 <- dispinc2018 %>%  
  left_join(base_itbish, by = c("hhid")) %>%
  select(-factor_expansion) %>%
  left_join(gasto_total, by = c("hhid")) %>%
  filter(!(yd_pc==0 & ym_pc==0)) %>%
  arrange(hhid) %>%
  mutate(pondera_2023 = round_stata(pondera_2023, 0))

for (i in variable_values) {
  nombre_hh <- paste0("itx_itb", i,"_hh")
  nombre_pc <- paste0("itx_itb", i,"_pc")
  nombre_ef <- paste0("eff_itb", i,"_pc")
  nombre_ra <- paste0("ritb", i,"_pc")
  
  # variable de compras = monto_pagado_hogar_mensual, escenario = i
  print(i)
  
  # Corrección: agregar NA como caso adicional en el fallback
  dinc2018 <- dinc2018 %>%
    mutate(!!sym(nombre_ef) := !!sym(nombre_hh) / gasto_total) %>%
    mutate(!!sym(nombre_ef) := if_else(
      !!sym(nombre_ef) > 0.2 | is.na(!!sym(nombre_ef)),  # <-- agregar is.na()
      !!sym(nombre_hh) / monto_total_mensual,
      !!sym(nombre_ef)
    ))
  
  # Calcular media ponderada para relation==1
  mean_eff <- weighted.mean(
    dinc2018[[nombre_ef]][dinc2018$relation == 1],
    dinc2018$pondera_2023[dinc2018$relation == 1],
    na.rm = TRUE
  )
  
  dinc2018 <- dinc2018  %>%
    mutate(!!sym(nombre_ef) := if_else(!!sym(nombre_ef) == 0 | 
                                         is.na(!!sym(nombre_ef)),
                                       mean_eff,
                                       !!sym(nombre_ef))) %>%
    mutate(!!sym(nombre_pc) := !!sym(nombre_ef) * yd_pc) %>%
    mutate(!!sym(nombre_pc) := if_else(!!sym(nombre_pc) == 0 | 
                                         is.na(!!sym(nombre_pc)),
                                       mean_eff * yd_pc,
                                       !!sym(nombre_pc))) %>%
    mutate(!!sym(nombre_ra) := !!sym(nombre_pc) / yd_pc) 
  
}


dinc2018 %>%
  select(starts_with("r"), starts_with("eff")) %>%
  summary()

dinc2018 %>%
  select(starts_with("itx") & ends_with("pc")) %>%
  summary()

# Para un resultado simple
dinc2018 %>%
  select(starts_with("itx") & ends_with("pc")) %>%
  fsum(na.rm = TRUE) %>%
  comma()

# 4. Imputar ITBIS en ENCFT ----------------------------------------------------

## 4.1 Calcular ratios ITBIS -----------------------------------------------------

dinc2018 <- dinc2018 %>%
  mutate(decil  = xtile(yd_pc, n=10,  wt = pondera_2023),
         centil = xtile(yd_pc, n=100, wt = pondera_2023))

dinc2018 %>%
  group_by(decil) %>%
  summarise(n=n())

dinc2018 %>%
  group_by(centil) %>%
  summarise(n=n())  

weighted.mean(dinc2018$ritb0_pc, dinc2018$pondera_2023)
## [1] 0.06646568

dinc2018 %>%
  group_by(decil) %>%
  summarise(Mean = weighted.mean(ritb0_pc, pondera_2023, na.rm = TRUE)) %>%
  arrange(decil) %>%
  mutate(decil = as.character(decil)) %>%
  bind_rows(
    dinc2018 %>%
      summarise(decil = "Total", 
                Mean = weighted.mean(ritb0_pc, pondera_2023, na.rm = TRUE))
  ) %>%
  gt() %>%
  fmt_number(columns = Mean, decimals = 7) %>%
  cols_label(decil = "", Mean = "Mean") %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold"
  )

## ajustar outliers 
# Diagnóstico antes de aplicar el winsorizing
cols <- grep("^ritb[0-9]_pc$", names(dinc2018), value = TRUE)

diagnostico <- map_dfr(cols, ~ {
  x   <- dinc2018[[.x]]
  p99 <- quantile(x[x > 0 & !is.na(x)], probs = 0.99, 
                  na.rm = TRUE, type = 2)  
  tibble(
    columna            = .x,
    p99                = formatC(p99, digits = 8, format = "f"),
    n_cambiadas        = sum(x > p99 & !is.na(x)),
    valor_min_cambiado = formatC(min(x[x > p99 & !is.na(x)], na.rm = TRUE), 
                                 digits = 7, format = "f"),
    valor_max_cambiado = formatC(max(x[x > p99 & !is.na(x)], na.rm = TRUE), 
                                 digits = 7, format = "f")
  )
})

print(diagnostico)

# Luego aplicas el mutate
dinc2018 <- dinc2018 %>%
  mutate(across(
    matches("^ritb[0-9]_pc$"),
    ~ {
      p99 <- quantile(.[. > 0 & !is.na(.)], probs = 0.99, 
                      na.rm = TRUE, type = 2)
      ifelse(. > p99 & !is.na(.), p99, .)
    }
  )) 


sum(dinc2018$ritb0_pc)
sum(dinc2018$pondera_2023)

weighted.mean(dinc2018$ritb0_pc, dinc2018$pondera_2023)
##  [1] 0.06634346

dinc2018 %>%
  group_by(centil, urban) %>%
  summarise(across(starts_with("ritb"), ~ weighted.mean(., wt=pondera_2023),
                   .names = "ritx{col}"),
            .groups = "drop") %>%
  filter(urban==0)

dinc2018 %>%
  group_by(centil, urban) %>%
  summarise(n = n(),  .groups = "drop") %>%
  pivot_wider(names_from = urban, values_from = n,
              names_prefix = "urban_")

dinc2018 %>%
  group_by(centil, urban) %>%
  summarise(n = mean(ritb0_pc),  .groups = "drop") %>%
  pivot_wider(names_from = urban, values_from = n,
              names_prefix = "urban_")            


dinc2018 %>%
  group_by(centil, urban) %>%
  summarise(n = weighted.mean(ritb0_pc, wt=pondera_2023),
            .groups = "drop") %>%
  pivot_wider(names_from = urban, values_from = n,
              names_prefix = "urban_")                        

dinc2018 %>%
  group_by(centil, urban) %>%
  summarise(n =sum(pondera_2023),
            .groups = "drop") %>%
  pivot_wider(names_from = urban, values_from = n,
              names_prefix = "urban_")                                    

# Aplicar a columnas
dinc2018 <- dinc2018 %>%
  mutate(across(starts_with("rit"), ~ round_stata(., 9)))

## write_dta(dinc2018, paste0(fdbmod,"dinc2018_R.dta"))

## 4.2 Suavizar ratios ITBIS ---------------------------------------------------

ratios <- dinc2018 %>%
  group_by(centil, urban) %>%
  summarise(
    across(
      starts_with("ritb"),
      ~ weighted.mean(., w = round_stata(pondera_2023, 0)),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

ratios_s <- suav_series_ur(
  data = ratios,
  group_col = "urban",  
  xcol = "centil",
  degree = 0,
  gridsize = 100
)

# Combinados
plot_s_ur(ratios_s, "ritb0_pc", "pitb0_pc", tit = "ITBIS simulación base")

ratios_s %>%
  group_by(urban) %>%
  summarise(across(starts_with(c("pitb", "ritb")), ~ mean(., na.rm = TRUE)))

#write_dta(ratios_s, paste0(fdbmod,"ratios_R.dta"))


## 4.3 Resultados en ENCFT -----------------------------------------------------

dinc2024 <- dispinc2024 %>%
  mutate(decil  = xtile(round_stata(yd_pc, 0), n=10,  
                        wt = round_stata(factor_expansion_anual,0)),
         centil = xtile(round_stata(yd_pc, 0), n=100, 
                        wt = round_stata(factor_expansion_anual,0))) %>%
  left_join(ratios_s, by=c("urban","centil")) %>%
  arrange(hhid, hhin, trimestre)

dinc2024 %>%
  group_by(centil) %>%
  summarise(n = n())

dinc2024 %>%
  arrange(centil, hhid, hhin, trimestre)  %>%
  group_by(centil, urban) %>%
  summarise(across(starts_with(c("pitb")), ~ mean(., na.rm = TRUE)), 
            .groups = "drop_last")

# Nombres de columnas
cols_ef <- paste0("pitb",    variable_values, "_pc")
cols_pc <- paste0("itx_itb", variable_values, "_pc")

# Crear nuevas columnas directamente
for (i in seq_along(variable_values)) {
  dinc2024[[cols_pc[i]]] <- dinc2024[[cols_ef[i]]] * dinc2024$yd_pc
}

dinc2024[, c("hhid","trimestre","yd_pc","ritb0_pc","itx_itb0_pc")]

dinc2024 %>%
  arrange(hhid, hhin, trimestre)  %>%
  select(hhid, hhin, trimestre, starts_with(c("itx","pitb")), centil)


dinc2024 %>%
  select(starts_with("itx") & ends_with("pc")) %>%
  fsum(na.rm = TRUE) %>%
  comma()
# itx_itb0_pc     itx_itb1_pc     itx_itb2_pc 
# "936,134,585" "1,310,107,037" "1,133,873,749" 

dinc2024 %>%
  select(starts_with("itx") & ends_with("pc")) %>%
  fsum(w = dinc2024$factor_expansion_anual, na.rm = TRUE) %>%
  comma()
# itx_itb0_pc       itx_itb1_pc       itx_itb2_pc 
# "162,386,798,478" "226,212,683,599" "196,142,337,350" 

dinc2024 %>%
  select(hhid, hhin, trimestre, yd_pc, ritb0_pc, pitb0_pc, 
         starts_with("itx") & ends_with("pc"))


# 5. Guardar información   ----------------------

## 5.1 Merge del ingreso disponible con los datos de gastos --------------------
dom_sim_itbis <- dinc2024 %>%
  select(hhid, hhin, trimestre, zona, factor_expansion_anual, yd_pc, 
         no_hogar, urban, starts_with("itx"), centil)
summary(dom_sim_itbis)
sum(dom_sim_itbis$itx_itb0_pc)
itx_cols_diag <- intersect(
  paste0("itx_itb", variable_values, "_pc"),
  names(dom_sim_itbis)
)
if (length(itx_cols_diag)) {
  invisible(dom_sim_itbis[, c("hhid", "hhin", "trimestre", itx_cols_diag), drop = FALSE])
}

## 5.2  grabar información -----------------------------------------------------

saveRDS(dom_sim_itbis, paste0(fdbmod, "DOM_simitbis.rds"))

## Graba Ingreso disponible (versión simple para consolidar escenarios)
saveRDS(dispinc2024, paste0(fdbmod, "DOM_ingdisponible.rds"))

## 5.2 eliminar datos de memoria -----------------------------------------------
rm(list = ls(pattern = "base"))
rm(list = ls(pattern = "dinc"))
rm(list = ls(pattern = "dispinc"))
rm(list = ls(pattern = "nom"))
rm(list = ls(pattern = "fix"))
rm(dom_sim_itbis)
rm(j, bsim_itbis_esc)
rm(sim_itbis_esc)

rm(
  i_effects, gasto_total, lugares_formales, tasas_itx1, ratios, ratios_s, 
  urban, col, cols_ef, cols_pc, columnas_deseadas, d, fila_totales,
  i, itb_formal, itx_cols, mean_eff, nueva_tasa, 
  row, titbisp, variable_values, 
  estima_itbis, get_unique_sectors, list_libraries,
  install_if_missing, loaded_successfully, n_sectors, diagnostico, 
  cols)


# # Ver qué bandwidth usa R para cada grupo
# for (grp in c(0, 1)) {
#   x <- ratios$centil[ratios$urban == grp]
#   y <- ratios$ritb0_pc[ratios$urban == grp]
#   bw <- dpill(x, y)
#   cat("Urban =", grp, "| BW dpill:", round(bw, 4), "\n")
# }

