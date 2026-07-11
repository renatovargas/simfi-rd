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
## Version:    	  			      2.0 
## Fecha de modificación:     2026-03-10

# 0. Importar archivos y definir funciones -------------------------------------

## 0.1 Importar archivos -------------------------------------------------------

# El escenario base (0) siempre se estima junto al escenario simulado, igual que
# en 01_DOM26_itbis.R (bsim_itbis_esc). Parámetros desde CSV (data/params/).
sim_sub_esc <- sort(unique(c(0L, as.integer(escenarios$sim_sub))))

sim_sub <- read_param_csv("sim_sub", fparam_csv) %>%
  filter(activo == 1) %>%
  filter(sim_sub %in% sim_sub_esc)

# La app local puede inyectar una fila personalizada (sim_sub_row_override) que
# reemplaza la fila del escenario simulado, conservando la fila base (0).
if (exists("sim_sub_row_override", inherits = FALSE) &&
    is.data.frame(sim_sub_row_override) && nrow(sim_sub_row_override) > 0L) {
  ov <- sim_sub_row_override %>%
    filter(activo == 1) %>%
    filter(sim_sub %in% sim_sub_esc)
  if (nrow(ov) > 0L) {
    sim_sub <- sim_sub %>%
      filter(!(sim_sub %in% ov$sim_sub)) %>%
      bind_rows(ov) %>%
      arrange(sim_sub)
  }
}

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


## Ingreso disponible 2024
base2024 <- readRDS(paste0(fpresim, "DOM26_SDIncome.RDS")) %>%
  select(hhid, hhin, trimestre, factor_expansion_anual, yd_pc, zona, 
         tipo_alumbrado) %>%
  arrange(hhid, hhin)

## Ingreso disponible CEQ 2023
dispinc2018 <- readRDS(paste0(finput, "DOMCEQ_DIncome.RDS")) %>%
  select(hhid, yd_pc, ym_pc, relation, weight, urban, pondera_2023) %>%
  arrange(hhid)

elec <- readRDS(paste0(fpresim, "elec.RDS"))

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


# 1. Estimación de escenarios --------------------------------------------------

# OPTIMIZACIÓN: precalcular fuera del loop los valores que no cambian entre
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
  
  elec[[sub_col]]  <- pmax(0, elec[[costo_col]] - elec[[precio_col]],
	na.rm = TRUE)
  elec[[subt_col]] <- pmax(0,
                           elec$consumo_e * elec[[costo_col]] -
						   elec[[pago_col]],na.rm = TRUE)
  
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

# 2. Guardar datos subsidios simulados -----------------------------------------

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
cat("Sum sub_ele0_pc:   ", sum(subelec$sub_ele0_pc, na.rm=TRUE), "\n")
cat("Mean sub_ele0_pc:  ", mean(subelec$sub_ele0_pc, na.rm=TRUE), "\n") # debe ≈ 123.3
cat("Sum sub_ele1_pc:   ", sum(subelec$sub_ele1_pc, na.rm=TRUE), "\n")
cat("Mean sub_ele1_pc:  ", mean(subelec$sub_ele1_pc, na.rm=TRUE), "\n") # debe ≈ 109.8


# 3. Imputar resultados --------------------------------------------------------

base <- readRDS(paste0(finput, "DOMCEQ_DIncome.RDS"))

gtotal <- readRDS(paste0(fpresim, "DOM26_gtotal.RDS")) %>%
                   select(hhid, gasto_total)

cod_gasto <- read_param_csv("itbis_base", fparam_csv) %>%
  select(-cod_arancelario) %>%
  select(id_variedad, cod_grupo)

gtotals <- readRDS(paste0(fpresim, "DOM26_gtotals.RDS")) %>%
           select(hhid, id_variedad, monto_total_mensual) %>%
  left_join(cod_gasto, by = "id_variedad") %>%
  filter(cod_grupo != 20) %>%
  group_by(hhid) %>%
  summarise(monto_total = sum(monto_total_mensual, na.rm = TRUE))

base <- base %>%
  left_join(subelec, by = "hhid") %>%
  left_join(gtotal,  by = "hhid") %>%
  left_join(gtotals, by = "hhid") %>%
  filter(!(yd_pc == 0 & ym_pc == 0)) %>%
  select(-starts_with("sub_ela"), -starts_with("sub_elb"),
         -starts_with("sub_elc")) 

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

# ============================================================================
# CORREGIDO: el codigo original usaba pmin(x, y, na.rm = TRUE) en dos lugares.
# Con na.rm = TRUE, pmin() NO preserva NA cuando uno de los dos vectores tiene
# NA -- en su lugar devuelve el otro valor no-faltante (aqui, el percentil 99).
# Esto convertia todo valor faltante en el percentil 99 (un valor ALTO) en vez
# de dejarlo como NA para luego imputarle el promedio (mean_eff). El bug se
# agravaba en escenarios mas focalizados (mas hogares con subsidio = 0
# legitimo), inflando el promedio en vez de bajarlo.
#
# La correccion distingue explicitamente:
#   - x_is_na / eff_is_na  -> dato realmente faltante (se imputa mean_eff)
#   - eff_col == 0         -> hogar sin subsidio, valor legitimo (se conserva)
# ============================================================================
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
  
  # replace eff_z = mean_eff if eff_z == . (SOLO missing real, no cero legitimo)
  base[[eff_col]] <- ifelse(eff_is_na, mean_eff, base[[eff_col]])
  # gen pc = yd_pc * eff_z
  base[[pc_col]] <- base$yd_pc * base[[eff_col]]
  
  # resguardo de consistencia (ya no deberia activarse tras el fix de arriba)
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

## 3.1 Ratio sobre ingreso disponible ------------------------------------------

base <- base %>%
  mutate(across(starts_with("sub") & ends_with("pc"),
                ~ .x / yd_pc,
                .names = "r{.col}"))


format(summary(base$rsub_ele0_pc), scientific = FALSE, digits = 3)

base2 <- base %>%
  rename(sub_ele0_h2 = sub_ele0_hh, sub_ele1_h2 = sub_ele1_hh,
         sub_ele0_p2 = sub_ele0_pc, sub_ele1_p2 = sub_ele1_pc,)


# 4. Estimar ratios  ----------------------------------------------------
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
    starts_with("rsub_ele") & ends_with("_pc"),
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


## 4.1 Suavizado locpoly ------------------------------------------------------

subratios <- base %>%
  filter(electricos != 0 | pago_e_0 != 0) %>%
  group_by(centil, urban) %>%
  summarise(
    across(
      starts_with("rsub"),
      ~ weighted.mean(., w = round_stata(pondera_2023, 0)),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

sub_percentil <- suav_subs_ur(
  data = subratios,
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



# Diagnóstico opcional (desactivado en la app: evita abrir dispositivos gráficos
# durante run_pipeline_cached). Active con options(dom.pipeline.diagnostics = TRUE).
if (isTRUE(getOption("dom.pipeline.diagnostics", FALSE))) {
  plot(
    sub_percentil$centil[sub_percentil$urban == 1],
    sub_percentil$rsub_ele0_pc[sub_percentil$urban == 1],
    pch  = 18, col  = "lightblue",
    xlab = "Centil", ylab = "Ratio", main = "Urbano"
  )
  lines(sub_percentil$centil[sub_percentil$urban == 1],
        sub_percentil$psub_ele0_pc[sub_percentil$urban == 1],
        col = "blue", lwd = 2)
}

# 5. Imputación de subsidio eléctrico en ENCFT ---------------------------------

base2024 <- base2024 %>%
  mutate(
		centil = xtile(round(yd_pc,0), wt = round(factor_expansion_anual, 0), 
		n = 100),
    urban  = as.integer(zona == 1)
  ) %>%
  left_join(sub_percentil, by = c("urban", "centil"))

base2024 %>%
	group_by(centil) %>%
	summarise(n = n())

# OPTIMIZACIÓN: construir todas las columnas sub_ele*_pc en un solo mutate
sub_cols_2024 <- lapply(sim_sub_esc, function(s) {
  tibble(
    !!paste0("sub_ele", s, "_pc") :=
      base2024$yd_pc *
      base2024[[paste0("psub_ele", s, "_pc")]] *
      (base2024$tipo_alumbrado == 1)
  )
}) %>% bind_cols()

base2024 <- bind_cols(base2024, sub_cols_2024)


# Sintaxis correcta de fsum con pesos en collapse:
fsum(base2024$sub_ele0_pc, w = base2024$factor_expansion_anual, na.rm = TRUE)
fsum(base2024$yd_pc, w = base2024$factor_expansion_anual, na.rm = TRUE)/1000

base2024[, c("hhid","hhin", "trimestre","urban", "sub_ele0_pc",
             "sub_ele1_pc","psub_ele0_pc")]

# 6. Guardar resultados  -------------------------------------------------------

base2024 <- base2024 %>%
  select(hhid, hhin, trimestre, starts_with("sub_ele"))

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
rm(acunor_0, acunor_1, acuses, acuses_0, acuses_1, b2,
   ceste_val, cnorte_val, 
   csur_val,  ij,     
   mean_eff, n_bloques, nombre, 
   perc99, s, st, t2, to, tt, x, z)
rm(acunor, consumo2, costo_sures, eff_validos, gravar_exentos, idx_decil,
   mask_jefes, mask_norte, mask_sures, mask_validos, max_vars, mean_consumo_v,
   pa4o_vals, pago_vals, pesos_validos, rename_map, siminc, sim_renta_esc, 
   plot_s, plot_s_ur, suav_series, suav_series_ur, suav_subs_ur)

rm(calcular_pago_bloques)

# # Ver qué bandwidth usa R para cada grupo
# for (grp in c(0, 1)) {
#   x <- subratios$centil[subratios$urban == grp]
#   y <- subratios$rsub_ele0_pc[subratios$urban == grp]
#   bw <- dpill(x, y)
#   cat("Urban =", grp, "| BW dpill:", round(bw, 4), "\n")
# }