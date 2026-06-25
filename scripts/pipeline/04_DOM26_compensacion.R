## Banco Mundial
## Herramienta de Microsimulación
## Country: Dominican Republic, 2026
## Authors: Maynor Cabrera, Renato Vargas
## Script: Maynor Cabrera
## 04_DOM26_compensacion.R
## E-email:       mynorvc@gmail.com
##  Dependencias:  00_DOM26_Master.R
##
## Input files
## -- data/pipeline/r_params/param_csv/*.csv
## -- Procesados
## ---- DOM26_SDIncome.dta
## ---- DOM_simitbis.rds
## ---- DOM_simirenta.rds  
## ---- DOM_simsubele.rds
##
## Output files:
## -- DOM_simcompensacion.rds
##
## Fecha de creación:         5 Mar 2026 - 15:02
## Version:                   1.3
## Fecha de modificación:     2026-04-09

# 1. Importar archivos --------------------------------------------------------

## Solo estima sim_comp activos en escenarios
sim_com_esc <- unique(escenarios$sim_com)

# Escenarios Compensación
sim_com <- read_param_csv("sim_comp", fparam_csv) %>%
  filter(activo == 1) %>%
  filter(sim_comp %in% sim_com_esc)

if (exists("sim_comp_override", inherits = FALSE) &&
    is.data.frame(sim_comp_override) && nrow(sim_comp_override) > 0L) {
  sim_com <- sim_comp_override %>%
    dplyr::filter(.data$activo == 1L, .data$sim_comp %in% sim_com_esc)
}

dom_ingdis <- readRDS(paste0(fpresim, "DOM26_SDIncome.RDS")) %>%
  select(hhid, hhin, yd_pc, ym_pc, trimestre, factor_expansion_anual,
    zona, yn_pc, edad, relation, sexo, dtr_supe_pc, pline_mod, 
    pline_ext, macro_region, icv) %>%
  filter(!(yd_pc==0 & ym_pc==0)) %>%
  mutate(no_hogar = hhid, urban = if_else(zona == 1, 1, 0)) %>%
  arrange(hhid)

dom_sim_itbis <- readRDS(paste0(fdbmod, "DOM_simitbis.rds")) %>%
  select(-yd_pc, -factor_expansion_anual, -no_hogar, -urban, -zona)

dom_sim_irenta <- readRDS(paste0(fdbmod, "DOM_simirenta.rds")) %>%
  select(-Ilab_isr12a, starts_with("dtx") & ends_with("_hh"), 
  -factor_expansion_anual)

dom_sim_subele <- readRDS(paste0(fdbmod, "DOM_simsubele.rds"))

## Estratos según Paridad de poder adquisitivo (línea pobreza internacional)
## Actualizado al ppp 2021
estratos <- 23.135822 * (174.88908 / 148.47661) * 365 * c(8.3, 17, 98)

# 2. Construir base principal -------------------------------------------------
# OPT: pre-unir tablas de simulación entre sí antes del join con dom_ingdis
#      → reduce de 3 joins encadenados sobre dom_ingdis a 1
# OPT: calcular hh_demo en un solo summarise (any() cortocircuita antes que max())
# OPT: un solo mutate al final en vez de 4 separados
# OPT: ave() reemplaza group_by %>% mutate %>% ungroup para sjefe

hh_demo <- dom_ingdis %>%
  group_by(hhid) %>%
  summarise(
    oldhh = as.integer(any(edad > 65)),
    chihh = as.integer(any(edad < 15)),
    .groups = "drop"
  )

sim_combined <- dom_sim_itbis %>%
  left_join(dom_sim_irenta, by = c("hhin", "hhid", "trimestre")) %>%
  left_join(dom_sim_subele, by = c("hhin", "hhid", "trimestre"))

DOM_results <- dom_ingdis %>%
  left_join(sim_combined, by = c("hhin", "hhid", "trimestre")) %>%
  left_join(hh_demo, by = "hhid") %>%
  mutate(
    across(matches("^(itx|sub|dtx).*_pc$"), ~ ., .names = "s{col}"),
    cathhd = case_when(
      chihh == 0 & oldhh == 0 ~ 1L,
      chihh == 1 & oldhh == 0 ~ 2L,
      chihh == 0 & oldhh == 1 ~ 3L,
      chihh == 1 & oldhh == 1 ~ 4L
    ),
    yd0_pc    = yd_pc,
    pline_830 = 365 * 8.30 * 23.135822 * (174.88908 / 148.47661),
    jefe      = relation == 1,
    gender    = sexo == 2,
    sjefe     = ave(as.integer(relation == 1 & sexo == 2),
                   hhid, FUN = max) + 1L
  ) %>%
  filter(!(yd_pc==0 & ym_pc==0))

# Valores de referencia base
vs_itb <- fsum(DOM_results$itx_itb0_pc,
               w = DOM_results$factor_expansion_anual,
               na.rm = TRUE) / 1e6

vs_sub <- fsum(DOM_results$sub_ele0_pc,
               w = DOM_results$factor_expansion_anual,
               na.rm = TRUE) / 1e6

# 3. Estimar escenarios (impuestos + ingresos en un solo loop) ----------------

variable_values <- c(0, escenarios$escenario)
library(rlang)
for (i in variable_values) {

  nom_itbx <- paste0("itx_itb", i, "_pc")
  nom_isrx <- paste0("dtx_isr", i, "_pc")
  nom_subx <- paste0("sub_ele", i, "_pc")
  nom_yd   <- paste0("yd",  i, "_pc")
  nom_yc   <- paste0("yc",  i, "_pc")
  nom_yi   <- paste0("yi",  i, "_pc")
  nom_ye   <- paste0("ys",  i, "_pc")

  # Columnas de simulación solo para escenarios distintos del base
  new_cols <- if (i != 0) {
    esc_i <- escenarios[escenarios$escenario == i, ]
    exprs(
      !!sym(nom_itbx) := !!sym(paste0("sitx_itb", esc_i$sim_itbis, "_pc")),
      !!sym(nom_isrx) := !!sym(paste0("sdtx_isr", esc_i$sim_renta, "_pc")),
      !!sym(nom_subx) := !!sym(paste0("ssub_ele",  esc_i$sim_sub,   "_pc"))
    )
  } else {
    list()
  }

  # Un solo mutate: asigna simulaciones + estima ingresos
  DOM_results <- DOM_results %>%
    mutate(
      !!!new_cols,
      !!sym(nom_yd) := yd_pc - (!!sym(nom_isrx) - dtx_isr0_pc),
      !!sym(nom_yc) := !!sym(nom_yd) -
        ((!!sym(nom_itbx) - itx_itb0_pc) + (sub_ele0_pc - !!sym(nom_subx))),
      !!sym(nom_yi) := yd0_pc - (!!sym(nom_itbx) - itx_itb0_pc),
      !!sym(nom_ye) := yd0_pc - (sub_ele0_pc - !!sym(nom_subx))
    )
}

# 4. Estimar compensaciones ---------------------------------------------------

DOM_results <- DOM_results %>%
  mutate(
    decyd = xtile(yd_pc, 10, wt = factor_expansion_anual)
  )

comp_precomputed <- list()

for (i in variable_values) {
  if (i == 0) next

  sim_comp_i <- escenarios$sim_com[escenarios$escenario == i]
  if (length(sim_comp_i) == 0) next

  sim_i   <- sim_com[sim_com$sim_comp == sim_comp_i, ]
  decil_i <- sim_i$decil_est
  yc_i    <- DOM_results[[paste0("yc", i, "_pc")]]
  #mask    <- DOM_results$decyd %in% seq_len(decil_i)
  mask    <- DOM_results$decyd <= decil_i & DOM_results$relation == 1 

  comp_precomputed[[as.character(i)]] <- weighted.mean(
    ((DOM_results$yc0_pc - yc_i) * DOM_results$hsize)[mask],
    w     = DOM_results$factor_expansion_anual[mask],
    na.rm = TRUE
  )
}

for (i in variable_values) {

  nombre_columna <- paste0("comp_", i)
  valor_comp     <- paste0("valorc_", i)
  sim_comp_i     <- escenarios$sim_com[escenarios$escenario == i]

  if (length(sim_comp_i) == 0 || i == 0) {
    DOM_results <- DOM_results %>%
      mutate(
        !!sym(valor_comp)     := 0,
        !!sym(nombre_columna) := 0
      )
    next
  }

  sim_i    <- sim_com[sim_com$sim_comp == sim_comp_i, ]
  metodo_i <- sim_i$metodo_com
  grupo_i  <- sim_i$grupo_com
  valor_i  <- sim_i$valor_com
  decyd_i  <- sim_i$decil_com
  icv_i    <- sim_i$icv_com
  valor_wm <- comp_precomputed[[as.character(i)]]

  DOM_results <- DOM_results %>%
    mutate(
      !!sym(valor_comp) := case_when(
        metodo_i == 1 ~ valor_wm,
        metodo_i == 2 ~ valor_i * 12 * as.numeric(relation == 1),
        .default = 0
      ),
      !!sym(nombre_columna) := case_when(
        grupo_i == 1 ~ !!sym(valor_comp) * as.numeric(relation == 1) *
                         as.numeric(dtr_supe_pc > 0),
        grupo_i == 2 ~ !!sym(valor_comp) * as.numeric(relation == 1) *
                         as.numeric(decyd <= decyd_i),
        grupo_i == 3 ~ !!sym(valor_comp) * as.numeric(relation == 1) *
                         as.numeric(icv <= icv_i),
        .default = 0
      )
    )
}


# 5. Calcular per cápita, yz y estratos ---------------------------------------

# Índices presentes en yc*_pc y en comp_* (intersección para evitar
# el error "object 'comp_X_pc' not found" cuando no existe el escenario)
indices_yc <- DOM_results %>%
  select(matches("^yc[0-9]+_pc$")) %>%
  names() %>% gsub("yc|_pc", "", .) %>% as.integer()

indices_comp <- DOM_results %>%
  select(matches("^comp_[0-9]+$")) %>%
  names() %>% gsub("comp_", "", .) %>% as.integer()

indices <- intersect(indices_yc, c(0L, indices_comp))

# Sumar comp_* por hogar y dividir por hsize en un solo paso
# usando summarise + left_join en vez de group_by + mutate (más rápido)
comp_pc <- DOM_results %>%
  group_by(hhid, trimestre) %>%
  summarise(
    across(matches("^comp_[0-9]+$"),
           ~ sum(.x, na.rm = TRUE) / first(hsize),
           .names = "{.col}_pc"),
    .groups = "drop"
  )

# Expresiones yz*_pc: yc + compensación neta respecto al base
exprs_yz <- indices %>%
  set_names(paste0("yz", ., "_pc")) %>%
  map(~ expr(
    !!sym(paste0("yc", .x, "_pc")) +
      (!!sym(paste0("comp_", .x, "_pc")) - comp_0_pc)
  ))

exprs_estrato <- indices %>%
  set_names(paste0("estrato", ., "_pc")) %>%
  map(~ expr(
    cut(
      !!sym(paste0("yz", .x, "_pc")),
      breaks = c(-Inf, estratos[1], estratos[2], estratos[3], Inf),
      labels = c(1, 2, 3, 4),
      right  = FALSE
    ) %>% as.integer()
  ))

DOM_results <- DOM_results %>%
  select(-starts_with("sitx"), -starts_with("sdtx"), -starts_with("ssub")) %>%
   select(-matches("^comp.*pc$")) %>%
  left_join(comp_pc, by = c("hhid", "trimestre")) %>%
  rename_with(~ sub("_in_pc$", "_pc", .), ends_with("in_pc")) %>%
  rename_with(~ sub("_in_hh$", "_hh", .), ends_with("in_hh")) %>%
  mutate(across(starts_with("comp") & ends_with("_pc"), ~ replace_na(., 0))) %>%  
  mutate(!!!exprs_yz) %>%
  mutate(!!!exprs_estrato) %>%  
  select(-starts_with("valor"), -matches("^comp_[0-9]+$"))

# 6. Guardar resultado --------------------------------------------------------

saveRDS(DOM_results, paste0(fdbmod, "DOM_simcompensacion.rds"))

# 7. Limpiar objetos intermedios ----------------------------------------------

rm(dom_ingdis, DOM_results, dom_sim_irenta,
   dom_sim_itbis, dom_sim_subele, esc_i, sim_i,
   comp_precomputed, comp_pc, sim_combined, hh_demo,
   decil_i, decyd_i, exprs_estrato, exprs_yz,
   indices, indices_yc, indices_comp, metodo_i)
rm(list = ls(pattern = "mask"))
rm(list = ls(pattern = "nom"))
rm(new_cols, estratos, grupo_i, i, icv_i, sim_com_esc, sim_comp_i, valor_comp,
   valor_i, valor_wm, variable_values, vs_itb, vs_sub)

