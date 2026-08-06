## Banco Mundial
## Herramienta de Microsimulación
## Country: Dominican Republic, 2026
## Authors: Maynor Cabrera, Renato Vargas
## Script: Maynor Cabrera
## 02_DOM26_irenta.R
## E-email:       mynorvc@gmail.com
##  Dependencias:  00_DOM26_Master.R
##
## Input files
## -- ENCFT 2024
## ---- DOM26_IngImponible.RDS
##
## Output files:
## -- DOM_simirenta.rds
##
## Fecha de creación:         2 Mar 2026 - 15:02
## Version:    	  			      1.0
## Fecha de modificación:     9 Abr 2026 -  7:23

# 0. Importar archivos y definir funciones -------------------------------------

## 0.1 Importar archivos -------------------------------------------------------

imponible2024 <- readRDS(paste0(fpresim, "DOM26_IngImponible.RDS"))%>%
    arrange(hhid, hhin)

sim_renta_esc <- sort(unique(escenarios$sim_renta))

sim_renta <- read_param_csv("sim_renta", fparam_csv) %>%
  filter(activo == 1) %>%
  filter(sim_inc %in% sim_renta_esc)

if (exists("sim_renta_override", inherits = FALSE) &&
    is.data.frame(sim_renta_override) && nrow(sim_renta_override) > 0) {
  sim_renta <- sim_renta_override %>%
    filter(activo == 1) %>%
    filter(sim_inc %in% sim_renta_esc)
}

## 0.2 FUNCIONES --------------------------------------------------------------
percap_t <- function(data, varlist) {
  
  for (x in varlist) {
    
    sufijo <- substr(x, nchar(x) - 1, nchar(x))
    
    # ── Caso: variable individual ("_in") 
    #     → agregar a hogar y calcular per cápita
    if (sufijo == "in") {
      col_hh <- sub("in$", "hh", x)
      col_pc <- sub("in$", "pc", x)
      
      data <- data %>%
        group_by(hhid, trimestre) %>%
        mutate(!!col_hh := sum(.data[[x]], na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(!!col_pc := .data[[col_hh]] / hsize)
    }
    
    # ── Caso: variable de hogar ("_hh") → calcular per cápita directamente
    if (sufijo == "hh") {
      col_pc <- sub("hh$", "pc", x)
      
      data <- data %>%
        mutate(!!col_pc := .data[[x]] / hsize)
    }
  }
  
  return(data)
}

# ── Función auxiliar: ISR para un escenario ─────────────────────────────────

calcular_isr_tramos <- function(data, ylab_col, isr_col, params, max_tram) {
  
  for (tramo in seq_len(max_tram)) {
    
    tram_val <- params %>% pull(paste0("tramo", tramo))
    tasa_val <- params %>% pull(paste0("tasa",  tramo))
    base_val <- params %>% pull(paste0("base",  tramo))
    
    if (is.na(tram_val)) next
    
    es_primer_tramo <- tramo == 1
    es_ultimo_tramo <- tramo == max_tram
    
    # Límite superior del tramo siguiente (NA si es el último)
    tram_post <- if (!es_ultimo_tramo) {
      params %>% pull(paste0("tramo", tramo + 1))
    } else {
      NA_real_
    }
    
    data <- data %>%
      mutate(
        !!isr_col := case_when(
          
          # Primer tramo: tasa plana sobre el ingreso
          es_primer_tramo & .data[[ylab_col]] <= tram_val
          ~ .data[[ylab_col]] * tasa_val,
          
          # Tramos intermedios: base fija + marginal sobre excedente del tramo
          !es_primer_tramo & !es_ultimo_tramo &
            .data[[ylab_col]] >  tram_val &
            .data[[ylab_col]] <= tram_post
          ~ base_val + (.data[[ylab_col]] - tram_val) * tasa_val,
          
          # Último tramo: sin límite superior
          es_ultimo_tramo & .data[[ylab_col]] > tram_val
          ~ base_val + (.data[[ylab_col]] - tram_val) * tasa_val,
          
          TRUE ~ .data[[isr_col]]
        )
      )
  }
  
  return(data)
}

# 1.Estimación de escenarios -----------------------------------

## 1.1. Limpiar base ---------------------------------

# Renombrar columna
imponible2024 <- imponible2024 %>%
  rename(dtx_isr0_pc = dtx_isra_pc)

# Extraer escenarios y número de tramos
siminc    <- sim_renta$sim_inc
tram_cols <- sim_renta %>% select(starts_with("tram")) %>% slice(1)
max_tram  <- sum(!is.na(tram_cols))

# ── Procesar cada escenario de ISR ──────────────────────────────────────────

for (esc in siminc) {
  
  if (esc == 0) next
  
  # Nombres de columnas del escenario
  educ_col <- paste0("d_educ_", esc)
  ylab_col <- paste0("ylab_",   esc)
  isr_col  <- paste0("dtx_isr", esc, "_in")
  edu_pcty <- sim_renta$educ_share[sim_renta$sim_inc == esc]
  edu_smin <- sim_renta$educ_limit[sim_renta$sim_inc == esc]
  min_impo <- sim_renta$tramo2[sim_renta$sim_inc == esc]

  params <- sim_renta %>% filter(sim_inc == esc) %>% slice(1)
  
  # Crear columnas base (deducción, ingreso neto, ISR inicial)
  if (!ylab_col %in% names(imponible2024)) {
    imponible2024 <- imponible2024 %>%
      mutate(
        !!educ_col := pmin(Ilab_isr12a * edu_pcty,
                           min_impo * edu_smin,
                           gasto_educ_imp),
        !!ylab_col := pmax(0, Ilab_isr12a - .data[[educ_col]]),
        !!isr_col  := 0
      )
  }
  
  # Calcular ISR por tramos y agregar per cápita
  imponible2024 <- imponible2024 %>%
    calcular_isr_tramos(ylab_col, isr_col, params, max_tram) %>%
    percap_t(isr_col)
  
}

imponible2024 %>%
  select(starts_with("dtx") & ends_with("_pc")) %>%
  fsum(w = imponible2024$factor_expansion_anual, na.rm = TRUE) %>%
  comma()

## Estimación base = hard-coded
imponible2024 <- imponible2024 %>%
  mutate(rangoi = 
           case_when (
             Ilab_isr12a <= 416220 ~ 1,
             Ilab_isr12a > 416220  & Ilab_isr12a <= 624329 ~ 2,
             Ilab_isr12a > 624329  & Ilab_isr12a <= 867123 ~ 3,
             Ilab_isr12a > 867123  & Ilab_isr12a <= 4800000 ~ 4,
             Ilab_isr12a > 4800000 ~ 5
           ))

dom_sim_irenta <- imponible2024 %>%
  select(-starts_with("ylab"), -starts_with("d_educ"), -rangoi, 
                      -gasto_educ_imp)

imponible2024 %>%
  select(starts_with("dtx") & ends_with("_in"), rangoi) %>%
  group_by(rangoi) %>%
  fsum(w = imponible2024$factor_expansion_anual, na.rm = TRUE) 


## 2. Guardar información  -----------------------------------------------------

# grabar información
saveRDS(dom_sim_irenta, paste0(fdbmod, "DOM_simirenta.rds"))

## eliminar datos de memoria
vars_to_remove <- c(
  "dom_sim_irenta", "imponible2024", "educ_col",
  "isr_col", "max_tram", "ylab_col", "calcular_isr_tramos",
  "esc", "tram_cols", "edu_pcty", "edu_smin", "min_impo"
)

rm(list = intersect(vars_to_remove, ls()))


