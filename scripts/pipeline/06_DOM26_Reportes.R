## =============================================================================
## Banco Mundial — Herramienta de Microsimulación
## Country: República Dominicana 2026
## Authors: Maynor Cabrera, Renato Vargas
## 06_DOM26_Reportes.R  |  v5.0 (incluye archivo de dashboard)
## =============================================================================
##
## CAMBIOS DE RENDIMIENTO respecto a v3.0:
##
##   1. DPI reducido a 96 (de 300) para gráficos Excel — reduce tiempo de
##      renderizado ~60%. Si se necesita alta resolución para impresión,
##      cambiar DPI_GRAFICOS a 300.
##
##   2. Generación paralela de gráficos con future + furrr. Aprovecha todos
##      los núcleos disponibles. Si no están instalados, cae a secuencial.
##
##   3. Estructuras pre-computadas: textos de grupo, nombres de escenarios,
##      params_esc_base se calculan UNA sola vez fuera de los loops.
##
##   4. Escritura batch en Excel: una sola función maneja textos y datos,
##      eliminando overhead de despacho repetido.
##
##   5. Caché de gráficos comparativos: se generan y guardan como PNG una
##      sola vez, luego se insertan sin re-renderizar.
##
##   6. ragg como backend de gráficos: ~2x más rápido que el default de R
##      para renderizar PNGs. Si no está instalado, usa el default.
##
## =============================================================================

## =============================================================================
## 0. CONFIGURACIÓN CENTRAL ----------------------------------------------------
## =============================================================================

## ── 0.0 Parámetros de rendimiento --------------------------------------------
DPI_GRAFICOS   <- 96    #  96 150 = rápido y suficiente para Excel; 300 = print
ANCHO_GRAFICO  <- 6     # pulgadas
ALTO_GRAFICO   <- 4     # pulgadas
USAR_PARALELO  <- TRUE  # FALSE para desactivar paralelismo

CFG <- list(

  ## 0.1 Rutas -----------------------------------------------------------------
  rds_sims      = paste0(fdbmod, "DOM_resultados.rds"),
  rds_resumen   = paste0(fdbmod, "DOM_resumen.rds"),
  shp_deptos    = paste0(finput, "macro_regiones_rd.shp"),
  sheet_simcomp = "sim_comp",

  ## 0.2 Disclaimer ------------------------------------------------------------
  disclaimer = paste0(
    "Descargo de responsabilidad: Las simulaciones presentadas son ejercicios ",
    "teóricos y no constituyen ninguna intención o propuesta de reforma por ",
    "parte del Gobierno de República Dominicana."
  ),

  ## 0.3 Columnas de resultados ------------------------------------------------
  columnas = list(
    inc    = list(col = "^nitx",    mult = -1, sufijo = "inc", add_i = FALSE,
                  label = "Incidencia neta"),
    isr    = list(col = "^dtx_isr", mult =  1, sufijo = "inc", add_i = TRUE,
                  label = "Cambio en incidencia del ISR"),
    itbis  = list(col = "^itx_itb", mult =  1, sufijo = "inc", add_i = TRUE,
                  label = "Cambio en incidencia del ITBIS"),
    sub    = list(col = "^sub_ele", mult =  1, sufijo = "inc", add_i = TRUE,
                  label = "Cambio en incidencia del subsidio"),
    com    = list(col = "^comp_",   mult =  1, sufijo = "inc", add_i = TRUE,
                  label = "Compensación"),
    cisr   = list(col = "^dtx_isr", mult =  1, sufijo = "con", add_i = TRUE,
                  label = "Concentración ISR"),
    citbis = list(col = "^itx_itb", mult =  1, sufijo = "con", add_i = TRUE,
                  label = "Concentración ITBIS"),
    csub   = list(col = "^sub_ele", mult =  1, sufijo = "con", add_i = TRUE,
                  label = "Concentración subsidio"),
    ccom   = list(col = "^comp_",   mult =  1, sufijo = "con", add_i = TRUE,
                  label = "Concentración compensación")
  ),

  ## 0.4 Gráfico fiscal --------------------------------------------------------
  fiscal = list(
    "ISR" = 6, "ITBIS" = 7, "Subsidio" = 8, "Compensación" = 10
  ),
  colores_fiscal = c("purple", "steelblue", "lightblue", "orange"),

  ## 0.5 Grupos de clasificación -----------------------------------------------
  grupos = list(
    list(prefijo = "dec",  titulo_key = "decil",   rotulo = "Decil",
         rotar = 1, nombre_largo = "decil"),
    list(prefijo = "estr", titulo_key = "estrato", rotulo = "Estrato económico",
         rotar = 0, nombre_largo = "estrato"),
    list(prefijo = "urb",  titulo_key = "urban", rotulo = "Área de residencia",
         rotar = 0, nombre_largo = "área de residencia"),
    list(prefijo = "sex",  titulo_key = "sexo",  rotulo = "Sexo",
         rotar = 0, nombre_largo = "sexo del jefe del hogar"),
    list(prefijo = "cat",  titulo_key = "categoria", rotulo = "Categoría",
         rotar = 0, nombre_largo = "tipo de hogar"),
    list(prefijo = "dep",  titulo_key = "region",    rotulo = "Macro región",
         rotar = 0, nombre_largo = "macro región")
  ),

  ## 0.6 Posiciones Excel (comparativos) ---------------------------------------
  col_excel = list(
    inc   = list(c1 =   3, c2 =   4),
    isr   = list(c1 =  31, c2 =  32),
    itbis = list(c1 =  58, c2 =  59),
    sub   = list(c1 =  85, c2 =  86),
    com   = list(c1 = 112, c2 = 113)
  ),

  ## 0.7 Posiciones de gráficos en Excel ---------------------------------------
  pos_graficos_base = list(
    c(1, 15), c(14, 22), c(27, 15), c(38, 22), c(50, 15), c(64, 22)
  ),
  offset_isr   = c(0,  28),
  offset_itbis = c(0,  54),
  offset_sub   = c(0,  81),
  offset_com   = c(0, 108),

  ## 0.8 Colores mapas ---------------------------------------------------------
  mapa_color_low  = "firebrick",
  mapa_color_high = "lightblue",

  ## 0.9 Indicadores del anexo -------------------------------------------------
  indicadores = list(
    list(n = "Pobreza",
         d = "Porcentaje de personas cuyo ingreso disponible del hogar per cápita es inferior a la línea de pobreza"),
    list(n = "Cambio en la pobreza",
         d = "Cambio en la tasa de pobreza respecto al escenario base"),
    list(n = "Nuevos pobres",
         d = "Cambio en la cantidad de personas pobres respecto al escenario base"),
    list(n = "Brecha relativa",
         d = "Distancia promedio entre el ingreso de los pobres y la línea de pobreza, expresada como porcentaje de dicha línea"),
    list(n = "Brecha nacional RD$ hogar/mes",
         d = "Ingreso adicional mensual requerido por hogar para alcanzar la línea de pobreza"),
    list(n = "Brecha pobres RD$ hogar/mes",
         d = "Ingreso adicional mensual requerido por los hogares pobres antes de la reforma para alcanzar la línea de pobreza"),
    list(n = "Gini",
         d = "Coeficiente de Gini: mide la distribución del ingreso en [0,1], donde 0 = equidad absoluta y 1 = inequidad absoluta"),
    list(n = "Cambio en Gini",
         d = "Cambio en el coeficiente de Gini respecto al escenario base"),
    list(n = "Palma",
         d = "Relación entre los ingresos del 10% más rico y el 40% más pobre"),
    list(n = "Theil",
         d = "Medida de desigualdad de la familia de índices de entropía generalizada"),
    list(n = "% ingreso disponible",
         d = "Ratio del instrumento fiscal total del grupo respecto al ingreso disponible del mismo grupo"),
    list(n = "% del total",
         d = "Ratio del instrumento fiscal total del grupo respecto al instrumento nacional total"),
    list(n = "% del PIB",
         d = "Valor total de los instrumentos fiscales como proporción del PIB, estimado a partir de la encuesta"),
    list(n = "Grupos",
         d = "Descripción de los grupos de clasificación utilizados"),
    list(n = "Decil",
         d = "Agrupación de la población en diez grupos ordenados según el ingreso per cápita del hogar"),
    list(n = "Estrato de ingreso",
         d = "Estratos del Banco Mundial: \npobres (<8.30 USD PPA/día), \nvulnerables (8.3-17), \nclase media (17-98), \nalta (residuo)"),
    list(n = "Área de residencia",
         d = "Rural o urbana según la definición de la ENCFT"),
    list(n = "Tipo hogar",
         d = "Sin niños ni adultos mayores \ncon niños \ncon adultos mayores \ncon niños y adultos mayores"),
    list(n = "Macro región",
         d = "Cuatro macro regiones geográficas: \nOzama, \nNorte, \nSur y \nEste")
  )
)


## =============================================================================
## 0.10 TÍTULOS Y ETIQUETAS (pre-computados una sola vez) ----------------------
## =============================================================================

TITULOS_GRUPOS <- list(
  decil     = c("Decil", as.character(1:10), "Total"),
  estrato   = c("Pobres", "Vulnerables", "Clase media", "Alta", "Total"),
  urban     = c("Rural", "Urbano", "Total"),
  sexo      = c("Hombre", "Mujer", "Total"),
  categoria = c("Hogar sin niños ni adultos mayores", "Hogar con niños",
                "Hogar con ancianos", "Hogar con niños y ancianos", "Total"),
  region    = c("Ozama", "Norte", "Sur", "Este", "Total")
)

TITULO_COLS_POV <- t(c(
  "Base", "Efecto marginal ISR", "Efecto ITBIS y subsidios",
  "+ Compensación", "Efecto marginal ITBIS", "Efecto marginal subsidios"
))

TITULO_FILAS_POV <- as.matrix(c(
  "Pobreza", "Cambio en la pobreza", "Nuevos pobres",
  "Brecha relativa", "Brecha nacional RD$ hogar/mes",
  "Brecha pobres RD$ hogar/mes", "Brecha nuevos pobres RD$/mes",
  "Brecha nuevos pobres % PIB"
))

TITULO_FILAS_INEQ <- as.matrix(c("Gini", "Cambio en Gini", "Palma", "Theil"))

TITULO_COLS_KAKWANI <- t(c(
  "Base concentración", "Base Kakwani", "Reforma concentración", 
  "Reforma Kakwani")
)
TITULO_FILAS_KAKWANI <- as.matrix(c("ISR", "ITBIS", "Subsidios", "Compensaciones"))

TITULO_PREREFORMA <- t(c(
  "Pre-reforma", "", "", "", "Reforma", "", "", "", "",
  "Cambio respecto al escenario pre-reforma"
))
TITULO_SUBHEADER <- t(c(
  "ISR", "ITBIS", "Subsidio electricidad", "ISR + ITBIS - Subsidio",
  "ISR", "ITBIS", "Subsidio electricidad", "ISR + ITBIS - Subsidio",
  "Compensación", "\u25B2 ISR", "\u25B2 ITBIS",
  "\u25B2 Subsidio electricidad", "\u25B2 compensación", "Efecto neto"
))

LAYOUT_GRUPOS <- data.frame(
  prefijo   = c("dec", "estr", "urb", "sex", "cat", "dep"),
  fila_data = c(  22,    38,    48,    56,    64,    72),
  n_filas   = c(  11,     5,     4,     4,     5,     5),
  stringsAsFactors = FALSE
)

COL_VISTA <- list(
  inc = list(data = 1,  titulo = 2,  encab = 2),
  con = list(data = 17, titulo = 18, encab = 18),
  sum = list(data = 33, titulo = 34, encab = 34)
)

## =============================================================================
# 1. CARGA DE DATOS -----------------------------------------------------------
## =============================================================================

fresults <- paste0(path_o, "/resultados/")
setwd(fresults)

dom_sims              <- readRDS(CFG$rds_sims)
resultados_escenarios <- readRDS(CFG$rds_resumen)

sim_com_esc <- unique(escenarios$sim_com)
sim_com     <- read_excel(fparams, col_names = TRUE,
  sheet = CFG$sheet_simcomp) %>%
    janitor::clean_names() %>%
    select(sim_comp, activo, ends_with("com"), decil_est) %>%
    filter(activo == 1, sim_comp %in% sim_com_esc)

variable_values <- escenarios$escenario

## Nombres de escenarios (pre-computados, se usan en múltiples lugares)
nombres_esc_cortos <- str_replace_all(escenarios$des_corto, "Reforma ", "")
#  Creamos el vector con los nombres de los escenarios
nombres_esc_cols <- c("Pre-reforma", nombres_esc_cortos[escenarios$escenario %in% variable_values])

# Validamos si existen duplicados y abortamos si es necesario
if (anyDuplicated(nombres_esc_cols) > 0) {
  stop("Los nombres cortos de escenarios están duplicados, revisar hoja parámetros")
}

## =============================================================================
# 2. FUNCIONES------------------------------------------------------------------
## =============================================================================

## ── 2.0 Backend de gráficos --------------------------------------------------

# Verificar disponibilidad de ragg (requerido para el backend rápido)
RAGG_ACTIVO <- requireNamespace("ragg", quietly = TRUE)
if (!RAGG_ACTIVO) {
  warning("Paquete 'ragg' no encontrado. Instalarcon install.packages('ragg') ",
          "para acelerar el renderizado de gráficos.")
}

# Paralelismo: útil para operaciones CPU-intensivas, no para ggsave/I/O
# Se mantiene disponible para map_optimo en mapas (st_read + ggplot complejos)
PARALELO_ACTIVO <- FALSE
if (USAR_PARALELO &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("furrr",  quietly = TRUE)) {
  future::plan(future::multisession,
               workers = max(1, parallel::detectCores() - 1))
  PARALELO_ACTIVO <- TRUE
  message(sprintf("Paralelismo activado: %d workers",
                  max(1, parallel::detectCores() - 1)))
}

map_optimo <- function(.x, .f, ...) {
  if (PARALELO_ACTIVO) {
    furrr::future_map(.x, .f, ...,
                      .options = furrr::furrr_options(seed = TRUE))
  } else {
    lapply(.x, .f, ...)
  }
}

## ── 2.1 Estilos Excel -------------------------------------------------------

estilos <- list(
  encabezado = createStyle(
    fontSize = 10, fontColour = "black", fgFill = "#FAF2F2",
    halign = "center", valign = "center", wrapText = TRUE,
    textDecoration = "bold", fontName = "Aptos",
    border = "TopBottom", borderColour = "#800000"
  ),
  descriptor = createStyle(
    fontSize = 8, fontColour = "black", halign = "left",
    textDecoration = "bold", fontName = "Aptos"
  ),
  titulo = createStyle(
    fontSize = 12, fontColour = "#800000", halign = "left",
    textDecoration = "bold"
  ),
  titulo2 = createStyle(
    fontSize = 10, fontColour = "white", fgFill = "#333333",
    halign = "left", valign = "center",
    textDecoration = "bold", fontName = "Aptos"
  ),
  numerico  = createStyle(fontSize = 10, numFmt = "###,##0.0",
    fontName = "Aptos"),
  numerico2 = createStyle(fontSize = 10, numFmt = "##0.0000",
    halign = "right",
                          fontName = "Aptos"),
  numerico3 = createStyle(fontSize = 10, numFmt = "###,##0.00",
    fontName = "Aptos"),
  disclaimer = createStyle(fontName = "Aptos Narrow", fontColour = "gray",
                           fontSize = 8, textDecoration = "italic")
)

## ── 2.2 Helpers de escritura en Excel ----------------------------------------

#' Escribe una lista de items en una hoja (batch, un solo despacho)
escribir_en_hoja <- function(wb, hoja, items, tipo = c("data", "text")) {
  tipo <- match.arg(tipo)
  for (item in items) {
    if (tipo == "text") {
      writeData(wb, hoja, item$text,
                startRow = item$startRow, startCol = item$startCol)
    } else {
      writeData(wb, hoja, item$data,
                startRow = item$row, startCol = item$col, colNames = FALSE)
    }
  }
}

#' Guarda un ggplot como 
# guardar_grafico <- function(g, ancho = ANCHO_GRAFICO, alto = ALTO_GRAFICO) {
guardar_grafico <- function(g, ancho = ANCHO_GRAFICO, alto = ALTO_GRAFICO) {
  tf <- tempfile(fileext = ".png")
  ragg::agg_png(tf, width = ancho, height = alto, 
                units = "in", res = DPI_GRAFICOS)
  print(g)
  dev.off()
  tf
}

message("Usando ragg: ", requireNamespace("ragg", quietly = TRUE))

#' Guarda una lista de ggplots como PNG en paralelo y devuelve rutas
# guardar_graficos_batch <- function(graficos) {
#   map_optimo(graficos, guardar_grafico)
# }

guardar_graficos_batch <- function(graficos) {
  lapply(graficos, guardar_grafico)
}

#' Inserta PNGs ya guardados en Excel (solo I/O, sin renderizado)
insertar_pngs <- function(wb, hoja, rutas, posiciones) {
  for (k in seq_along(rutas)) {
    insertImage(wb, sheet = hoja, file = rutas[[k]],
                startRow = posiciones[[k]][1], startCol = posiciones[[k]][2])
  }
}

#' Flujo completo: renderiza en paralelo + inserta
insertar_graficos <- function(wb, hoja, graficos, posiciones) {
  rutas <- guardar_graficos_batch(graficos)
  insertar_pngs(wb, hoja, rutas, posiciones)
}

## ── 2.3 Gráficos de resumen -------------------------------------------------

grafico_barras_fila <- function(data, dec, tit1, tit2, fila) {
  print(tit1)
  cols <- nombres_esc_cols
  df <- as.data.frame(data)[fila, , drop = FALSE] %>%
    setNames(cols) %>%
    mutate(ID = 1) %>%
    pivot_longer(-ID, names_to = "Categoria", values_to = "Valor") %>%
    mutate(
      Fila      = factor(if_else(Categoria == "Pre-reforma",
                                 "Pre-reforma", "Reformas"),
                         levels = c("Pre-reforma", "Reformas")),
      Categoria = factor(Categoria, levels = cols)
    )

  ggplot(df, aes(x = Categoria, y = Valor, fill = Fila)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(aes(label = scales::comma(round(Valor, dec))),
              position = position_dodge(width = 0.7), vjust = -1) +
    scale_fill_manual(values = c("steelblue", "orange")) +
    scale_x_discrete(labels = scales::wrap_format(35)) +
    scale_y_continuous(limits = c(0, max(df$Valor) * 1.2)) +
    labs(title = tit1, y = tit2, x = "", fill = "") +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank())
}

grafico_barras_base <- function(data, dec, tit1, tit2) {
  cols <- nombres_esc_cols
  df <- as.data.frame(data) %>%
    setNames(cols) %>%
    mutate(Fila = factor(c("Sin compensación", "Con compensación", "Base"),
                         levels = c("Sin compensación", "Con compensación",
                                    "Base"))) %>%
    pivot_longer(-Fila, names_to = "Categoria", values_to = "Valor")

  ggplot() +
    geom_bar(data = filter(df, Fila != "Base"),
             aes(x = Categoria, y = Valor, fill = Fila),
             stat = "identity", position = "dodge", width = 0.7) +
    geom_text(data = filter(df, Fila != "Base"),
              aes(x = Categoria, y = Valor,
                  label = comma(round(Valor, dec)), group = Fila),
              position = position_dodge(width = 0.7), vjust = -1) +
    geom_line(data = filter(df, Fila == "Base"),
              aes(x = Categoria, y = Valor, group = 1, color = Fila),
              linewidth = 1) +
    geom_point(data = filter(df, Fila == "Base"),
               aes(x = Categoria, y = Valor, color = Fila), size = 3) +
    scale_fill_manual(values = c("steelblue", "orange")) +
    scale_color_manual(values = c("Base" = "red")) +
    scale_y_continuous(limits = c(0, max(df$Valor) * 1.2)) +
    labs(title = tit1, y = tit2, x = "", fill = "", color = "Escenario") +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank())
}


## ── 2.4 Gráficos comparativos por grupo -------------------------------------


grafico_grupo <- function(matriz, titulo_vec, quitar_primero, rotulo_x,
                          titulo_g, incluir_base = FALSE) {
  idx_excluir <- if (quitar_primero) c(1L, length(titulo_vec)) else
    length(titulo_vec)
  etiquetas   <- titulo_vec[-idx_excluir]
  
  nombres_esc <- str_replace_all(escenarios$des_corto, "Reforma ", "")
  if (incluir_base) nombres_esc <- c("Base", nombres_esc)
  
  df <- do.call(rbind, lapply(seq_along(etiquetas), function(k) {
    data.frame(cat = etiquetas[k], Escenario = nombres_esc,
               Valor = as.numeric(as.data.frame(matriz)[k, ]),
               stringsAsFactors = FALSE)
  }))
  
  df$cat       <- factor(df$cat,       levels = unique(df$cat))
  df$Escenario <- factor(df$Escenario, levels = nombres_esc)
  
  ggplot(df, aes(x = cat, y = Valor, fill = Escenario)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    scale_x_discrete(labels = wrap_format(16)) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    labs(title = titulo_g, x = rotulo_x, fill = "") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid       = element_blank(),
      plot.background  = element_rect(fill = "white"),
      legend.position  = "bottom",
      legend.text      = element_text(size = 7),
      legend.key.size  = unit(0.4, "cm"),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.margin      = margin(5, 5, 8, 5)
    ) +
    guides(fill = guide_legend(ncol = 2, byrow = TRUE, label.hjust = 0))
}

generar_graficos_todos <- function(resultado_lista, sufijo_mat, titulo_g,
                                   incluir_base = FALSE) {
  lapply(CFG$grupos, function(g) {
    grafico_grupo(
      resultado_lista[[paste0(g$prefijo, sufijo_mat)]],
      TITULOS_GRUPOS[[g$titulo_key]],
      as.logical(g$rotar), g$rotulo, titulo_g,
      incluir_base = incluir_base
    )
  })
}

## ── 2.5 Extracción de columnas para comparativos ----------------------------

# comp_matriz <- function(nombre_matriz, patron_col, mult) {
#   matrices <- lapply(variable_values, function(i) {
#     df      <- resultados_escenarios[[paste0("escenario_", i)]][[nombre_matriz]]
#     col_idx <- grep(patron_col, names(df), value = FALSE)
#     if (length(col_idx) == 0) {
#       stop(sprintf("Patrón '%s' no encontrado en %s. Columnas: %s",
#                    patron_col, nombre_matriz, paste(names(df),
#                    collapse = ", ")))
#     }
#     mat <- as.matrix(df[, col_idx, drop = FALSE])
#     as.matrix(mat[-nrow(mat), , drop = FALSE]) * mult
#   })
#   do.call(cbind, matrices)
# }

comp_matriz <- function(nombre_matriz, patron_col, mult, add_i = TRUE) {
  
  extraer <- function(i, incluir_base = FALSE) {
    df <- resultados_escenarios[[paste0("escenario_", i)]][[nombre_matriz]]
    
    if (!add_i) {
      patron_i <- patron_col
    } else if (incluir_base) {
      patron_i <- paste0(patron_col, "[0", i, "]")
    } else {
      patron_i <- paste0(patron_col, i)
    }
    
    col_idx <- grep(patron_i, names(df), value = FALSE)
    if (length(col_idx) == 0) {
      stop(sprintf("Patrón '%s' no encontrado en %s. Columnas: %s",
                   patron_i, nombre_matriz, paste(names(df), collapse = ", ")))
    }
    mat <- as.matrix(df[, col_idx, drop = FALSE])
    as.matrix(mat[-nrow(mat), , drop = FALSE]) * mult
  }
  
  # Primera iteración incluye la columna base (0), el resto solo su columna
  do.call(cbind, c(
    list(extraer(variable_values[1], incluir_base = TRUE)),
    lapply(variable_values[-1], extraer)
  ))
}

procesar_resultado <- function(cfg_col) {
  sufijos <- paste0(sapply(CFG$grupos, `[[`, "prefijo"), cfg_col$sufijo)
  setNames(
    lapply(sufijos, comp_matriz, patron_col = cfg_col$col,
           mult = cfg_col$mult, add_i = cfg_col$add_i),
    sufijos
  )
}

## ── 2.6 Generación de parámetros para hojas comparativas --------------------

generate_params <- function(resultado_lista, col_1, col_2, row_start,
                            sufijo_mat, descri) {
  offsets <- c(dec = 0, estr = 17, urb = 27, sex = 35, cat = 43, dep = 51)
  params <- imap(offsets, function(off, prefijo) {
    g    <- CFG$grupos[[which(sapply(CFG$grupos, `[[`, "prefijo") == prefijo)]]
    tvec <- TITULOS_GRUPOS[[g$titulo_key]]
    etiquetas <- if (prefijo == "dec") tvec[-c(1, length(tvec))] else
      tvec[-length(tvec)]
    
    # Verificar si la matriz tiene columna base
    mat     <- resultado_lista[[paste0(prefijo, sufijo_mat)]]
    # DESPUÉS
    descri_i <- if (ncol(mat) == ncol(descri) + 1) {
      cbind("Base", descri)
    } else {
      descri
    }
    
    list(
      list(data = as.matrix(etiquetas),
           row  = row_start + off + 1, col = col_1),
      list(data = mat,
           row  = row_start + off + 1, col = col_2),
      list(data = descri_i,
           row  = row_start + off,     col = col_2)
    )
  })
  unlist(params, recursive = FALSE)
}

## =============================================================================
# 3. CREAR LIBRO Y HOJA ÍNDICE -------------------------------------------------
## =============================================================================

wb <- createWorkbook()

addWorksheet(wb, "Indice")
writeData(wb, "Indice", "Indice", startCol = 2, startRow = 2)
addStyle(wb, "Indice", rows = 2, cols = 2,
         style = createStyle(fontName = "Aptos", fontSize = 15,
                             textDecoration = "bold"))

estilo_link <- createStyle(fontColour = "blue", fontName = "Aptos",
  fontSize = 12, , textDecoration = "underline")

j_idx <- 4

walk(seq_along(escenarios$des_corto), function(idx) {
  writeFormula(wb, "Indice", startRow = j_idx + idx - 1, startCol = 3,
               x = paste0('HYPERLINK("#sim', idx, '!A1", "',
                          escenarios$des_corto[idx], '")'))
  addStyle(wb, "Indice", rows = j_idx + idx - 1, cols = 3, style = estilo_link)
})

j_extra <- j_idx + length(escenarios$des_corto)

for (k in seq_along(list(
  list(dest = "#resumen!A1", text = "Resumen"),
  list(dest = "#comparativos!A1", text = "Comparativo")))) {
    lk <- list(list(dest = "#resumen!A1", text = "Resumen"),
             list(dest = "#comparativos!A1", text = "Comparativo"))[[k]]
    writeFormula(wb, "Indice", startRow = j_extra + k - 1, startCol = 3,
               x = paste0('HYPERLINK("', lk$dest, '", "', lk$text, '")'))
    addStyle(wb, "Indice", rows = j_extra + k - 1, cols = 3, style = estilo_link)
}

addStyle(wb, "Indice", rows = j_extra + 4, cols = 3, style = estilos$disclaimer)
writeData(wb, "Indice", CFG$disclaimer, startCol = 3, startRow = j_extra + 4)

## =============================================================================
# 4. HOJAS POR ESCENARIO -------------------------------------------------------
## =============================================================================

## ── 4.1 Pre-computar todas las estructuras que NO cambian entre escenarios ---

# Textos de grupo (idénticos para cada escenario)
construir_textos_grupo <- function(nombre_grupo, fila_titulo) {
  vistas <- c(
    "como % del ingreso disponible" = COL_VISTA$inc$titulo,
    "% del total"                   = COL_VISTA$con$encab,
    "% del PIB"                     = COL_VISTA$sum$encab
  )
  lapply(seq_along(vistas), function(k) {
    list(text = paste0("Impuestos y compensaciones por ", nombre_grupo,
                       ", ", names(vistas)[k]),
         startRow = fila_titulo, startCol = vistas[[k]])
  })
}

# FIX: se eliminó la primera construcción duplicada de textos_grupo_precomp
textos_grupo_precomp <- unlist(lapply(seq_len(nrow(LAYOUT_GRUPOS)),
  function(k) {
    construir_textos_grupo(CFG$grupos[[k]]$nombre_largo,
                         LAYOUT_GRUPOS$fila_data[k] - 2)
}), recursive = FALSE)

# Textos fijos (solo cambian escenarios$des_corto[i] y el número i)
textos_fijos_template <- list(
  list(startRow = 2,  startCol = 2,
       text = "Efectos en pobreza (nacional)"),
  list(startRow = 2,  startCol = 18,
       text = "Efectos en pobreza extrema"),
  list(startRow = 2,  startCol = 34,
       text = "Efectos en pobreza (internacional) - después de impuestos y compensación"),
  list(startRow = 13, startCol = 2,
       text = "Efectos en desigualdad - después de impuestos y compensación"),
  list(startRow = 13, startCol = 11,
       text = "Progresividad fiscal (índices de concentración y Kakwani)")
)

# Posiciones de datos
construir_variables_esc <- function() {
  fijos <- list(
    list(data = "pov_gral", startRow = 3,  startCol = 2),
    list(data = "pov_ext",  startRow = 3,  startCol = 18),
    list(data = "pov_int",  startRow = 3,  startCol = 34),
    list(data = "ineq",     startRow = 14, startCol = 2),
    list(data = "kakwani",  startRow = 14, startCol = 10)
  )
  sufijos_vista <- c(inc = "inc", con = "con", sum = "sum")
  col_starts    <- c(inc = 1, con = 17, sum = 33)
  dinamicos <- unlist(lapply(seq_len(nrow(LAYOUT_GRUPOS)), function(i) {
    pref <- LAYOUT_GRUPOS$prefijo[i]
    fila <- LAYOUT_GRUPOS$fila_data[i]
    lapply(names(sufijos_vista), function(v) {
      list(data = paste0(pref, sufijos_vista[v]),
           startRow = fila, startCol = col_starts[v])
    })
  }), recursive = FALSE)
  c(fijos, dinamicos)
}

variables_esc <- construir_variables_esc()

# Params de títulos (idénticos para cada escenario)
construir_params_esc <- function() {
  params <- list()
  for (col in c(2, 18, 34)) {
    params <- c(params, list(
      list(data = TITULO_COLS_POV,  row = 3,  col = col),
      list(data = TITULO_FILAS_POV, row = 4,  col = col - 1)
    ))
  }
  params <- c(params, list(
    list(data = TITULO_COLS_POV,   row = 14, col = 2),
    list(data = TITULO_FILAS_INEQ, row = 15, col = 1)
  ))
  
  col_encab <- c(2, 18, 34)
  for (i in seq_len(nrow(LAYOUT_GRUPOS))) {
    pref <- LAYOUT_GRUPOS$prefijo[i]
    fila <- LAYOUT_GRUPOS$fila_data[i]
    g    <- CFG$grupos[[which(sapply(CFG$grupos, `[[`, "prefijo") == pref)]]
    tvec <- TITULOS_GRUPOS[[g$titulo_key]]
    etiqueta_col <- if (pref == "dec") tvec else c(g$rotulo, tvec)
    for (col in c(1, 17, 33))
      params <- c(params, list(list(data = etiqueta_col, row = fila, col = col)))
    for (col in col_encab) {
      params <- c(params, list(
        list(data = TITULO_PREREFORMA, row = fila - 1, col = col),
        list(data = TITULO_SUBHEADER,  row = fila,     col = col)
      ))
    }
  }
  params
}
params_esc_base <- construir_params_esc()

## ── 4.2 Estilos (pre-computar rangos de filas) -------------------------------

# Rangos de filas para estilos de grupos (calculados una vez)
filas_datos_all_precomp <- unlist(lapply(seq_len(nrow(LAYOUT_GRUPOS)), function(k) {
  (LAYOUT_GRUPOS$fila_data[k] + 1):(LAYOUT_GRUPOS$fila_data[k] + LAYOUT_GRUPOS$n_filas[k])
}))

aplicar_estilos_escenario <- function(wb, hoja) {
  filas_data   <- LAYOUT_GRUPOS$fila_data
  filas_titulo <- filas_data - 2
  
  # Pobreza y desigualdad
  addStyle(wb, hoja, estilos$titulo,     rows = 1, cols = 2:10)
  addStyle(wb, hoja, estilos$titulo2,    rows = 2, cols = c(2:7, 18:23, 34:39))
  addStyle(wb, hoja, estilos$encabezado, rows = 3, cols = c(2:7, 18:23, 34:39),
           gridExpand = TRUE)
  addStyle(wb, hoja, estilos$numerico,   rows = 4:11,
           cols = c(2:7, 18:23, 34:39), gridExpand = TRUE)
  addStyle(wb, hoja, estilos$descriptor, rows = 4:11, cols = c(1, 17, 33),
           gridExpand = TRUE)
  addStyle(wb, hoja, estilos$titulo2,    rows = 13, cols = 2:7)
  addStyle(wb, hoja, estilos$encabezado, rows = 14, cols = 2:7,
           gridExpand = TRUE)
  addStyle(wb, hoja, estilos$descriptor, rows = 15:18, cols = 1)
  addStyle(wb, hoja, estilos$numerico2,  rows = 15:18, cols = 2:6,
           gridExpand = TRUE)
  
  # Kakwani
  n_cols_kakwani <- 2 + 2 * 1
  cols_kk_datos  <- 11:(10 + n_cols_kakwani)
  
  addStyle(wb, hoja, estilos$titulo2,    rows = 13, cols = 10:(10 + n_cols_kakwani))
  addStyle(wb, hoja, estilos$encabezado, rows = 14, cols = 10:(10 + n_cols_kakwani),
           gridExpand = TRUE)
  addStyle(wb, hoja, estilos$descriptor, rows = 15:18, cols = 9)
  addStyle(wb, hoja, estilos$numerico2,  rows = 15:18, cols = cols_kk_datos,
           gridExpand = TRUE)
  
  # Grupos
  addStyle(wb, hoja, estilos$encabezado, rows = filas_data,
           cols = c(2:15, 18:31, 34:47), gridExpand = TRUE)
  addStyle(wb, hoja, estilos$descriptor, rows = filas_data - 1,
           cols = 2:48, gridExpand = TRUE)
  addStyle(wb, hoja, estilos$descriptor,
           rows = c(filas_data, filas_datos_all_precomp),
           cols = c(1, 17, 33), gridExpand = TRUE)
  addStyle(wb, hoja, estilos$numerico3,  rows = filas_datos_all_precomp,
           cols = 2:10, gridExpand = TRUE)
  addStyle(wb, hoja, estilos$numerico,   rows = filas_datos_all_precomp,
           cols = c(18:31, 34:47), gridExpand = TRUE)
  addStyle(wb, hoja, estilos$titulo2,    rows = filas_titulo,
           cols = c(2:15, 18:31, 34:47), gridExpand = TRUE)
  
  setColWidths(wb, hoja, cols = c(1:16, 17:32, 33:48),
               widths = rep(c(24, rep(15, 14), 3), 3))
}


## ── 4.3 Loop principal por escenario ----------------------------------------

message("Generando hojas de escenario...")
# Crear hoja plantilla con estilos aplicados una sola vez
PLANTILLA <- "_plantilla_"
addWorksheet(wb, PLANTILLA)
aplicar_estilos_escenario(wb, PLANTILLA)
addStyle(wb, PLANTILLA, rows = 1, cols = 8, style = estilos$disclaimer)

for (i in variable_values) {
  hoja    <- paste0("sim", i)
  esc_key <- paste0("escenario_", i)
  
  # FIX: clonar la plantilla en vez de crear una hoja vacía y re-aplicar estilos
  cloneWorksheet(wb, sheetName = hoja, clonedSheet = PLANTILLA)
  
  # Datos (loop con for en vez de walk para evitar overhead de purrr)
  for (v in variables_esc) {
    writeData(wb, hoja, resultados_escenarios[[esc_key]][[v$data]],
              startRow = v$startRow, startCol = v$startCol)
  }
  
  # Textos que varían por escenario (solo 3 items)
  writeData(wb, hoja, escenarios$des_corto[i], startRow = 1, startCol = 2)
  writeData(wb, hoja, paste0("Escenario ", i), startRow = 1, startCol = 7)
  writeData(wb, hoja, CFG$disclaimer,          startRow = 1, startCol = 8)
  
  # Títulos Kakwani — varían por escenario
  titulo_kk_i <- t(c(
    "Base concentración", "Base Kakwani",
    paste(nombres_esc_cortos[i], "concentración"),
    paste(nombres_esc_cortos[i], "Kakwani")
  ))
  writeData(wb, hoja, titulo_kk_i,          startRow = 14, startCol = 10,
            colNames = FALSE)
  writeData(wb, hoja, TITULO_FILAS_KAKWANI, startRow = 15, startCol = 9,
            colNames = FALSE)
  
  # Textos pre-computados (fijos + grupo)
  escribir_en_hoja(wb, hoja, textos_fijos_template, tipo = "text")
  escribir_en_hoja(wb, hoja, textos_grupo_precomp,  tipo = "text")
  
  # Params pre-computados
  escribir_en_hoja(wb, hoja, params_esc_base, tipo = "data")
}

# FIX: eliminar la hoja plantilla antes de guardar el archivo
removeWorksheet(wb, PLANTILLA)

## =============================================================================
## 5. HOJA RESUMEN
## =============================================================================

message("Generando hoja resumen...")
hoja <- "resumen"
addWorksheet(wb, hoja)

extraer_resumen <- function(i) {
  if (i == 0) {
    esc <- resultados_escenarios[[1]]
    col_pov <- 1
    col_kak <- 2
    comr <- rbind(as.numeric(esc$decsum[11, 2]),
                  as.numeric(esc$decsum[11, 3]),
                  as.numeric(esc$decsum[11, 4]), 0)
  } else {
    esc <- resultados_escenarios[[paste0("escenario_", i)]]
    col_pov <- 4
    col_kak <- 4
    comr <- rbind(as.numeric(esc$decsum[11, 6]),
                  as.numeric(esc$decsum[11, 7]),
                  as.numeric(esc$decsum[11, 8]),
                  as.numeric(esc$decsum[11, 10] * -1))
  }
  list(
    povr = rbind(as.numeric(esc$pov_gral[1, col_pov]),
                 as.numeric(esc$pov_ext[1,  col_pov]),
                 as.numeric(esc$pov_int[1,  col_pov])),
    npob = rbind(as.numeric(esc$pov_gral[3, col_pov]),
                 as.numeric(esc$pov_ext[3,  col_pov]),
                 as.numeric(esc$pov_int[3,  col_pov])),
    povb = rbind(as.numeric(esc$pov_gral[6, col_pov]),
                 as.numeric(esc$pov_ext[6,  col_pov]),
                 as.numeric(esc$pov_int[6,  col_pov])),
    desr = rbind(as.numeric(esc$ineq[1, col_pov]),
                 as.numeric(esc$ineq[3, col_pov]),
                 as.numeric(esc$ineq[4, col_pov])
                 ),
    kakr = rbind(as.numeric(esc$kakwani[1, col_kak]),
                 as.numeric(esc$kakwani[2, col_kak]),
                 as.numeric(esc$kakwani[3, col_kak]),
                 as.numeric(esc$kakwani[4, col_kak])
    ),
    comr = comr
  )
}

res_list <- lapply(c(0, variable_values), extraer_resumen)
combinar <- function(campo) do.call(cbind, lapply(res_list, `[[`, campo))
povr <- combinar("povr"); npov <- combinar("npob")
povb <- combinar("povb"); comr <- combinar("comr"); desr <- combinar("desr")
kakr <- combinar("kakr")

descri1 <- t(as.matrix(nombres_esc_cols))
enc_pov <- as.matrix(c("Pobreza general", "Pobreza extrema",
                       "Pobreza -línea internacional-"))
enc_inq <- as.matrix(c("Gini", "Palma", "Theil"))
enc_kak <- as.matrix(c("ISR", "ITBIS", "Subsidios", "Compensaciones"))


escribir_en_hoja(wb, hoja, list(
  list(text = "Pobreza - después de impuestos y compensación",
       startRow = 3,  startCol = 2),
  list(text = "Brecha de la pobreza - después de impuestos y compensación",
       startRow = 14, startCol = 2),
  list(text = "Desigualdad (luego de impuestos y transferencias)",
       startRow = 29, startCol = 2),
  list(text = "Progresividad fiscal (kakwani)",
       startRow = 35, startCol = 2),
  list(text = "Efectos fiscales estimados (% del PIB)",
       startRow = 42, startCol = 2)
), tipo = "text")

escribir_en_hoja(wb, hoja, list(
  list(data = descri1, row = 4,  col = 3),
  list(data = descri1, row = 15, col = 3),
  list(data = descri1, row = 30, col = 3),
  list(data = descri1, row = 35, col = 3),
  list(data = descri1, row = 43, col = 3),
  list(data = povr,    row = 5,  col = 3),
  list(data = povb,    row = 16, col = 3),
  list(data = desr,    row = 31, col = 3),
  list(data = kakr,    row = 37, col = 3),
  list(data = comr,    row = 44, col = 3),
  list(data = enc_pov, row = 5,  col = 2),
  list(data = enc_pov, row = 16, col = 2),
  list(data = enc_inq, row = 31, col = 2),
  list(data = enc_kak, row = 37, col = 2),
  list(data = as.matrix(names(CFG$fiscal)), row = 44, col = 2)
), tipo = "data")

# Estilos
jj <- 4 + length(variable_values); ll <- length(variable_values)
addStyle(wb, hoja, estilos$encabezado, rows = c(4, 15, 30, 36, 43), cols = 2:jj,
         gridExpand = TRUE)
addStyle(wb, hoja, estilos$descriptor,
         rows = c(5:13, 16:28, 31:33, 37:40, 44:50), cols = 2, gridExpand = TRUE)
addStyle(wb, hoja, estilos$numerico, rows = c(5:11, 16:18, 44:47),
         cols = 3:jj, gridExpand = TRUE)
addStyle(wb, hoja, estilos$numerico2, rows = c(31:33, 37:40), cols = 3:jj,
         gridExpand = TRUE)
addStyle(wb, hoja, estilos$titulo2, rows = c(3, 14, 29, 35, 42),
         cols = 2:jj, gridExpand = TRUE)
setColWidths(wb, hoja, cols = 2:(ll + 2), widths = c(42, rep(15, ll)))

# Gráficos resumen (renderizar en paralelo)
graficos_pov <- list(
  grafico_barras_fila(povr, 1, "Pobreza general",                   "Porcentaje", 1),
  grafico_barras_fila(povr, 1, "Pobreza extrema",                   "Porcentaje", 2),
  grafico_barras_fila(povr, 1, "Pobreza, línea internacional",      "Porcentaje", 3),
  grafico_barras_fila(povb, 0, "Brecha de la pobreza (general)",    "RD$ al mes", 1),
  grafico_barras_fila(povb, 0, "Brecha de la pobreza (extrema)",    "RD$ al mes por hogar", 2),
  grafico_barras_fila(povb, 0, "Brecha de la pobreza (línea int.)", "RD$ al mes por hogar", 3),
  grafico_barras_fila(desr, 4, "Desigualdad (Gini)",                "Coeficiente de Gini", 1)
)

df_fiscal <- as.data.frame(comr) %>%
  setNames(nombres_esc_cols) %>%
  mutate(Fila = names(CFG$fiscal)) %>%
  pivot_longer(-Fila, names_to = "Categoria", values_to = "Valor") %>%
  mutate(Categoria = factor(Categoria, levels = nombres_esc_cols))

g_fiscal <- ggplot(filter(df_fiscal, !is.na(Valor)),
                   aes(x = Categoria, y = Valor, fill = Fila)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.7) +
  geom_text(aes(label = scales::comma(round(Valor, 1)), group = Fila),
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = CFG$colores_fiscal) +
  scale_x_discrete(labels = scales::wrap_format(35)) +
  scale_y_continuous(
    limits = c(min(df_fiscal$Valor, na.rm = TRUE) * 1.2,
               max(df_fiscal$Valor, na.rm = TRUE) * 1.2)) +
  labs(title = "Efectos fiscales de impuestos y compensaciones",
       y = "% del PIB", x = "", fill = "") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

posiciones_res <- list(
  c(1, 16), c(1, 24), c(1, 32), c(15, 16), c(15, 24), c(15, 32),
  c(29, 16), c(43, 16)
)
insertar_graficos(wb, hoja, c(graficos_pov, list(g_fiscal)), posiciones_res)

## =============================================================================
# 6. HOJAS COMPARATIVOS -------------------------------------------------------
## =============================================================================

message("Generando hojas comparativas...")

resultados_comp <- lapply(CFG$columnas, procesar_resultado)
names(resultados_comp) <- names(CFG$columnas)

descri_comp <- t(as.matrix(nombres_esc_cortos[
  escenarios$escenario %in% variable_values]))

for (tipo in c("isr", "itbis", "sub", "com")) {
  resultados_comp[[tipo]] <- lapply(resultados_comp[[tipo]],
                                    function(mat) mat * -1)
}

pos_graficos <- list(
  inc   = CFG$pos_graficos_base,
  isr   = lapply(CFG$pos_graficos_base, `+`, CFG$offset_isr),
  itbis = lapply(CFG$pos_graficos_base, `+`, CFG$offset_itbis),
  sub   = lapply(CFG$pos_graficos_base, `+`, CFG$offset_sub),
  com   = lapply(CFG$pos_graficos_base, `+`, CFG$offset_com)
)

# ── Pre-computar datos del mapa UNA sola vez, fuera de escribir_hoja_comp ──
dep_titulos <- TITULOS_GRUPOS$region
dep_nombres <- dep_titulos[-length(dep_titulos)]
rd_depto    <- as.data.frame(resultados_comp$inc$depinc)
rd_depto[["macro_region"]] <- dep_nombres
rd_depto <- rd_depto %>% select(macro_region, everything())

rd_mapa <- st_read(CFG$shp_deptos, quiet = TRUE) %>%
  mutate(macro_region = str_to_title(mcr_rgn)) %>%
  left_join(rd_depto, by = "macro_region")

cols_nitx <- grep("^nitx", names(rd_mapa), value = TRUE)

# ── Generar objetos ggplot: 30 comparativos + 6 mapas ──────────────────────
message("  Renderizando gráficos comparativos...")
t_graf <- Sys.time()

graficos_comp <- list(
  inc   = generar_graficos_todos(resultados_comp$inc,   "inc",
                                 "Incidencia neta en hogares",
                                 incluir_base = FALSE),
  isr   = generar_graficos_todos(resultados_comp$isr,   "inc",
                                 "Cambio en incidencia ISR",
                                 incluir_base = TRUE),
  itbis = generar_graficos_todos(resultados_comp$itbis, "inc",
                                 "Cambio en incidencia ITBIS",
                                 incluir_base = TRUE),
  sub   = generar_graficos_todos(resultados_comp$sub,   "inc",
                                 "Cambio en incidencia subsidio",
                                 incluir_base = TRUE),
  com   = generar_graficos_todos(resultados_comp$com,   "inc",
                                 "Incidencia compensación",
                                 incluir_base = FALSE)
)

graficos_mapas <- lapply(seq_along(cols_nitx), function(idx) {
  col_name <- cols_nitx[idx]
  ggplot(rd_mapa) +
    geom_sf(aes(fill = .data[[col_name]]), color = "white") +
    scale_fill_gradient(low = CFG$mapa_color_low, high = CFG$mapa_color_high,
                        name = "Efecto neto") +
    labs(title    = "Incidencia neta reforma por macro región",
         subtitle = escenarios$des_corto[idx]) +
    theme_minimal()
})

# ── Guardar TODO en un solo batch paralelo: 30 comparativos + 6 mapas ──────
todos_los_graficos <- c(unlist(graficos_comp, recursive = FALSE), graficos_mapas)
todas_las_rutas    <- guardar_graficos_batch(todos_los_graficos)

message(sprintf("  Gráficos renderizados: %.1fs (%d gráficos)",
                as.numeric(Sys.time() - t_graf, units = "secs"),
                length(todas_las_rutas)))

# ── Separar rutas: primeras N son comparativos, últimas 6 son mapas ────────
n_comp      <- length(unlist(graficos_comp, recursive = FALSE))
rutas_comp  <- todas_las_rutas[seq_len(n_comp)]
rutas_mapas <- todas_las_rutas[seq(n_comp + 1, length(todas_las_rutas))]

# Agrupar rutas comparativas por tipo para inserción
rutas_por_tipo <- split(rutas_comp,
                        rep(names(graficos_comp),
                            sapply(graficos_comp, length)))
pos_por_tipo   <- split(unlist(pos_graficos, recursive = FALSE),
                        rep(names(pos_graficos),
                            sapply(pos_graficos, length)))

categorias_comp <- data.frame(
  categoria = c("por deciles", "por estrato económico",
                "por área de residencia", "por sexo del jefe del hogar",
                "por categorías de hogar", "por macro región"),
  startRow  = c(2, 18, 28, 36, 44, 52),
  stringsAsFactors = FALSE
)

# ── escribir_hoja_comp: solo I/O, sin renderizado ──────────────────────────
escribir_hoja_comp <- function(nombre_hoja) {
  addWorksheet(wb, nombre_hoja)
  
  textos_h <- unlist(lapply(names(CFG$col_excel), function(tipo) {
    ce  <- CFG$col_excel[[tipo]]
    lab <- CFG$columnas[[tipo]]$label
    lapply(seq_len(nrow(categorias_comp)), function(k) {
      list(text = lab, startRow = categorias_comp$startRow[k], startCol = ce$c1)
    })
  }), recursive = FALSE)
  escribir_en_hoja(wb, nombre_hoja, textos_h, tipo = "text")
  
  params_h <- unlist(lapply(names(CFG$col_excel), function(tipo) {
    ce  <- CFG$col_excel[[tipo]]
    res <- resultados_comp[[tipo]]
    suf <- CFG$columnas[[tipo]]$sufijo
    generate_params(res, ce$c1, ce$c2, 3, suf, descri_comp)
  }), recursive = FALSE)
  escribir_en_hoja(wb, nombre_hoja, params_h, tipo = "data")
  
  cols_all <- unlist(lapply(CFG$col_excel, function(ce) {
    ce$c1:(ce$c1 + length(variable_values)+1)
  }))
  addStyle(wb, nombre_hoja, estilos$encabezado,
           rows = categorias_comp$startRow + 2,
           cols = cols_all, gridExpand = TRUE)
  addStyle(wb, nombre_hoja, estilos$titulo2,
           rows = categorias_comp$startRow,
           cols = cols_all, gridExpand = TRUE)
  addStyle(wb, nombre_hoja, estilos$encabezado,
           rows = 3, cols = cols_all, gridExpand = TRUE)
  addStyle(wb, nombre_hoja, estilos$descriptor,
           rows = 2:100,
           cols = sapply(CFG$col_excel, function(ce) ce$c1 - 1),
           gridExpand = TRUE)
  addStyle(wb, nombre_hoja, estilos$numerico,
           rows = c(4:13, 21:24, 31:32, 39:40, 47:50, 55:58),
           cols = cols_all, gridExpand = TRUE)
  setColWidths(wb, nombre_hoja, cols = 1:2, widths = c(2, 2))
  setColWidths(wb, nombre_hoja, cols = cols_all,
               widths = c(25, rep(15, length(variable_values))))
  
  # Insertar PNGs comparativos pre-renderizados
  for (tipo in names(rutas_por_tipo)) {
    insertar_pngs(wb, nombre_hoja, rutas_por_tipo[[tipo]], pos_por_tipo[[tipo]])
  }
  
  # Insertar mapas pre-renderizados
  fila_ini <- 100; esp <- 16
  for (idx in seq_along(rutas_mapas)) {
    insertImage(wb, sheet = nombre_hoja, file = rutas_mapas[[idx]],
                startRow = fila_ini + (idx - 1) * esp, startCol = 17)
  }
}

escribir_hoja_comp("comparativos")

insumos <- list(
  resumen = list(
    "pobreza"         = povr,
    "nuevos_pobres"   = npov,
    "brecha_pobreza"  = povb,
    "desigualdad"     = desr,
    "progresividad"   = kakr
  ),
  incidencia = list(
    "efecto_neto"    = resultados_comp$inc,
    "isr"            = resultados_comp$isr,
    "itbis"          = resultados_comp$itbis,
    "subsidios"      = resultados_comp$sub,
    "compensacion"   = resultados_comp$com
  ),
  concentracion = list(
    "efecto_neto"    = resultados_comp$cinc,
    "isr"            = resultados_comp$cisr,
    "itbis"          = resultados_comp$citbis,
    "subsidios"      = resultados_comp$csub,
    "compensacion"   = resultados_comp$ccom
  )
)


## =============================================================================
# 7. HOJA ANEXO ----------------------------------------------------------------
## =============================================================================

hoja <- "anexo"
addWorksheet(wb, hoja)

writeData(wb, hoja, "Descripción de escenarios", startCol = 2, startRow = 2)

des00 <- as_tibble(cbind(
  escenarios$escenario, escenarios$des_corto, escenarios$des_escenario
), .name_repair = "unique") %>%
  janitor::clean_names() %>%
  rename("No." = x1, "Descripción" = x2, "Descripción larga" = x3)
writeData(wb, hoja, des00, startCol = 2, startRow = 4)

ind_df <- as.matrix(cbind(
  sapply(CFG$indicadores, `[[`, "n"),
  sapply(CFG$indicadores, `[[`, "d")
))

ind_df <- as_tibble(ind_df, .name_repair = "unique")

#ind_df <- as_tibble(ind_df)
colnames(ind_df) <- c("Indicador","Descripción")
estilo_desc2 <- createStyle(fontSize = 9, fontColour = "black",
                            halign = "justify", fontName = "Aptos Narrow",
                            wrapText =  TRUE)

j_ind <- 4 + length(variable_values)
addStyle(wb, hoja, estilo_desc2,
         rows = 5:j_ind, cols = 4, gridExpand = TRUE)

j_ind <- 6 + length(variable_values)
writeData(wb, hoja, "Descripción de indicadores", startCol = 2,
  startRow = j_ind)
writeData(wb, hoja, ind_df, startCol = 3, startRow = j_ind + 2)

setColWidths(wb, hoja, cols = 3:4, widths = c(50, 80))
addStyle(wb, hoja, estilos$titulo,     rows = 2,      cols = 2)
addStyle(wb, hoja, estilos$titulo,     rows = j_ind,  cols = 2)
addStyle(wb, hoja, estilos$titulo2,    rows = 4,      cols = 2:4,
         gridExpand = TRUE)
addStyle(wb, hoja, estilos$titulo2,    rows = j_ind + 2, cols = 3:4,
         gridExpand = TRUE)
addStyle(wb, hoja, estilos$descriptor,
         rows = 5:(4 + length(variable_values)), cols = 2:3, gridExpand = TRUE)


k_ind <- j_ind + 2 + nrow(ind_df)
addStyle(wb, hoja, estilos$descriptor,
         rows = (j_ind + 3):k_ind, cols = 2:3, gridExpand = TRUE)
addStyle(wb, hoja, estilo_desc2,
         rows = (j_ind + 3):k_ind, cols = 4, gridExpand = TRUE)

## =============================================================================
## 8. GUARDAR ------------------------------------------------------------------
## =============================================================================

message("Guardando archivo Excel...")
saveRDS(insumos, paste0(fdbmod, "DOM_insumos.rds"))

fecha  <- format(Sys.time(), "%Y%m%d_%H%M")
nombre <- paste0("Resultados_", fecha, ".xlsx")
saveWorkbook(wb, nombre, overwrite = TRUE)
# Limpiar workers paralelos
if (PARALELO_ACTIVO) future::plan(future::sequential)

if (.Platform$OS.type == "windows") shell.exec(nombre) else system2("open", nombre)