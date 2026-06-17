# República Dominicana: constructor de escenarios fiscales (Shiny local).
# Instalar antes los paquetes indicados en README.md.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(jsonlite)
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(scales)
  library(RColorBrewer)
})

# Funciones auxiliares

`%||%` <- function(x, y) if (is.null(x)) y else x

strip_cli_markup <- function(x) {
  if (!length(x) || !nzchar(x)) return(x)
  x <- as.character(x)
  x <- gsub("\u001b\\[[0-9;]*m", "", x, perl = TRUE)
  x <- gsub("\u00a4\\[[0-9;]*m", "", x, perl = TRUE)
  x
}

fill_sub_ele_inputs_from_row <- function(session, r) {
  r1 <- as.list(r[1, ])
  for (j in 1:7) {
    nm  <- paste0("block", j)
    val <- suppressWarnings(as.numeric(r1[[nm]]))
    if (length(val) != 1L || is.na(val)) val <- NA_real_
    updateNumericInput(session, paste0("sub_block_", j), value = val)
  }
  for (pref in c("bsur","bnorte","beste","tsur","tnorte","teste")) {
    for (j in 1:7) {
      nm  <- paste0(pref, j)
      val <- suppressWarnings(as.numeric(r1[[nm]]))
      if (length(val) != 1L || is.na(val)) val <- NA_real_
      updateNumericInput(session, paste0("sub_", pref, "_", j), value = val)
    }
  }
  for (cx in c("costsur","costnorte","costeste","otsur","otnorte","oteste")) {
    v <- suppressWarnings(as.numeric(r1[[cx]]))
    if (length(v) != 1L || is.na(v)) v <- NA_real_
    updateNumericInput(session, paste0("sub_", cx), value = v)
  }
  updateTextInput(session, "sub_nom",
                  value = as.character(r1$nom_subsidio %||% ""))
}

# Indicador visual de pasos del asistente (1 a 6)
wizard_header <- function(step) {
  lbls <- c("ITBIS", "Renta", "Subsidio", "Compensaci\u00f3n",
            "Revisar", "Simular")
  tags$div(
    class = "d-flex align-items-center gap-2 mb-3 flex-wrap",
    style = "font-size:0.78rem;",
    lapply(seq_along(lbls), function(i) {
      num_cls  <- if (i < step)  "bg-success text-white"  else
                  if (i == step) "text-white"              else
                  "bg-light text-secondary border"
      num_sty  <- if (i == step) "background:#005A9C;" else ""
      lbl_cls  <- if (i == step) "fw-semibold" else
                  if (i < step)  "text-success" else "text-secondary"
      tagList(
        tags$div(
          class = "d-flex align-items-center gap-1",
          tags$span(
            class = paste("rounded-circle d-flex align-items-center",
                          "justify-content-center", num_cls),
            style = paste0("width:22px;height:22px;font-size:11px;",
                           "font-weight:600;", num_sty),
            if (i < step) HTML("&#10003;") else as.character(i)
          ),
          tags$span(class = lbl_cls, lbls[i])
        ),
        if (i < length(lbls))
          tags$span(style = "width:16px;height:2px;background:#dee2e6;",
                    class = "d-inline-block align-middle mx-1")
      )
    })
  )
}

# Fila de navegación inferior del asistente
nav_row <- function(prev_id = NULL, prev_label = "\u2190 Anterior",
                    next_id = NULL, next_label = "Siguiente \u2192",
                    next_class = "btn-primary") {
  tags$div(
    class = "d-flex justify-content-between pt-3 mt-4 border-top",
    if (!is.null(prev_id))
      actionButton(prev_id, HTML(prev_label),
                   class = "btn-outline-secondary")
    else tags$div(),
    if (!is.null(next_id))
      actionButton(next_id, HTML(next_label), class = next_class)
    else tags$div()
  )
}

# Subsidio eléctrico: tabla de límites de consumo por bloque (compartida entre
# distribuidoras). Los IDs sub_block_1..7 alimentan el motor sin cambios.
sub_limits_table <- function() {
  tags$table(
    class = "sub-ele-tbl", style = "max-width:360px;",
    tags$thead(tags$tr(
      tags$th(style = "width:70px;", "Bloque"),
      tags$th("L\u00edmite de consumo (kWh/mes)")
    )),
    tags$tbody(lapply(1:7, function(j) {
      tags$tr(
        tags$td(j, style = "text-align:center;font-weight:600;"),
        tags$td(numericInput(paste0("sub_block_", j), NULL,
                             value = NA_real_, min = 0, step = 1))
      )
    }))
  )
}

# Subsidio eléctrico: panel de una distribuidora (cargo fijo y RD$/kWh por bloque,
# tarifa de los bloques iniciales y costo de referencia). `pref` ∈ sur/norte/este.
sub_distribuidora_panel <- function(title, pref) {
  nav_panel(
    title = title,
    tags$table(
      class = "sub-ele-tbl", style = "max-width:420px;",
      tags$thead(tags$tr(
        tags$th(style = "width:70px;", "Bloque"),
        tags$th("Cargo fijo (RD$)"),
        tags$th("Tarifa (RD$/kWh)")
      )),
      tags$tbody(lapply(1:7, function(j) {
        tags$tr(
          tags$td(j, style = "text-align:center;font-weight:600;"),
          tags$td(numericInput(paste0("sub_b", pref, "_", j), NULL,
                               value = NA_real_, min = 0, step = 0.01)),
          tags$td(numericInput(paste0("sub_t", pref, "_", j), NULL,
                               value = NA_real_, min = 0, step = 0.01))
        )
      }))
    ),
    fluidRow(
      class = "mt-2",
      column(6, numericInput(paste0("sub_ot", pref),
                             "Tarifa bloques iniciales (RD$/kWh)",
                             value = NA_real_, min = 0, step = 0.01)),
      column(6, numericInput(paste0("sub_cost", pref),
                             "Costo de referencia (RD$/kWh)",
                             value = NA_real_, min = 0, step = 0.0001))
    )
  )
}

# Tabla de solo lectura con las tarifas vigentes (escenario base) para comparar.
build_sub_reference_ui <- function(ref) {
  if (is.null(ref) || nrow(ref) < 1L) {
    return(tags$p(class = "text-muted small", "Tarifas actuales no disponibles."))
  }
  r   <- as.list(ref[1, ])
  fmt <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) != 1L || is.na(v)) "\u2014"
    else formatC(v, format = "f", digits = 2, big.mark = ",")
  }
  tagList(
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        class = "sub-ele-tbl",
        tags$thead(tags$tr(
          tags$th("Bloque"), tags$th("L\u00edmite kWh"),
          tags$th("Edesur: cargo fijo"), tags$th("Edesur: RD$/kWh"),
          tags$th("Edenorte: cargo fijo"), tags$th("Edenorte: RD$/kWh"),
          tags$th("Edeeste: cargo fijo"), tags$th("Edeeste: RD$/kWh")
        )),
        tags$tbody(lapply(1:7, function(j) {
          tags$tr(
            tags$td(j, style = "text-align:center;font-weight:600;"),
            tags$td(fmt(r[[paste0("block",  j)]])),
            tags$td(fmt(r[[paste0("bsur",   j)]])),
            tags$td(fmt(r[[paste0("tsur",   j)]])),
            tags$td(fmt(r[[paste0("bnorte", j)]])),
            tags$td(fmt(r[[paste0("tnorte", j)]])),
            tags$td(fmt(r[[paste0("beste",  j)]])),
            tags$td(fmt(r[[paste0("teste",  j)]]))
          )
        }))
      )
    ),
    tags$p(class = "text-muted small mt-1 mb-0",
      sprintf(paste0("Tarifa de bloques iniciales (RD$/kWh) \u2014 ",
                     "Edesur: %s, Edenorte: %s, Edeeste: %s."),
              fmt(r$otsur), fmt(r$otnorte), fmt(r$oteste))),
    tags$p(class = "text-muted small mb-0",
      sprintf(paste0("Costo de referencia (RD$/kWh) \u2014 ",
                     "Edesur: %s, Edenorte: %s, Edeeste: %s."),
              fmt(r$costsur), fmt(r$costnorte), fmt(r$costeste)))
  )
}

# Carga de archivos del motor
root <- getwd()
source(file.path(root, "R", "00_autoload.R"))
autoload_dom_r(root)

itbis_catalog    <- load_itbis_catalog(root)
itbis_grupo_full <- itbis_catalog %>%
  distinct(.data$COD_GRUPO, .data$DES_GRUPO) %>%
  arrange(.data$COD_GRUPO)

make_grupo_choices <- function(df) {
  stats::setNames(
    df$COD_GRUPO,
    paste(df$COD_GRUPO, df$DES_GRUPO, sep = ": ")
  )
}

grupo_choice_vals_full <- make_grupo_choices(itbis_grupo_full)

# Tasas distintas presentes en el catálogo base (para el desplegable "Por tasa").
itbis_rate_choices <- {
  rr <- sort(unique(round(suppressWarnings(as.numeric(itbis_catalog$tasa)), 2)))
  rr <- rr[is.finite(rr)]
  stats::setNames(as.character(rr), paste0(rr, "%"))
}

# Clave por fila precalculada (para resolver rápido la selección de la UI).
itbis_row_keys <- vapply(
  seq_len(nrow(itbis_catalog)),
  function(i) itbis_row_key(itbis_catalog[i, , drop = FALSE]),
  character(1)
)

# Barra reutilizable "Guardar como / Cargar" para la biblioteca de un componente.
# `prefix` ∈ {itbis, isr, sub, comp}. Genera ids lib_<prefix>_{name,save,pick,load}.
component_lib_bar <- function(prefix, titulo = "Guardar esta configuraci\u00f3n") {
  div(
    class = "component-lib-bar border rounded p-2 mt-3 bg-light",
    tags$div(class = "small fw-semibold text-muted mb-1",
             icon("box-archive"), " ", titulo,
             " \u2014 puede reutilizarla al componer escenarios"),
    uiOutput(paste0("lib_", prefix, "_status")),
    fluidRow(
      column(3, textInput(paste0("lib_", prefix, "_name"),
                          "Nombre", placeholder = "p. ej. Canasta b\u00e1sica exenta",
                          width = "100%")),
      column(2, div(class = "mt-4 pt-1",
                    actionButton(paste0("lib_", prefix, "_save"), "Guardar",
                                 class = "btn-outline-success w-100"))),
      column(2, div(class = "mt-4 pt-1",
                    actionButton(paste0("lib_", prefix, "_saveas"),
                                 "Guardar como",
                                 class = "btn-outline-success w-100"))),
      column(3, selectInput(paste0("lib_", prefix, "_pick"), "Cargar guardado",
                            choices = c("\u2014 elegir \u2014" = ""), width = "100%")),
      column(2, div(class = "mt-4 pt-1 d-flex gap-1",
                    actionButton(paste0("lib_", prefix, "_load"), "Cargar",
                                 class = "btn-outline-primary flex-grow-1"),
                    actionButton(paste0("lib_", prefix, "_del"),
                                 icon("trash"),
                                 class = "btn-outline-danger px-2",
                                 title = "Eliminar componente seleccionado")))
    )
  )
}

# Escala de renta base (sim_inc = 0) leída del CSV de parámetros. Es el escenario
# "pre-reforma" que debe mostrarse por defecto en la pantalla de Renta (no la propuesta).
isr_base_defaults <- local({
  fallback <- list(
    lim_inf  = c(0, 416220, 624329, 867123, 2400000, NA),
    lim_sup  = c(416220, 624329, 867123, 2400000, NA, NA),
    tasa_pct = c(0, 15, 20, 25, 27, NA)
  )
  out <- tryCatch({
    sr  <- read_param_csv("sim_renta", file.path(root, "data", "params"))
    row <- sr[sr$sim_inc == 0L, , drop = FALSE]
    if (nrow(row) < 1L) stop("sim_renta: no hay fila base (sim_inc = 0).")
    isr_brackets_from_sim_renta_row(row[1, , drop = FALSE], max_slots = 6L)
  }, error = function(e) fallback)
  out
})

# Tarifas de subsidio vigentes (fila base sim_sub = 0) para mostrar como referencia.
sub_base_ref <- tryCatch({
  read_sim_sub_template_row(file.path(root, "data", "params", ""), 0L)
}, error = function(e) NULL)
sub_reference_ui <- build_sub_reference_ui(sub_base_ref)

# --- Helpers de previsualización para la pantalla Revisar ---

# Productos cuya tasa efectiva difiere de la legal, dadas las listas de un escenario.
itbis_changes_table <- function(rate_v = list(), rate_s = list(),
                                rate_g = list()) {
  if (length(rate_v) + length(rate_s) + length(rate_g) == 0L) return(NULL)
  d0 <- itbis_catalog
  n  <- nrow(d0)
  eff <- vapply(seq_len(n), function(i) {
    tasa_efectiva_desde_listas(d0[i, , drop = FALSE],
                               rate_v, rate_s, rate_g, "actual")
  }, numeric(1))
  ley     <- suppressWarnings(as.numeric(d0$tasa))
  changed <- !is.na(ley) & abs(eff - ley) > 1e-6
  if (!any(changed)) return(NULL)
  tibble::tibble(
    Grupo    = d0$DES_GRUPO[changed],
    Producto = paste0(d0$ID_VARIEDAD[changed], ": ", d0$DES_VARIEDAD[changed]),
    `Tasa vieja (%)` = round(ley[changed], 2),
    `Tasa nueva (%)` = round(eff[changed], 2)
  )
}

# Tabla HTML compacta de solo lectura desde un data.frame.
df_to_compact_table <- function(df) {
  tags$table(
    class = "sub-ele-tbl", style = "width:100%;",
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(lapply(seq_len(nrow(df)), function(r) {
      tags$tr(lapply(seq_along(df), function(cc)
        tags$td(as.character(df[[cc]][r]))))
    }))
  )
}

# Descripción legible de la política de compensación de un escenario guardado.
comp_describe <- function(comp) {
  if (is.null(comp) || isTRUE(comp$sin_compensacion) ||
      !isTRUE(comp$enabled)) {
    return("Ninguna")
  }
  gmap <- c("1" = "Beneficiarios Sup\u00e9rate",
            "2" = "Decil de ingreso", "3" = "ICV")
  mmap <- c("1" = "p\u00e9rdida neta promedio", "2" = "valor fijo mensual")
  g <- gmap[[as.character(comp$grupo_com)]] %||% "?"
  m <- mmap[[as.character(comp$metodo_com)]] %||% "?"
  who <- g
  if (identical(as.character(comp$grupo_com), "2") &&
      length(comp$decil_com) && !is.na(comp$decil_com)) {
    who <- paste0(g, " (hasta decil ", comp$decil_com, ")")
  }
  if (identical(as.character(comp$grupo_com), "3") &&
      length(comp$icv_com) && !is.na(comp$icv_com)) {
    who <- paste0(g, " (ICV \u2264 ", comp$icv_com, ")")
  }
  how <- m
  if (identical(as.character(comp$metodo_com), "1") &&
      length(comp$decil_est) && !is.na(comp$decil_est)) {
    how <- paste0(m, " (promedio hasta decil ", comp$decil_est, ")")
  }
  if (identical(as.character(comp$metodo_com), "2")) {
    how <- paste0(m, " (RD$ ", comp$valor_com %||% 0, "/mes)")
  }
  paste0(who, " \u2014 ", how)
}

# Bloque de detalle (previsualización) para un escenario guardado.
slot_detail_ui <- function(sc) {
  ch <- itbis_changes_table(
    sc$itbis$rate_variedad %||% list(),
    sc$itbis$rate_subclase %||% list(),
    sc$itbis$rate_grupo    %||% list()
  )
  itbis_block <- if (is.null(ch)) {
    tags$p(class = "small text-muted mb-2",
           tags$strong("ITBIS:"), " sin cambios respecto a la referencia.")
  } else {
    tagList(
      tags$p(class = "small fw-semibold mb-1",
             sprintf("ITBIS: %d producto(s) modificado(s)", nrow(ch))),
      tags$div(style = "max-height:180px;overflow:auto;",
               df_to_compact_table(ch))
    )
  }

  isr_block <- if (isTRUE(sc$custom_isr) && !is.null(sc$isr)) {
    li <- sc$isr$lim_inf; ls <- sc$isr$lim_sup; tp <- sc$isr$tasa_pct
    keep <- which(vapply(seq_along(li), function(i)
      isTRUE(is.finite(suppressWarnings(as.numeric(li[[i]])))), logical(1)))
    fmt <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (length(v) != 1L || is.na(v)) "sin tope"
      else formatC(v, format = "f", digits = 0, big.mark = ",")
    }
    tbl <- tibble::tibble(
      `Lim. inferior` = vapply(keep, function(i) fmt(li[[i]]), character(1)),
      `Lim. superior` = vapply(keep, function(i) fmt(ls[[i]]), character(1)),
      `Tasa (%)`      = vapply(keep, function(i) {
        v <- suppressWarnings(as.numeric(tp[[i]]))
        if (length(v) != 1L || is.na(v)) "\u2014" else as.character(round(v, 2))
      }, character(1))
    )
    tagList(
      tags$p(class = "small fw-semibold mb-1",
             "Renta: escala personalizada ",
             tags$span(class = "text-muted",
                       paste0("(", sc$isr$nom_renta %||% "", ")"))),
      df_to_compact_table(tbl)
    )
  } else {
    tags$p(class = "small text-muted mb-2",
           tags$strong("Renta:"), " escala vigente (sin cambios).")
  }

  sub_block <- if (isTRUE(sc$sub_ele$custom)) {
    tags$p(class = "small mb-2",
           tags$strong("Subsidio el\u00e9ctrico:"), " personalizado",
           if (nzchar(sc$sub_ele$nom_subsidio %||% ""))
             tags$span(class = "text-muted",
                       paste0(" (", sc$sub_ele$nom_subsidio, ")")))
  } else {
    tags$p(class = "small text-muted mb-2",
           tags$strong("Subsidio el\u00e9ctrico:"), " vigente (sin cambios).")
  }

  comp_block <- tags$p(class = "small mb-0",
                       tags$strong("Compensaci\u00f3n:"), " ",
                       comp_describe(sc$comp))

  tagList(itbis_block, tags$hr(class = "my-2"),
          isr_block, sub_block, comp_block)
}

# Estilos CSS
dom_css <- HTML("
  .dom-par-card .card-header  { font-weight:600; }
  .isr-tbl { width:100%; border-collapse:collapse; font-size:13px; max-width:720px; }
  .isr-tbl th { padding:8px 10px; text-align:center; border:1px solid #bbb;
                background:#2c3e50; color:#fff; }
  .isr-tbl td { padding:4px 6px; border:1px solid #e0e0e0; vertical-align:middle; }
  .isr-tbl tr:nth-child(even) td { background:#f9f9f9; }
  .isr-tbl .form-group { margin-bottom:0; }
  .isr-tbl .form-control { height:30px; font-size:13px; }

  .sub-ele-tbl { width:100%; border-collapse:collapse; font-size:12px; }
  .sub-ele-tbl th { padding:6px 4px; text-align:center; border:1px solid #bbb;
                    background:#2c3e50; color:#fff; }
  .sub-ele-tbl td { padding:2px 4px; border:1px solid #e0e0e0; vertical-align:middle; }
  .sub-ele-tbl tr:nth-child(even) td { background:#f9f9f9; }
  .sub-ele-tbl .form-group { margin-bottom:0; }
  .sub-ele-tbl .form-control { height:28px; font-size:12px; padding:2px 6px; }

  .slot-card { border-left:4px solid #dee2e6; }
  .slot-card.filled { border-left-color:#005A9C; }
  .slot-badge { font-size:0.7rem; }
  .scenario-chip { background:#e8f0fb; border-radius:6px; padding:6px 10px;
                   font-size:0.8rem; margin-bottom:4px; }
")

# Interfaz de usuario
ui <- page_navbar(
  id     = "main_nav",
  title  = tags$span(style = "font-weight:600;",
                     "RD: Microsimulación Fiscal"),
  header = tags$head(tags$style(dom_css)),
  theme  = bs_theme(
    version      = 5,
    bootswatch   = "zephyr",
    primary      = "#005A9C",
    "navbar-bg"  = "#0d2137"
  ),
  fillable = FALSE,

  # Pantalla 1: ITBIS
  nav_panel(
    title = "1 \u00b7 ITBIS",
    icon  = icon("percent"),
    div(class = "container-fluid py-2",
      wizard_header(1L),
      card(
        card_header(
          tags$div(class = "d-flex justify-content-between align-items-center",
            "Tasas por producto",
            actionButton("btn_nuevo_escenario",
                         tagList(icon("file"), " Empezar de cero"),
                         class = "btn-sm btn-outline-secondary",
                         title = "Restablece todas las pantallas a la situaci\u00f3n vigente")
          )
        ),
        component_lib_bar("itbis", "Guardar esta pol\u00edtica de ITBIS"),
        navset_pill(
          # Modo 1: navegar por la clasificación del producto
          nav_panel(
            "Por clasificaci\u00f3n",
            p(class = "text-muted small mt-2",
              "Elija un grupo, una subclase o una variedad y aplique la tasa."),
            layout_columns(
              col_widths = c(4, 4, 4),
              selectInput("itbis_grupo", "Grupo",
                          choices  = grupo_choice_vals_full,
                          selected = itbis_grupo_full$COD_GRUPO[1],
                          width    = "100%"),
              selectInput("itbis_subclase", "Subclase",
                          choices = NULL, width = "100%"),
              selectizeInput("itbis_variedad", "Variedad",
                             choices = NULL, width = "100%")
            ),
            uiOutput("itbis_sel_info"),
            layout_columns(
              col_widths = c(6, 3, 3),
              radioButtons(
                "itbis_nivel_aplicar", "Aplicar tasa a",
                choiceNames  = list("Todo el grupo",
                                    "La subclase",
                                    "Solo esta variedad"),
                choiceValues = c("grupo", "subclase", "variedad"),
                selected     = "subclase", inline = TRUE
              ),
              numericInput("tasa_aplicar", "Nueva tasa (%)",
                           value = NA, min = 0, max = 100, step = 0.25,
                           width = "100%"),
              div(class = "mt-4 pt-1",
                  actionButton("btn_aplicar_tasa", "Aplicar tasa",
                               class = "btn-outline-primary w-100"))
            ),
            div(class = "d-flex gap-2",
              actionButton("btn_reset_sel", "Restablecer la selecci\u00f3n",
                           class = "btn-outline-warning"),
              actionButton("btn_reset_rama", "Restablecer grupo",
                           class = "btn-outline-warning")
            )
          ),
          # Modo 2: seleccionar productos por su tasa actual (cruza grupos)
          nav_panel(
            "Por tasa",
            p(class = "text-muted small mt-2",
              "Elija una de las tasas existentes (o escriba otra) para listar todos ",
              "los productos que la tienen actualmente, selecci\u00f3nelos y aplique ",
              "una nueva tasa."),
            layout_columns(
              col_widths = c(4, 4, 4),
              selectizeInput("itbis_query_tasa",
                           "Mostrar productos con tasa (%)",
                           choices = itbis_rate_choices,
                           selected = itbis_rate_choices[1],
                           width = "100%",
                           options = list(
                             create = TRUE,
                             createOnBlur = TRUE,
                             persist = FALSE
                           )),
              numericInput("itbis_byrate_nueva", "Nueva tasa (%)",
                           value = NA, min = 0, max = 100, step = 0.25,
                           width = "100%"),
              div(class = "mt-4 pt-1",
                  actionButton("btn_byrate_aplicar",
                               "Aplicar a seleccionados",
                               class = "btn-outline-primary w-100"))
            ),
            div(class = "d-flex gap-2 mb-2",
              actionButton("btn_byrate_all", "Seleccionar todos",
                           class = "btn-outline-secondary btn-sm"),
              actionButton("btn_byrate_none", "Quitar selecci\u00f3n",
                           class = "btn-outline-secondary btn-sm")
            ),
            DTOutput("tbl_itbis_byrate", height = "260px")
          )
        ),
        hr(),
        actionButton("btn_limpiar_itbis",
                     "Restablecer valores del escenario base",
                     class = "btn-outline-danger"),
        tags$p(class = "small text-muted mt-1 mb-0",
               "Descarta todos los cambios y vuelve a las tasas vigentes.")
      ),
      card(
        card_header("Vista previa de cambios"),
        p(class = "small text-muted",
          "Productos con tasa diferente a la de referencia."),
        layout_columns(
          col_widths = c(6, 6),
          selectInput("prev_grupo_fil", "Filtrar por grupo",
                      choices = c("Todos los grupos" = "__todos__"), width = "100%"),
          selectInput("prev_subclase_fil", "Filtrar por subclase",
                      choices = c("Todas las subclases" = "__todos__"), width = "100%")
        ),
        DTOutput("tbl_itbis_resumen", height = "360px")
      ),
      nav_row(next_id = "nav_1_next")
    )
  ),

  # Pantalla 2: Renta
  nav_panel(
    title = "2 \u00b7 Renta",
    icon  = icon("landmark"),
    div(class = "container-fluid py-2 dom-par-card",
      wizard_header(2L),
      card(
        card_header("Impuesto sobre la renta personal"),
        checkboxInput("custom_isr",
                      "Personalizar escala y tramos (l\u00edmites y tasas)",
                      value = FALSE),
        component_lib_bar("isr", "Guardar esta escala de renta"),
        conditionalPanel(
          condition = "input.custom_isr == true",
          p(class = "text-muted small",
            "Los valores iniciales corresponden a la escala vigente (escenario base). ",
            "Por tramo indique l\u00edmite inferior, l\u00edmite superior y tasa (%). ",
            "El primer l\u00edmite inferior debe ser 0. ",
            "Deje el \u00faltimo superior vac\u00edo si el tramo no tiene tope."),
          p(class = "text-muted small fst-italic",
            "Las tasas indicadas son ",
            tags$strong("tasas marginales"),
            ": se aplican solo a la porci\u00f3n del ingreso dentro de cada tramo."),
          tags$table(
            class = "isr-tbl",
            tags$thead(tags$tr(
              tags$th(style = "width:50px;", "#"),
              tags$th("L\u00edmite inferior (RD$)"),
              tags$th("L\u00edmite superior (RD$)"),
              tags$th(style = "width:110px;", "Tasa marginal (%)")
            )),
            tags$tbody(lapply(1:6, function(i) {
              tags$tr(
                tags$td(i, style = "text-align:center;font-weight:bold;"),
                tags$td(numericInput(paste0("isr_li_", i), NULL,
                                    value = isr_base_defaults$lim_inf[i],
                                    min = 0, step = 1000)),
                tags$td(numericInput(paste0("isr_ls_", i), NULL,
                                    value = isr_base_defaults$lim_sup[i],
                                    min = 0, step = 1000)),
                tags$td(numericInput(paste0("isr_tp_", i), NULL,
                                    value = isr_base_defaults$tasa_pct[i],
                                    min = 0, max = 100, step = 0.5))
              )
            }))
          )
        )
      ),
      nav_row(prev_id = "nav_2_prev", next_id = "nav_2_next")
    )
  ),

  # Pantalla 3: Subsidio eléctrico
  nav_panel(
    title = "3 \u00b7 Subsidio",
    icon  = icon("bolt"),
    div(class = "container-fluid py-2 dom-par-card",
      wizard_header(3L),
        card(
          card_header("Subsidio el\u00e9ctrico"),
          p(class = "text-muted small mb-2",
            "Por defecto se usa el subsidio el\u00e9ctrico vigente. ",
            "Active la personalizaci\u00f3n para ajustar los bloques de consumo ",
            "y las tarifas de cada distribuidora."),
          checkboxInput("custom_sub_ele",
                      "Personalizar bloques y tarifas por distribuidora",
                      value = FALSE),
          component_lib_bar("sub", "Guardar esta pol\u00edtica de subsidio"),
        conditionalPanel(
          condition = "input.custom_sub_ele == true",
          p(class = "text-muted small",
            "Cada bloque de consumo cobra un ",
            tags$strong("cargo fijo (RD$)"),
            " m\u00e1s una ", tags$strong("tarifa por consumo (RD$/kWh)"),
            ". Defina los l\u00edmites de cada bloque y, para cada distribuidora, ",
            "el cargo fijo y la tarifa. Deje vac\u00edos los bloques que no use."),
          h6("L\u00edmites de consumo por bloque"),
          p(class = "text-muted small mb-1",
            "Estos l\u00edmites son comunes a las tres distribuidoras."),
          sub_limits_table(),
          h6(class = "mt-3", "Tarifas por distribuidora"),
          p(class = "text-muted small mb-2",
            "Los bloques corresponden a los l\u00edmites definidos arriba. ",
            "La tarifa de los bloques iniciales (consumo bajo) puede fijarse ",
            "aparte como tarifa subsidiada."),
          navset_pill(
            sub_distribuidora_panel("Edesur",   "sur"),
            sub_distribuidora_panel("Edenorte", "norte"),
            sub_distribuidora_panel("Edeeste",  "este")
          )
        ),
        tags$details(
          class = "mt-3",
          tags$summary(
            class = "text-primary", style = "cursor:pointer;",
            "Ver tarifas actuales (referencia)"),
          tags$div(class = "mt-2", sub_reference_ui)
        )
      ),
      nav_row(prev_id = "nav_3_prev", next_id = "nav_3_next")
    )
  ),

  # Pantalla 4: Compensación
  nav_panel(
    title = "4 \u00b7 Compensaci\u00f3n",
    icon  = icon("hand-holding-heart"),
    div(class = "container-fluid py-2 dom-par-card",
      wizard_header(4L),
        card(
          card_header("Pol\u00edtica de compensaci\u00f3n"),
          p(class = "text-muted small mb-2",
            "Si el escenario incluye una pol\u00edtica de compensaci\u00f3n, ",
            "marque la casilla y defina los par\u00e1metros."),
          checkboxInput("comp_con_comp", "Con compensaci\u00f3n",
                        value = FALSE),
          component_lib_bar("comp", "Guardar esta pol\u00edtica de compensaci\u00f3n"),
          conditionalPanel(
            condition = "input.comp_con_comp == true",
          hr(),
          h6("\u00bfQui\u00e9n recibe la compensaci\u00f3n?"),
          fluidRow(
            column(6, selectInput("comp_grupo", "Grupo beneficiario",
                                  choices = c(
                                    "Beneficiarios Sup\u00e9rate" = "1",
                                    "Decil de ingreso"            = "2",
                                    "\u00cdndice de Calidad de Vida (ICV)" = "3"
                                  ), selected = "1"))
          ),
          # Parámetro propio del grupo beneficiario (solo el relevante).
          conditionalPanel(
            condition = "input.comp_grupo == '2'",
            fluidRow(column(6, selectInput("comp_decil_com",
                                  "Tope de decil de ingreso",
                                  choices = c(
                                    "Sin seleccionar" = "",
                                    stats::setNames(as.character(1:10),
                                                    paste("Decil", 1:10))
                                  ), selected = "2"))),
            p(class = "text-muted small",
              "Reciben la compensaci\u00f3n los hogares hasta el decil indicado.")
          ),
          conditionalPanel(
            condition = "input.comp_grupo == '3'",
            fluidRow(column(6, selectInput("comp_icv",
                                  "Umbral ICV",
                                  choices = c(
                                    "Sin seleccionar" = "",
                                    stats::setNames(as.character(1:4),
                                                    paste("ICV", 1:4))
                                  ), selected = ""))),
            p(class = "text-muted small",
              "Reciben la compensaci\u00f3n los hogares con ICV hasta el umbral.")
          ),
          conditionalPanel(
            condition = "input.comp_grupo == '1'",
            p(class = "text-muted small",
              "Reciben la compensaci\u00f3n los beneficiarios actuales de Sup\u00e9rate.")
          ),
          hr(),
          h6("\u00bfC\u00f3mo se calcula el monto?"),
          fluidRow(
            column(6, selectInput("comp_metodo", "M\u00e9todo",
                                  choices = c(
                                    "P\u00e9rdida neta promedio" = "1",
                                    "Valor fijo mensual"        = "2"
                                  ), selected = "1"))
          ),
          # Parámetro propio del método (solo el relevante).
          conditionalPanel(
            condition = "input.comp_metodo == '1'",
            fluidRow(column(6, selectInput("comp_decil_est",
                                  "Hasta qu\u00e9 decil entra en el promedio",
                                  choices = c(
                                    "Sin seleccionar" = "",
                                    stats::setNames(as.character(1:10),
                                                    paste("Decil", 1:10))
                                  ), selected = "4"))),
            p(class = "text-muted small",
              "El monto es la p\u00e9rdida neta promedio (por la reforma) de los hogares ",
              "hasta el decil indicado.")
          ),
          conditionalPanel(
            condition = "input.comp_metodo == '2'",
            fluidRow(column(6, numericInput("comp_valor",
                                   "Valor fijo mensual (RD$)",
                                   value = 0, min = 0, step = 10))),
            p(class = "text-muted small",
              "Cada hogar beneficiario recibe este monto mensual (se anualiza \u00d7 12).")
          )
        )
      ),
      nav_row(prev_id = "nav_4_prev", next_id = "nav_4_next")
    )
  ),

  # Pantalla 5: Revisar y guardar
  nav_panel(
    title = "5 \u00b7 Revisar",
    icon  = icon("floppy-disk"),
    div(class = "container-fluid py-2",
      wizard_header(5L),
      card(
        card_header("Componer un escenario"),
        p(class = "text-muted small mb-2",
          "Un escenario combina una pol\u00edtica de cada pantalla. Elija las ",
          "configuraciones que guard\u00f3 (o \u201cReferencia\u201d para dejar esa ",
          "pantalla sin cambios) y agregue el escenario a la lista."),
        fluidRow(
          column(12, textInput("compose_name", "Nombre del escenario",
                               placeholder = "p. ej. Reforma integral 2026",
                               width = "100%"))
        ),
        fluidRow(
          column(3, selectInput("compose_itbis", "ITBIS",
                                choices = c("Par\u00e1metros pre-reforma (referencia)" = "__ref__"),
                                width = "100%")),
          column(3, selectInput("compose_isr", "Renta",
                                choices = c("Par\u00e1metros pre-reforma (referencia)" = "__ref__"),
                                width = "100%")),
          column(3, selectInput("compose_sub", "Subsidio",
                                choices = c("Par\u00e1metros pre-reforma (referencia)" = "__ref__"),
                                width = "100%")),
          column(3, selectInput("compose_comp", "Compensaci\u00f3n",
                                choices = c("Par\u00e1metros pre-reforma (referencia)" = "__ref__"),
                                width = "100%"))
        ),
        actionButton("btn_compose_add",
                     tagList(icon("plus"), " Agregar escenario a la lista"),
                     class = "btn-primary")
      ),
      card(
        card_header(
          tags$div(class = "d-flex justify-content-between align-items-center",
            "Escenarios guardados ",
            tags$span(class = "small text-muted", uiOutput("compare_count",
                                                            inline = TRUE)))
        ),
        p(class = "text-muted small",
          "Marque hasta cuatro para comparar en la simulaci\u00f3n. ",
          "Despliegue \u201cVer detalle\u201d para revisar tasas y parámetros."),
        uiOutput("scenarios_display")
      ),
      card(
        card_header("Guardar / compartir"),
        p(class = "text-muted small mb-2",
          "El archivo Excel lleva una pesta\u00f1a por componente y una con la lista ",
          "de escenarios; puede editarlo y volver a cargarlo. El trabajo tambi\u00e9n ",
          "se guarda autom\u00e1ticamente en esta sesi\u00f3n."),
        layout_columns(
          col_widths = c(6, 6),
          downloadButton("dl_xlsx", "Exportar escenario en formato Excel",
                         class = "w-100 btn-outline-secondary"),
          fileInput("up_xlsx", "Importar (.xlsx)",
                    accept = c(".xlsx"), width = "100%")
        ),
        hr(class = "my-2"),
        actionButton("btn_reset_all",
                     tagList(icon("trash"), " Nueva sesi\u00f3n (borrar todo)"),
                     class = "btn-outline-danger"),
        tags$span(class = "small text-muted ms-2",
                  "Borra componentes, escenarios y los valores en pantalla.")
      ),
      nav_row(prev_id = "nav_5_prev", next_id = "nav_5_next",
              next_label = "Ir a Simular \u2192")
    )
  ),

  # Pantalla 6: Simular y resultados
  nav_panel(
    title = "6 \u00b7 Simular",
    icon  = icon("chart-line"),
    div(class = "container-fluid py-2",
      wizard_header(6L),
      layout_columns(
        col_widths = c(12, 12),
        gap = "1rem",
        card(
          card_header("Ejecuci\u00f3n"),
          layout_columns(
            col_widths = c(4, 8),
            div(
              actionButton("run",
                           icon("play"),
                           label = " Ejecutar microsimulaci\u00f3n",
                           class = "btn-primary w-100"),
              p(class = "small text-muted mt-1 mb-0",
                "Corre los escenarios marcados para comparar.")
              # div(
              #   actionButton("run_test", icon("flask"),
              #                label = " Ejecutar prueba",
              #                class = "btn-outline-primary mt-2 w-100"),
              #   p(class = "small text-muted mt-1 mb-0",
              #     "Escenario de referencia del repositorio.")
              # )
            ),
            div(
              uiOutput("run_msg"),
              tags$div(class = "small text-muted mt-1",
                tags$strong("En cola: "),
                uiOutput("run_slots_summary", inline = TRUE)
              )
            )
          )
        ),
        navset_card_pill(
          id = "resultados_principales",
          nav_panel(
            title = "Resumen",
            navset_card_pill(
              id = "resumen_sub",
              nav_panel("Cuadros",
                p(class = "small text-muted",
                  "Columnas: ",
                  tags$strong("Pre-reforma"),
                  " y escenarios simulados."),
                layout_columns(col_widths = c(12, 12),
                  card(card_header("Tasas de pobreza (%)"),
                       DTOutput("dash_tbl_povr", height = "200px")),
                  card(card_header("Nuevos pobres (personas)"),
                       DTOutput("dash_tbl_npov", height = "200px"))
                ),
                layout_columns(col_widths = c(12, 12),
                  card(card_header("Brecha de pobreza promedio (pesos mensuales)"),
                       DTOutput("dash_tbl_povb", height = "200px")),
                  card(card_header("Nuevos pobres por escenario"),
                       plotlyOutput("dash_plot_npov_bar", height = "360px"))
                ),
                card(card_header("Desigualdad"),
                     DTOutput("dash_tbl_desr", height = "220px")),
                layout_columns(col_widths = c(12, 12),
                  card(card_header("\u00cdndice de concentraci\u00f3n (IC)"),
                       DTOutput("dash_tbl_kak_ic", height = "240px")),
                  card(card_header("\u00cdndice de Kakwani"),
                       DTOutput("dash_tbl_kak_kw", height = "240px"))
                )
              ),
              nav_panel("Pobreza general",
                layout_columns(col_widths = c(6, 6),
                  card(card_header("Pobreza general e incidencia"),
                       plotlyOutput("dash_plot_povr_1", height = "380px")),
                  card(card_header("Brecha de pobreza (general)"),
                       plotlyOutput("dash_plot_povb_1", height = "380px"))
                )
              ),
              nav_panel("Pobreza extrema",
                layout_columns(col_widths = c(6, 6),
                  card(card_header("Pobreza extrema"),
                       plotlyOutput("dash_plot_povr_2", height = "360px")),
                  card(card_header("Brecha (extrema)"),
                       plotlyOutput("dash_plot_povb_2", height = "360px"))
                )
              ),
              nav_panel("L\u00ednea internacional",
                p(class = "small text-muted",
                  tags$strong("LPIM:"),
                  " l\u00ednea internacional 8,3 USD/d\u00eda PPP 2021."),
                layout_columns(col_widths = c(6, 6),
                  card(card_header("Pobreza LPIM"),
                       plotlyOutput("dash_plot_povr_3", height = "360px")),
                  card(card_header("Brecha LPIM"),
                       plotlyOutput("dash_plot_povb_3", height = "360px"))
                )
              ),
              nav_panel("Desigualdad",
                layout_columns(col_widths = c(6, 6),
                  card(card_header("Coeficiente de Gini"),
                       plotlyOutput("dash_plot_gini", height = "360px")),
                  card(card_header("Cambio en Gini"),
                       plotlyOutput("dash_plot_dgini", height = "360px"))
                ),
                layout_columns(col_widths = c(6, 6),
                  card(card_header("\u00cdndice de Palma"),
                       plotlyOutput("dash_plot_palma", height = "360px")),
                  card(card_header("\u00cdndice de Theil"),
                       plotlyOutput("dash_plot_theil", height = "360px"))
                ),
                layout_columns(col_widths = c(12, 12),
                  card(card_header("Cuadro de indicadores"),
                       DTOutput("dash_tbl_gini", height = "200px")),
                  card(card_header("IC por instrumento"),
                       plotlyOutput("dash_plot_kak_ic", height = "380px"))
                ),
                card(card_header("Kakwani por instrumento"),
                     plotlyOutput("dash_plot_kak_kw", height = "380px"))
              ),
              nav_panel("Efecto fiscal",
                p(class = "small text-muted",
                  "Medidas agregadas (% PIB)."),
                card(card_header("Efectos fiscales por medida (% PIB)"),
                     DTOutput("dash_tbl_fiscal", height = "280px")),
                card(card_header("Compensaci\u00f3n (% PIB)"),
                     plotlyOutput("dash_plot_comp", height = "400px"))
              )
            )
          ),
          nav_panel(
            title = "Incidencia",
            p(class = "small text-muted px-1",
              tags$strong("Ingreso disponible:"),
              " definici\u00f3n oficial. ",
              "La incidencia neta es el efecto fiscal como % del ingreso."),
            navset_card_pill(
              id = "incidencia_sub",
              nav_panel("Deciles",
                        plotlyOutput("dash_inc_dec", height = "400px")),
              nav_panel("Estrato",
                        p(class = "text-muted small mt-1 mb-0",
                          "Los cuatro estratos corresponden a grupos socioecon\u00f3micos ",
                          "construidos a partir de la Encuesta Nacional de Fuerza de Trabajo (ENCFT): ",
                          "1 = m\u00e1s bajo, 4 = m\u00e1s alto. Permiten observar c\u00f3mo se distribuye ",
                          "el efecto de cada reforma entre segmentos de la poblaci\u00f3n m\u00e1s ",
                          "all\u00e1 del decil de ingreso."),
                        plotlyOutput("dash_inc_estr", height = "380px")),
              nav_panel("\u00c1rea",
                        plotlyOutput("dash_inc_urb", height = "400px")),
              nav_panel("Jefe de hogar",
                        plotlyOutput("dash_inc_sex", height = "400px")),
              nav_panel("Tipo de hogar",
                        plotlyOutput("dash_inc_cat", height = "400px")),
              nav_panel("Macroregiones",
                selectInput("dash_macro_esc", "Escenario para mapa",
                            choices = NULL, width = "100%"),
                card(card_header("Incidencia neta por macrorregi\u00f3n"),
                     DTOutput("dash_tbl_reg", height = "420px"))
              )
            )
          ),
          nav_panel(
            title = "Por instrumento",
            p(class = "small text-muted",
              "Incidencia por instrumento: efecto neto, ISR, ITBIS, ",
              "subsidio el\u00e9ctrico y compensaci\u00f3n."),
            navset_card_pill(
              id = "instrumento_sub",
              nav_panel("Efecto neto",
                        plotlyOutput("dash_inst_net_dec", height = "400px")),
              nav_panel("ISR",
                        plotlyOutput("dash_inst_isr_dec", height = "400px")),
              nav_panel("ITBIS",
                        plotlyOutput("dash_inst_itb_dec", height = "400px")),
              nav_panel("Subsidio el\u00e9ctrico",
                        plotlyOutput("dash_inst_sub_dec", height = "400px")),
              nav_panel("Compensaci\u00f3n",
                        plotlyOutput("dash_inst_comp_dec", height = "400px"))
            )
          ),
          nav_panel(
            title = "Glosario",
            navset_card_pill(
              id = "glosario_sub",
              nav_panel("Escenarios activos",
                        DTOutput("dash_tbl_glos_esc", height = "240px")),
              nav_panel("Indicadores",
                        DTOutput("dash_tbl_glos_ind", height = "320px")),
              nav_panel("Grupos de an\u00e1lisis",
                        DTOutput("dash_tbl_glos_grp", height = "280px")),
              nav_panel("Variables",
                        DTOutput("dash_tbl_glos_var", height = "360px"))
            )
          ),
          nav_panel(
            title = "Verificación",
            icon  = icon("flask"),
            div(class = "p-3",
              p(class = "text-muted",
                "Descarga los resultados completos de la simulación en formato ",
                tags$code(".rds"), " para verificación técnica.",
                "El archivo incluye los resúmenes de escenarios (pobreza, ",
                "desigualdad, incidencia) y la base de microdatos completa, ",
                "en la estructura que produce el flujo de análisis original."
              ),
              p(class = "text-muted small",
                "Ejemplo de uso en R: \n",
                tags$code('simul <- readRDS("simfi_resultados_2026-06-04.rds")'),
                br(),
                tags$code("simul$resultados_escenarios$escenario_1$pov_gral"),
                br(),
                tags$code("simul$dom_results |> dplyr::select(hhid, yz_1_pc, comp_1_pc)")
              ),
              downloadButton("btn_export_rds",
                             label = " Exportar resultados (.rds)",
                             icon  = icon("download"),
                             class = "btn-outline-secondary"),
              p(class = "small text-muted mt-2",
                "Ejecute la microsimulaci\u00f3n antes de descargar.")
            )
          )
        )
      ),
      nav_row(prev_id = "nav_6_prev", prev_label = "\u2190 Volver a Revisar")
    )
  ),

  nav_panel(
    "Acerca de",
    p("Este panel construye escenarios fiscales paso a paso y ejecuta la ",
      "microsimulaci\u00f3n en este equipo. Pueden definirse hasta cuatro escenarios ",
      "y compararse en tablas y gr\u00e1ficos. Los resultados son comparables con ",
      "los informes de pol\u00edtica fiscal del equipo.")
  )
)

# Servidor
server <- function(input, output, session) {

  # Valores reactivos
  tax_rv <- reactiveValues(
    rate_grupo    = list(),
    rate_subclase = list(),
    rate_variedad = list()
  )

  # Bibliotecas de componentes (nombre -> payload) y lista de escenarios compuestos.
  comp_lib <- reactiveValues(itbis = list(), isr = list(),
                             sub = list(), comp = list())
  # Componente "activo" por pantalla: nombre del último guardado/cargado.
  active_lib <- reactiveValues(itbis = "", isr = "", sub = "", comp = "")
  scen_rv  <- reactiveValues(scenarios = list())
  scen_counter <- reactiveVal(0L)
  # Selección "Comparar" desacoplada de la lista de tarjetas: alternar la casilla
  # NO debe re-renderizar scenarios_display (evita el parpadeo). Guarda uids.
  cmp_rv <- reactiveValues(sel = integer(0))
  # Permite forzar un re-render puntual de las tarjetas (p. ej. al rechazar
  # una cuarta selección) sin depender de cmp_rv$sel.
  render_nonce <- reactiveVal(0L)

  # Forma canónica para comparar payloads (ignora orden de nombres y redondea).
  .payload_canon <- function(x) {
    if (is.list(x)) {
      if (!is.null(names(x)) && length(x)) x <- x[order(names(x))]
      lapply(x, .payload_canon)
    } else if (is.numeric(x)) {
      round(as.numeric(x), 6)
    } else {
      x
    }
  }
  payload_equal <- function(a, b) {
    if (is.null(a) || is.null(b)) return(FALSE)
    identical(.payload_canon(a), .payload_canon(b))
  }

  paths <- reactive(get_dom_paths(root))

  # --- Captura del estado actual de cada pantalla como payload de componente ---
  capture_itbis <- function() {
    list(marco = "actual",
         rate_grupo    = as.list(tax_rv$rate_grupo),
         rate_subclase = as.list(tax_rv$rate_subclase),
         rate_variedad = as.list(tax_rv$rate_variedad))
  }
  capture_isr <- function() {
    rd <- function(stem) vapply(1:6, function(i) {
      x <- input[[paste0(stem, i)]]
      if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
    }, numeric(1))
    list(custom = isTRUE(input$custom_isr),
         nom_renta = input$nom_renta %||% "Personalizada",
         lim_inf = rd("isr_li_"), lim_sup = rd("isr_ls_"),
         tasa_pct = rd("isr_tp_"))
  }
  capture_sub <- function() {
    sv <- function(pref) vapply(1:7, function(j) {
      x <- input[[paste0("sub_", pref, "_", j)]]
      if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
    }, numeric(1))
    list(custom = isTRUE(input$custom_sub_ele),
         nom_subsidio = input$sub_nom %||% "",
         block = sv("block"), bsur = sv("bsur"), bnorte = sv("bnorte"),
         beste = sv("beste"), tsur = sv("tsur"), tnorte = sv("tnorte"),
         teste = sv("teste"),
         costsur = suppressWarnings(as.numeric(input$sub_costsur)),
         costnorte = suppressWarnings(as.numeric(input$sub_costnorte)),
         costeste = suppressWarnings(as.numeric(input$sub_costeste)),
         otsur = suppressWarnings(as.numeric(input$sub_otsur)),
         otnorte = suppressWarnings(as.numeric(input$sub_otnorte)),
         oteste = suppressWarnings(as.numeric(input$sub_oteste)))
  }
  capture_comp <- function() {
    if (!isTRUE(input$comp_con_comp)) {
      return(list(enabled = FALSE, sin_compensacion = TRUE))
    }
    int_or_na <- function(x) {
      if (is.null(x) || !nzchar(as.character(x)[1])) NA_integer_
      else suppressWarnings(as.integer(x))
    }
    list(enabled = TRUE, sin_compensacion = FALSE, sim_comp_id = 1L,
         grupo_com = as.integer(input$comp_grupo),
         metodo_com = as.integer(input$comp_metodo),
         valor_com = as.numeric(input$comp_valor %||% 0),
         decil_com = int_or_na(input$comp_decil_com),
         decil_est = int_or_na(input$comp_decil_est),
         icv_com = suppressWarnings(as.numeric(input$comp_icv)))
  }

  # --- Aplicar un payload guardado a los inputs en pantalla ---
  apply_itbis <- function(p) {
    tax_rv$rate_grupo    <- normalize_rate_list(p$rate_grupo %||% list())
    tax_rv$rate_subclase <- normalize_rate_list(p$rate_subclase %||% list())
    tax_rv$rate_variedad <- normalize_rate_list(p$rate_variedad %||% list())
  }
  apply_isr <- function(p) {
    cust <- isTRUE(p$custom)
    updateCheckboxInput(session, "custom_isr", value = cust)
    if (cust) {
      updateTextInput(session, "nom_renta",
                      value = p$nom_renta %||% "Personalizada")
      for (i in 1:6) {
        vi <- suppressWarnings(as.numeric(p$lim_inf[i]))
        vs <- suppressWarnings(as.numeric(p$lim_sup[i]))
        vp <- suppressWarnings(as.numeric(p$tasa_pct[i]))
        updateNumericInput(session, paste0("isr_li_", i),
                           value = if (is.finite(vi)) vi else NA)
        updateNumericInput(session, paste0("isr_ls_", i),
                           value = if (is.finite(vs)) vs else NA)
        updateNumericInput(session, paste0("isr_tp_", i),
                           value = if (is.finite(vp)) vp else NA)
      }
    }
  }
  apply_sub <- function(p) {
    cust <- isTRUE(p$custom)
    updateCheckboxInput(session, "custom_sub_ele", value = cust)
    if (cust) {
      upd7 <- function(nm, key) {
        v <- p[[key]]
        for (j in 1:7) {
          vj <- suppressWarnings(as.numeric(v[j]))
          updateNumericInput(session, paste0(nm, j),
                             value = if (is.finite(vj)) vj else NA)
        }
      }
      upd7("sub_block_", "block")
      upd7("sub_bsur_", "bsur");   upd7("sub_bnorte_", "bnorte")
      upd7("sub_beste_", "beste"); upd7("sub_tsur_", "tsur")
      upd7("sub_tnorte_", "tnorte"); upd7("sub_teste_", "teste")
      updateTextInput(session, "sub_nom", value = p$nom_subsidio %||% "")
      for (cx in .sub_sca_fields) {
        v <- suppressWarnings(as.numeric(p[[cx]]))
        updateNumericInput(session, paste0("sub_", cx),
                           value = if (is.finite(v)) v else NA)
      }
    }
  }
  apply_comp <- function(p) {
    enabled <- isTRUE(p$enabled) && !isTRUE(p$sin_compensacion)
    updateCheckboxInput(session, "comp_con_comp", value = enabled)
    if (enabled) {
      if (!is.na(p$grupo_com))
        updateSelectInput(session, "comp_grupo",
                          selected = as.character(p$grupo_com))
      if (!is.na(p$metodo_com))
        updateSelectInput(session, "comp_metodo",
                          selected = as.character(p$metodo_com))
      updateNumericInput(session, "comp_valor",
                         value = suppressWarnings(as.numeric(p$valor_com %||% 0)))
      if (!is.na(p$decil_est))
        updateSelectInput(session, "comp_decil_est",
                          selected = as.character(p$decil_est))
      if (!is.na(p$decil_com))
        updateSelectInput(session, "comp_decil_com",
                          selected = as.character(p$decil_com))
      iv <- suppressWarnings(as.integer(p$icv_com))
      if (!is.na(iv) && iv >= 1L && iv <= 4L)
        updateSelectInput(session, "comp_icv", selected = as.character(iv))
    }
  }

  # --- Componer un escenario (registro con nombres) en insumos completos ---
  compose_scenario_inputs <- function(scn) {
    # "__ref__" y "" son ambos centinelas para "sin componente / pre-reforma".
    # Normalizar aquí garantiza compatibilidad con sesiones guardadas antiguas.
    ckey <- function(v) { v <- v %||% ""; if (identical(v, "__ref__")) "" else v }
    itbis_p <- comp_lib$itbis[[ckey(scn$itbis)]] %||%
      list(marco = "actual", rate_grupo = list(),
           rate_subclase = list(), rate_variedad = list())
    isr_c   <- comp_lib$isr[[ckey(scn$isr)]]
    isr_full <- NULL
    if (!is.null(isr_c) && isTRUE(isr_c$custom)) {
      pv <- pipeline_isr_from_brackets(isr_c$lim_inf, isr_c$lim_sup, isr_c$tasa_pct)
      isr_full <- list(custom = TRUE, tramos = pv$tramos, bases = pv$bases,
                       tasas = pv$tasas, nom_renta = isr_c$nom_renta %||% "Personalizada",
                       lim_inf = isr_c$lim_inf, lim_sup = isr_c$lim_sup,
                       tasa_pct = isr_c$tasa_pct)
    }
    sub_p  <- comp_lib$sub[[ckey(scn$sub)]] %||% list(custom = FALSE)
    comp_p <- comp_lib$comp[[ckey(scn$comp)]] %||%
      list(enabled = FALSE, sin_compensacion = TRUE)
    nm <- scn$name %||% "Escenario"
    list(label = nm, des_corto = nm, des_escenario = nm, escenario = 1L,
         sim_itbis = 1L, custom_isr = isTRUE(isr_full$custom),
         sim_renta = if (isTRUE(isr_full$custom)) 1L else 0L,
         sim_sub = 1L,
         sim_com = if (isTRUE(comp_p$enabled) && !isTRUE(comp_p$sin_compensacion)) 1L else 0L,
         itbis = itbis_p, isr = isr_full, comp = comp_p, sub_ele = sub_p)
  }

  # --- Registro de bibliotecas por pantalla (guardar / cargar) ---
  register_component_lib <- function(prefix, lib_name, capture_fn, apply_fn) {
    store_under <- function(nm) {
      l <- comp_lib[[lib_name]]; l[[nm]] <- capture_fn()
      comp_lib[[lib_name]] <- l
      active_lib[[prefix]] <- nm
      updateTextInput(session, paste0("lib_", prefix, "_name"), value = nm)
    }
    # "Guardar" / "Actualizar": si hay un componente activo, lo sobreescribe;
    # si no, crea uno nuevo con el nombre escrito.
    observeEvent(input[[paste0("lib_", prefix, "_save")]], {
      act <- active_lib[[prefix]]
      if (nzchar(act %||% "")) {
        store_under(act)
        showNotification(paste0("Actualizado: \u201c", act, "\u201d."),
                         type = "message")
      } else {
        nm <- trimws(input[[paste0("lib_", prefix, "_name")]] %||% "")
        if (!nzchar(nm)) {
          showNotification("Asigne un nombre para guardar.", type = "warning")
          return(NULL)
        }
        store_under(nm)
        showNotification(paste0("Guardado: \u201c", nm, "\u201d."), type = "message")
      }
    })
    # "Guardar como nuevo": crea (o duplica) bajo el nombre escrito.
    observeEvent(input[[paste0("lib_", prefix, "_saveas")]], {
      nm <- trimws(input[[paste0("lib_", prefix, "_name")]] %||% "")
      if (!nzchar(nm)) {
        showNotification("Escriba un nombre para el nuevo componente.",
                         type = "warning")
        return(NULL)
      }
      existed <- nm %in% names(comp_lib[[lib_name]])
      store_under(nm)
      showNotification(
        paste0(if (existed) "Reemplazado: \u201c" else "Guardado: \u201c",
               nm, "\u201d."),
        type = "message")
    })
    observe({
      ch <- c("\u2014 elegir \u2014" = "", names(comp_lib[[lib_name]]))
      updateSelectInput(session, paste0("lib_", prefix, "_pick"), choices = ch)
    })
    observeEvent(input[[paste0("lib_", prefix, "_load")]], {
      nm <- input[[paste0("lib_", prefix, "_pick")]]
      if (is.null(nm) || !nzchar(nm)) {
        showNotification("Elija una configuraci\u00f3n guardada.", type = "warning")
        return(NULL)
      }
      p <- comp_lib[[lib_name]][[nm]]
      if (is.null(p)) return(NULL)
      apply_fn(p)
      active_lib[[prefix]] <- nm
      updateTextInput(session, paste0("lib_", prefix, "_name"), value = nm)
      showNotification(paste0("Cargado: \u201c", nm, "\u201d."), type = "message")
    })
    # Eliminar el componente seleccionado del desplegable.
    observeEvent(input[[paste0("lib_", prefix, "_del")]], {
      nm <- input[[paste0("lib_", prefix, "_pick")]]
      if (is.null(nm) || !nzchar(nm)) {
        showNotification("Seleccione un componente para eliminar.", type = "warning")
        return(NULL)
      }
      l <- comp_lib[[lib_name]]
      l[[nm]] <- NULL
      comp_lib[[lib_name]] <- l
      # Clear active state if we just deleted the active component.
      if (identical(active_lib[[prefix]], nm)) {
        active_lib[[prefix]] <- ""
        updateTextInput(session, paste0("lib_", prefix, "_name"), value = "")
      }
      showNotification(paste0("Eliminado: \u201c", nm, "\u201d."), type = "message")
    })
    # Etiqueta dinámica del botón principal: "Guardar" vs "Actualizar".
    observe({
      lbl <- if (nzchar(active_lib[[prefix]] %||% "")) "Actualizar" else "Guardar"
      updateActionButton(session, paste0("lib_", prefix, "_save"), label = lbl)
    })
    # Indicador de estado del componente activo.
    output[[paste0("lib_", prefix, "_status")]] <- renderUI({
      act <- active_lib[[prefix]]
      if (!nzchar(act %||% "")) {
        return(tags$div(class = "small text-muted mb-2",
                        icon("circle-plus"),
                        " Componente nuevo \u00b7 sin guardar en la biblioteca."))
      }
      stored <- comp_lib[[lib_name]][[act]]
      if (payload_equal(capture_fn(), stored)) {
        tags$div(class = "small text-success mb-2",
                 icon("circle-check"),
                 paste0(" Editando \u201c", act, "\u201d \u00b7 guardado."))
      } else {
        tags$div(class = "small text-warning-emphasis mb-2",
                 icon("triangle-exclamation"),
                 paste0(" Editando \u201c", act,
                        "\u201d \u00b7 cambios sin guardar."))
      }
    })
  }
  register_component_lib("itbis", "itbis", capture_itbis, apply_itbis)
  register_component_lib("isr",   "isr",   capture_isr,   apply_isr)
  register_component_lib("sub",   "sub",   capture_sub,   apply_sub)
  register_component_lib("comp",  "comp",  capture_comp,  apply_comp)

  # Opciones de los desplegables del compositor (nombres de cada biblioteca).
  # "__ref__" es el centinela para "sin componente / pre-reforma" y es un
  # valor no vacío que selectize.js puede re-seleccionar en cualquier momento.
  observe({
    ref <- c("Par\u00e1metros pre-reforma (referencia)" = "__ref__")
    updateSelectInput(session, "compose_itbis",
                      choices = c(ref, names(comp_lib$itbis)))
    updateSelectInput(session, "compose_isr",
                      choices = c(ref, names(comp_lib$isr)))
    updateSelectInput(session, "compose_sub",
                      choices = c(ref, names(comp_lib$sub)))
    updateSelectInput(session, "compose_comp",
                      choices = c(ref, names(comp_lib$comp)))
  })

  # --- Alta de escenarios + observadores por escenario (uid estable) ---
  n_comparar <- function() length(cmp_rv$sel)
  register_scenario_obs <- function(uid) {
    observeEvent(input[[paste0("scen_del_", uid)]], {
      scen_rv$scenarios <- Filter(function(s) !identical(s$uid, uid),
                                  scen_rv$scenarios)
      cmp_rv$sel <- setdiff(cmp_rv$sel, uid)
      showNotification("Escenario eliminado.", type = "warning")
    })
    observeEvent(input[[paste0("scen_cmp_", uid)]], {
      on  <- isTRUE(input[[paste0("scen_cmp_", uid)]])
      cur <- cmp_rv$sel
      if (on) {
        if (uid %in% cur) return(NULL)
        if (length(cur) >= 4L) {
          showNotification("Solo puede comparar hasta 4 escenarios.",
                           type = "warning")
          # Revertir la casilla en el DOM sin re-renderizar por cmp_rv$sel.
          render_nonce(render_nonce() + 1L)
          return(NULL)
        }
        cmp_rv$sel <- c(cur, uid)
      } else {
        cmp_rv$sel <- setdiff(cur, uid)
      }
    }, ignoreInit = TRUE)
  }
  add_scenario <- function(rec) {
    uid <- scen_counter() + 1L
    scen_counter(uid)
    want_cmp <- if (is.null(rec$comparar)) (n_comparar() < 4L)
                else isTRUE(rec$comparar)
    rec$comparar <- NULL
    rec$uid <- uid
    scen_rv$scenarios <- c(scen_rv$scenarios, list(rec))
    if (want_cmp && n_comparar() < 4L) cmp_rv$sel <- c(cmp_rv$sel, uid)
    register_scenario_obs(uid)
    uid
  }

  # --- Restablecer todas las pantallas a la situación vigente (GEN-1) ---
  reset_form_to_base <- function() {
    tax_rv$rate_grupo <- list()
    tax_rv$rate_subclase <- list()
    tax_rv$rate_variedad <- list()
    updateCheckboxInput(session, "custom_isr", value = FALSE)
    updateCheckboxInput(session, "custom_sub_ele", value = FALSE)
    updateCheckboxInput(session, "comp_con_comp", value = FALSE)
    for (i in 1:6) {
      updateNumericInput(session, paste0("isr_li_", i),
                         value = isr_base_defaults$lim_inf[i])
      updateNumericInput(session, paste0("isr_ls_", i),
                         value = isr_base_defaults$lim_sup[i])
      updateNumericInput(session, paste0("isr_tp_", i),
                         value = isr_base_defaults$tasa_pct[i])
    }
    updateNumericInput(session, "tasa_aplicar", value = NA)
    for (px in c("itbis", "isr", "sub", "comp")) {
      active_lib[[px]] <- ""
      updateTextInput(session, paste0("lib_", px, "_name"), value = "")
    }
  }

  # --- Restaurar autoguardado al iniciar la sesión ---
  isolate({
    rs <- scenario_autosave_read(root)
    if (!is.null(rs)) {
      comp_lib$itbis <- rs$libs$itbis %||% list()
      comp_lib$isr   <- rs$libs$isr   %||% list()
      comp_lib$sub   <- rs$libs$sub   %||% list()
      comp_lib$comp  <- rs$libs$comp  %||% list()
      for (s in (rs$scenarios %||% list())) {
        s$uid <- NULL
        add_scenario(s)
      }
      n_scen <- length(rs$scenarios %||% list())
      n_comp <- sum(vapply(list(rs$libs$itbis, rs$libs$isr,
                               rs$libs$sub,   rs$libs$comp),
                           function(l) length(l %||% list()), integer(1)))
      if (n_scen > 0 || n_comp > 0) {
        showNotification(
          paste0("Sesi\u00f3n anterior restaurada: ", n_scen, " escenario(s) y ",
                 n_comp, " componente(s) en las bibliotecas. ",
                 "Para empezar de cero use \u201cNueva sesi\u00f3n\u201d en 5 \u00b7 Revisar."),
          type = "message", duration = 10)
      }
    }
  })

  # --- Autoguardado ante cualquier cambio ---
  observe({
    libs <- list(itbis = comp_lib$itbis, isr = comp_lib$isr,
                 sub = comp_lib$sub, comp = comp_lib$comp)
    sel  <- cmp_rv$sel
    scs  <- lapply(scen_rv$scenarios, function(s) {
      s$comparar <- s$uid %in% sel
      s$uid <- NULL
      s
    })
    scenario_autosave_write(root, libs, scs)
  })

  # Navegación del asistente
  go_to <- function(panel_title)
    updateNavbarPage(session, "main_nav", selected = panel_title)

  observeEvent(input$nav_1_next,    go_to("2 \u00b7 Renta"))
  observeEvent(input$nav_2_prev,    go_to("1 \u00b7 ITBIS"))
  observeEvent(input$nav_2_next,    go_to("3 \u00b7 Subsidio"))
  observeEvent(input$nav_3_prev,    go_to("2 \u00b7 Renta"))
  observeEvent(input$nav_3_next,    go_to("4 \u00b7 Compensaci\u00f3n"))
  observeEvent(input$nav_4_prev,    go_to("3 \u00b7 Subsidio"))
  observeEvent(input$nav_4_next,    go_to("5 \u00b7 Revisar"))
  observeEvent(input$nav_5_prev,    go_to("4 \u00b7 Compensaci\u00f3n"))
  observeEvent(input$nav_5_next,    go_to("6 \u00b7 Simular"))
  observeEvent(input$nav_6_prev,    go_to("5 \u00b7 Revisar"))

  # Renta: sin huecos entre tramos. El límite superior de un tramo es siempre el
  # límite inferior del siguiente; se sincronizan en ambos sentidos.
  isr_vals_equal <- function(a, b) {
    a_na <- is.null(a) || length(a) != 1L || is.na(a)
    b_na <- is.null(b) || length(b) != 1L || is.na(b)
    if (a_na && b_na) return(TRUE)
    if (a_na || b_na) return(FALSE)
    isTRUE(a == b)
  }
  lapply(1:5, function(i) {
    ls_id <- paste0("isr_ls_", i)
    li_id <- paste0("isr_li_", i + 1L)
    observeEvent(input[[ls_id]], {
      if (!isr_vals_equal(input[[ls_id]], input[[li_id]]))
        updateNumericInput(session, li_id, value = input[[ls_id]])
    }, ignoreInit = TRUE)
    observeEvent(input[[li_id]], {
      if (!isr_vals_equal(input[[li_id]], input[[ls_id]]))
        updateNumericInput(session, ls_id, value = input[[li_id]])
    }, ignoreInit = TRUE)
  })

  # Sin desplegables pick_sim_sub y pick_sim_com: políticas desde fila referencia
  # (sim_sub = 1, sim_com = 1) o definidas por el usuario con las casillas de personalización.

  # Selects ITBIS en cascada
  observe({
    req(input$itbis_grupo)
    cat <- itbis_catalog
    sub <- cat %>%
      filter(.data$COD_GRUPO == input$itbis_grupo) %>%
      distinct(.data$COD_SUBGRUPO, .data$DES_SUBGRUPO,
               .data$COD_CLASE,    .data$DES_CLASE,
               .data$COD_SUBCLASE, .data$DES_SUBCLASE) %>%
      arrange(.data$COD_SUBGRUPO, .data$COD_CLASE, .data$COD_SUBCLASE)
    sub$sk <- paste(input$itbis_grupo, sub$COD_SUBGRUPO,
                    sub$COD_CLASE, sub$COD_SUBCLASE, sep = "|")
    ch <- stats::setNames(
      sub$sk,
      paste0(sub$COD_SUBGRUPO, ": ", sub$DES_SUBGRUPO,
             " \u00b7 ", sub$COD_CLASE, ": ", sub$DES_CLASE,
             " \u00b7 ", sub$COD_SUBCLASE, ": ", sub$DES_SUBCLASE)
    )
    updateSelectInput(session, "itbis_subclase",
                      choices = ch, selected = if (length(ch)) names(ch)[1])
  })

  observe({
    req(input$itbis_subclase)
    parts <- strsplit(input$itbis_subclase, "|", fixed = TRUE)[[1]]
    if (length(parts) != 4) return(NULL)
    rows <- itbis_catalog %>%
      filter(.data$COD_GRUPO    == parts[1],
             .data$COD_SUBGRUPO == parts[2],
             .data$COD_CLASE    == parts[3],
             .data$COD_SUBCLASE == parts[4]) %>%
      arrange(.data$COD_ARTICULO, .data$ID_VARIEDAD)
    rk  <- vapply(seq_len(nrow(rows)),
                  function(i) itbis_row_key(rows[i, , drop = FALSE]),
                  character(1))
    lab <- paste0(rows$COD_ARTICULO, ": ", rows$DES_ARTICULO,
                  " \u00b7 ", rows$ID_VARIEDAD, ": ", rows$DES_VARIEDAD)
    chvx <- stats::setNames(rk, lab)
    updateSelectizeInput(session, "itbis_variedad",
                         choices  = chvx,
                         selected = if (length(chvx)) names(chvx)[1],
                         server   = TRUE)
  })

  # Tasa actual del producto/subclase seleccionado (para saber sobre qué se modifica)
  output$itbis_sel_info <- renderUI({
    rk <- input$itbis_variedad
    if (is.null(rk) || !nzchar(rk)) return(NULL)
    idx <- match(rk, itbis_row_keys)
    if (is.na(idx)) return(NULL)
    row <- itbis_catalog[idx, , drop = FALSE]
    ley <- suppressWarnings(as.numeric(row$tasa))
    eff <- tasa_efectiva_desde_listas(
      row, tax_rv$rate_variedad, tax_rv$rate_subclase,
      tax_rv$rate_grupo, "actual"
    )
    cambiado <- is.finite(ley) && abs(eff - ley) > 1e-6
    div(
      class = "alert alert-light border py-1 px-2 mb-2 small",
      tags$strong("Tasa actual de la selecci\u00f3n: "),
      tags$span(class = "badge bg-secondary",
                paste0(round(ley, 2), "%")),
      if (cambiado) tagList(
        tags$span(class = "mx-1", "\u2192"),
        tags$span(class = "badge bg-primary",
                  paste0(round(eff, 2), "% en este escenario"))
      )
    )
  })

  # Aplicar tasas ITBIS
  observeEvent(input$btn_aplicar_tasa, {
    req(input$itbis_nivel_aplicar, input$itbis_grupo)
    lvl  <- input$itbis_nivel_aplicar
    rate <- suppressWarnings(as.numeric(input$tasa_aplicar))
    if (length(rate) != 1L || is.na(rate)) {
      showNotification("Indique una nueva tasa antes de aplicar.",
                       type = "warning")
      return(NULL)
    }
    if (lvl == "grupo") {
      tax_rv$rate_grupo[[input$itbis_grupo]] <- rate
      showNotification("Tasa aplicada al grupo.", type = "message")
      return(NULL)
    }
    if (lvl == "subclase") {
      req(input$itbis_subclase)
      tax_rv$rate_subclase[[input$itbis_subclase]] <- rate
      showNotification("Tasa aplicada a la subclase.", type = "message")
      return(NULL)
    }
    req(input$itbis_variedad)
    tax_rv$rate_variedad[[input$itbis_variedad]] <- rate
    showNotification("Tasa aplicada a la variedad.", type = "message")
  })

  observeEvent(input$btn_limpiar_itbis, {
    tax_rv$rate_grupo    <- list()
    tax_rv$rate_subclase <- list()
    tax_rv$rate_variedad <- list()
    showNotification(
      "Se restablecieron todos los productos a los valores del escenario base.",
      type = "message"
    )
  })

  # Restablecer solo la selección actual (al nivel elegido en "Aplicar tasa a")
  observeEvent(input$btn_reset_sel, {
    req(input$itbis_nivel_aplicar, input$itbis_grupo)
    lvl <- input$itbis_nivel_aplicar
    if (lvl == "grupo") {
      rg <- tax_rv$rate_grupo; rg[[input$itbis_grupo]] <- NULL
      tax_rv$rate_grupo <- rg
    } else if (lvl == "subclase") {
      req(input$itbis_subclase)
      rs <- tax_rv$rate_subclase; rs[[input$itbis_subclase]] <- NULL
      tax_rv$rate_subclase <- rs
    } else {
      req(input$itbis_variedad)
      rv <- tax_rv$rate_variedad; rv[[input$itbis_variedad]] <- NULL
      tax_rv$rate_variedad <- rv
    }
    showNotification("Selecci\u00f3n restablecida.", type = "message")
  })

  observeEvent(input$btn_reset_rama, {
    req(input$itbis_grupo)
    gk   <- input$itbis_grupo
    pref <- paste0(gk, "|")
    rg   <- tax_rv$rate_grupo;  rg[[gk]] <- NULL; tax_rv$rate_grupo <- rg
    rs   <- tax_rv$rate_subclase
    ns   <- names(rs)
    if (length(ns)) {
      rs2 <- rs[!startsWith(ns, pref)]
      tax_rv$rate_subclase <- if (length(rs2)) as.list(rs2) else list()
    } else {
      tax_rv$rate_subclase <- list()
    }
    rv <- tax_rv$rate_variedad
    nv <- names(rv)
    if (length(nv)) {
      rv2 <- rv[!startsWith(nv, pref)]
      tax_rv$rate_variedad <- if (length(rv2)) as.list(rv2) else list()
    } else {
      tax_rv$rate_variedad <- list()
    }
    showNotification("Grupo restablecido.", type = "message")
  })

  # --- Selección por tasa (cruza grupos): productos con la tasa de referencia
  #     buscada. Aplicar una nueva tasa fija overrides a nivel de variedad. ---
  itbis_byrate_matches <- reactive({
    q <- suppressWarnings(as.numeric(input$itbis_query_tasa))
    if (length(q) != 1L || is.na(q)) {
      return(itbis_catalog[0, , drop = FALSE])
    }
    tasa_ley <- suppressWarnings(as.numeric(itbis_catalog$tasa))
    itbis_catalog[!is.na(tasa_ley) & abs(tasa_ley - q) < 1e-6, , drop = FALSE]
  })

  itbis_byrate_table <- reactive({
    m <- itbis_byrate_matches()
    tibble::tibble(
      Grupo      = m$DES_GRUPO,
      Producto   = paste0(m$ID_VARIEDAD, ": ", m$DES_VARIEDAD),
      `Tasa actual (%)` = round(suppressWarnings(as.numeric(m$tasa)), 2)
    )
  })

  output$tbl_itbis_byrate <- renderDT({
    datatable(
      itbis_byrate_table(), rownames = FALSE,
      selection = "multiple",
      options = list(dom = "ftip", scrollX = TRUE, pageLength = 8),
      class = "compact stripe hover"
    )
  })

  byrate_proxy <- dataTableProxy("tbl_itbis_byrate")
  observeEvent(input$btn_byrate_all, {
    n <- nrow(itbis_byrate_matches())
    if (n > 0L) selectRows(byrate_proxy, seq_len(n))
  })
  observeEvent(input$btn_byrate_none, {
    selectRows(byrate_proxy, NULL)
  })

  observeEvent(input$btn_byrate_aplicar, {
    m   <- itbis_byrate_matches()
    sel <- input$tbl_itbis_byrate_rows_selected
    if (is.null(sel) || !length(sel)) {
      showNotification("Seleccione al menos un producto.", type = "warning")
      return(NULL)
    }
    rate <- suppressWarnings(as.numeric(input$itbis_byrate_nueva))
    if (length(rate) != 1L || is.na(rate)) {
      showNotification("Indique una nueva tasa v\u00e1lida.", type = "warning")
      return(NULL)
    }
    rv <- tax_rv$rate_variedad
    for (i in sel) {
      rk <- itbis_row_key(m[i, , drop = FALSE])
      rv[[rk]] <- rate
    }
    tax_rv$rate_variedad <- rv
    showNotification(
      paste0("Tasa aplicada a ", length(sel), " producto(s)."),
      type = "message"
    )
  })

  # Tabla previa de cambios ITBIS
  tbl_itbis_preview <- reactive({
    d0 <- itbis_catalog
    n  <- nrow(d0)
    tasa_eff <- vapply(seq_len(n), function(i) {
      tasa_efectiva_desde_listas(
        d0[i, , drop = FALSE],
        tax_rv$rate_variedad, tax_rv$rate_subclase, tax_rv$rate_grupo,
        "actual"   # marco siempre actual, sin modo vacío
      )
    }, numeric(1))
    tasa_ley <- suppressWarnings(as.numeric(d0$tasa))
    delta    <- tasa_eff - tasa_ley
    orig <- vapply(seq_len(n), function(i) {
      row <- d0[i, , drop = FALSE]
      rk  <- itbis_row_key(row)
      sk  <- itbis_subclase_key(row)
      gk  <- as.character(row$COD_GRUPO)
      if (!is.null(tax_rv$rate_variedad[[rk]]))  return("variedad")
      if (!is.null(tax_rv$rate_subclase[[sk]]))  return("subclase")
      if (!is.null(tax_rv$rate_grupo[[gk]]))     return("grupo")
      "referencia"
    }, character(1))
    muestra <- (orig != "referencia") | (abs(delta) > 1e-6)
    tibble::tibble(
      Grupo    = d0$DES_GRUPO[muestra],
      Producto = paste(d0$ID_VARIEDAD[muestra],
                       d0$DES_VARIEDAD[muestra], sep = ": "),
      `Tasa vieja (%)` = round(tasa_ley[muestra], 2),
      `Tasa nueva (%)` = round(tasa_eff[muestra], 2)
    )
  })

  # Populate grupo filter on first load / catalog change
  observe({
    grupos <- sort(unique(itbis_catalog$DES_GRUPO))
    updateSelectInput(session, "prev_grupo_fil",
                      choices = c("Todos los grupos" = "__todos__",
                                  setNames(grupos, grupos)),
                      selected = "__todos__")
  })

  # Cascade: update subclase choices based on selected grupo
  observeEvent(input$prev_grupo_fil, {
    g <- input$prev_grupo_fil %||% "__todos__"
    subclases <- if (g == "__todos__") {
      sort(unique(itbis_catalog$DES_SUBCLASE))
    } else {
      sort(unique(itbis_catalog$DES_SUBCLASE[itbis_catalog$DES_GRUPO == g]))
    }
    updateSelectInput(session, "prev_subclase_fil",
                      choices = c("Todas las subclases" = "__todos__",
                                  setNames(subclases, subclases)),
                      selected = "__todos__")
  }, ignoreInit = TRUE)

  tbl_itbis_preview_filtered <- reactive({
    df <- tbl_itbis_preview()
    g  <- input$prev_grupo_fil    %||% "__todos__"
    sc <- input$prev_subclase_fil %||% "__todos__"
    if (g != "__todos__") {
      df <- df[df$Grupo == g, , drop = FALSE]
    }
    if (sc != "__todos__") {
      ctlg <- itbis_catalog
      allowed_prods <- paste(
        ctlg$ID_VARIEDAD[ctlg$DES_SUBCLASE == sc],
        ctlg$DES_VARIEDAD[ctlg$DES_SUBCLASE == sc],
        sep = ": "
      )
      df <- df[df$Producto %in% allowed_prods, , drop = FALSE]
    }
    df
  })

  output$tbl_itbis_resumen <- renderDT({
    datatable(tbl_itbis_preview_filtered(), rownames = FALSE,
              options = list(dom = "ftip", scrollX = TRUE, pageLength = 15),
              class = "compact stripe hover")
  })

  # Subsidio eléctrico: al personalizar, partir de las tarifas vigentes (base).
  observeEvent(input$custom_sub_ele, {
    if (!isTRUE(input$custom_sub_ele)) return(invisible(NULL))
    p <- paths()
    r <- read_sim_sub_template_row(p$param_csv, 0L)
    fill_sub_ele_inputs_from_row(session, r)
  })

  # Armar lista de insumos del escenario desde el estado actual de la UI
  collect_scenario_inputs <- reactive({
    isr <- NULL
    if (isTRUE(input$custom_isr)) {
      li <- vapply(1:6, function(i) {
        x <- input[[paste0("isr_li_", i)]]
        if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
      }, numeric(1))
      ls_v <- vapply(1:6, function(i) {
        x <- input[[paste0("isr_ls_", i)]]
        if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
      }, numeric(1))
      tp <- vapply(1:6, function(i) {
        x <- input[[paste0("isr_tp_", i)]]
        if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
      }, numeric(1))
      pv  <- pipeline_isr_from_brackets(li, ls_v, tp)
      isr <- list(
        custom     = TRUE,
        tramos     = pv$tramos,
        bases      = pv$bases,
        tasas      = pv$tasas,
        nom_renta  = input$nom_renta %||% "Personalizada",
        lim_inf    = li,
        lim_sup    = ls_v,
        tasa_pct   = tp
      )
    }

    # Compensación solo si el usuario marca Con compensación
    comp_sin <- !isTRUE(input$comp_con_comp)
    sim_com_val <- if (comp_sin) 0L else 1L
    comp_list <- if (comp_sin) {
      list(enabled = FALSE, sin_compensacion = TRUE)
    } else {
      dec_est <- input$comp_decil_est
      dec_est_i <- if (is.null(dec_est) || !nzchar(as.character(dec_est)[1])) {
        NA_integer_
      } else {
        suppressWarnings(as.integer(dec_est))
      }
      dec_c <- input$comp_decil_com
      dec_c_i <- if (is.null(dec_c) || !nzchar(as.character(dec_c)[1])) {
        NA_integer_
      } else {
        suppressWarnings(as.integer(dec_c))
      }
      list(
        enabled          = TRUE,
        sin_compensacion = FALSE,
        sim_comp_id      = 1L,
        grupo_com        = as.integer(input$comp_grupo),
        metodo_com       = as.integer(input$comp_metodo),
        valor_com        = as.numeric(input$comp_valor %||% 0),
        decil_com        = dec_c_i,
        decil_est        = dec_est_i,
        icv_com          = suppressWarnings(as.numeric(input$comp_icv))
      )
    }

    sub_ele <- list(custom = isTRUE(input$custom_sub_ele))
    if (isTRUE(input$custom_sub_ele)) {
      sv_pref <- function(pref) {
        vapply(1:7, function(j) {
          x <- input[[paste0("sub_", pref, "_", j)]]
          if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
        }, numeric(1))
      }
      sub_ele <- list(
        custom      = TRUE,
        block       = vapply(1:7, function(j) {
          x <- input[[paste0("sub_block_", j)]]
          if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x))
        }, numeric(1)),
        bsur        = sv_pref("bsur"),
        bnorte      = sv_pref("bnorte"),
        beste       = sv_pref("beste"),
        tsur        = sv_pref("tsur"),
        tnorte      = sv_pref("tnorte"),
        teste       = sv_pref("teste"),
        nom_subsidio = input$sub_nom %||% "",
        costsur     = suppressWarnings(as.numeric(input$sub_costsur)),
        costnorte   = suppressWarnings(as.numeric(input$sub_costnorte)),
        costeste    = suppressWarnings(as.numeric(input$sub_costeste)),
        otsur       = suppressWarnings(as.numeric(input$sub_otsur)),
        otnorte     = suppressWarnings(as.numeric(input$sub_otnorte)),
        oteste      = suppressWarnings(as.numeric(input$sub_oteste))
      )
    }

    nm <- "Escenario actual"
    list(
      label         = nm,
      des_corto     = nm,
      des_escenario = nm,
      escenario     = 1L,
      sim_itbis     = 1L,
      custom_isr    = isTRUE(input$custom_isr),
      sim_renta     = if (isTRUE(input$custom_isr)) 1L else 0L,
      sim_sub       = 1L,
      sim_com       = sim_com_val,
      itbis = list(
        marco         = "actual",
        rate_grupo    = as.list(tax_rv$rate_grupo),
        rate_subclase = as.list(tax_rv$rate_subclase),
        rate_variedad = as.list(tax_rv$rate_variedad)
      ),
      isr     = isr,
      comp    = comp_list,
      sub_ele = sub_ele
    )
  })

  # Empezar de cero (GEN-1): un escenario nuevo parte de la situación vigente.
  observeEvent(input$btn_nuevo_escenario, {
    reset_form_to_base()
    showNotification("Formulario restablecido a la situaci\u00f3n vigente.",
                     type = "message")
  })

  # Agregar un escenario compuesto a la lista
  observeEvent(input$btn_compose_add, {
    nm <- trimws(input$compose_name %||% "")
    if (!nzchar(nm)) {
      showNotification("Asigne un nombre al escenario.", type = "warning")
      return(NULL)
    }
    add_scenario(list(
      name  = nm,
      itbis = input$compose_itbis %||% "",
      isr   = input$compose_isr   %||% "",
      sub   = input$compose_sub   %||% "",
      comp  = input$compose_comp  %||% ""
    ))
    updateTextInput(session, "compose_name", value = "")
    # Limpiar los desplegables para que el próximo escenario parta de referencia.
    for (drop_id in c("compose_itbis", "compose_isr", "compose_sub", "compose_comp")) {
      updateSelectInput(session, drop_id, selected = "__ref__")
    }
    showNotification(paste0("Escenario \u201c", nm, "\u201d agregado."),
                     type = "message")
  })

  # Restablecer todo (GEN-2): borra bibliotecas, escenarios y el formulario.
  observeEvent(input$btn_reset_all, {
    showModal(modalDialog(
      title = "Restablecer todo",
      "Se borrar\u00e1n todas las configuraciones guardadas, los escenarios y los ",
      "valores en pantalla, y se volver\u00e1 a la situaci\u00f3n vigente. ",
      "Esta acci\u00f3n no se puede deshacer.",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_reset_all_ok", "S\u00ed, restablecer todo",
                     class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$btn_reset_all_ok, {
    removeModal()
    comp_lib$itbis <- list(); comp_lib$isr <- list()
    comp_lib$sub <- list();   comp_lib$comp <- list()
    scen_rv$scenarios <- list()
    cmp_rv$sel <- integer(0)
    reset_form_to_base()
    scenario_autosave_clear(root)
    showNotification("Todo restablecido.", type = "warning")
  })

  output$compare_count <- renderUI(
    paste0(": ", n_comparar(), " de 4 marcados para comparar")
  )

  # Tarjetas de escenarios guardados (compositor)
  output$scenarios_display <- renderUI({
    render_nonce()  # dependencia para forzar re-render puntual
    scs <- scen_rv$scenarios
    sel <- isolate(cmp_rv$sel)  # estado de comparación sin crear dependencia
    if (!length(scs)) {
      return(tags$p(class = "text-muted fst-italic",
                    "A\u00fan no hay escenarios. Componga uno arriba."))
    }
    cmp_label <- function(lib, key) {
      if (is.null(key) || !nzchar(key)) "Referencia" else key
    }
    tagList(lapply(scs, function(sc) {
      uid <- sc$uid
      full <- compose_scenario_inputs(sc)
      card(
        class = "mb-2 slot-card filled",
        card_body(
          class = "py-2",
          tags$div(
            class = "d-flex justify-content-between align-items-start",
            tags$div(
              tags$strong(sc$name %||% "Escenario"),
              tags$div(
                class = "text-muted small mt-1",
                paste0("ITBIS: ", cmp_label("itbis", sc$itbis),
                       " | Renta: ", cmp_label("isr", sc$isr),
                       " | Subsidio: ", cmp_label("sub", sc$sub),
                       " | Compensaci\u00f3n: ", cmp_label("comp", sc$comp))
              ),
              tags$details(
                class = "mt-2",
                tags$summary(class = "text-primary small",
                             style = "cursor:pointer;", "Ver detalle"),
                tags$div(class = "mt-2", slot_detail_ui(full))
              )
            ),
            tags$div(
              class = "d-flex flex-column align-items-end gap-1",
              div(class = "form-check",
                tags$input(type = "checkbox", class = "form-check-input",
                           id = paste0("scen_cmp_", uid),
                           checked = if (uid %in% sel) "checked",
                           onclick = sprintf(
                             "Shiny.setInputValue('scen_cmp_%s', this.checked, {priority:'event'})",
                             uid)),
                tags$label(class = "form-check-label small",
                           `for` = paste0("scen_cmp_", uid), "Comparar")),
              actionButton(paste0("scen_del_", uid), icon("trash"),
                           class = "btn-sm btn-outline-danger",
                           title = "Eliminar escenario")
            )
          )
        )
      )
    }))
  })

  # Resumen de escenarios marcados en pantalla Simular
  output$run_slots_summary <- renderUI({
    sel    <- cmp_rv$sel
    marked <- Filter(function(s) s$uid %in% sel, scen_rv$scenarios)
    if (!length(marked)) {
      if (!length(scen_rv$scenarios)) {
        return(tags$span(class = "text-warning",
          "No hay escenarios. Se ejecutar\u00e1 el estado actual del formulario."))
      }
      return(tags$span(class = "text-warning",
        "Ning\u00fan escenario marcado para comparar en la pantalla 'Revisar'."))
    }
    tagList(lapply(marked, function(s) {
      tags$div(class = "scenario-chip",
               tags$strong(paste0(s$name %||% "Escenario", "")))
    }))
  })

  # Normalización de listas de tasas (importación / carga de componentes)
  normalize_rate_list <- function(x) {
    if (is.null(x) || !is.list(x)) return(list())
    nm  <- names(x)
    out <- list()
    for (i in seq_along(x)) {
      z <- x[[i]]
      v <- if (is.numeric(z)) as.numeric(z)[[1L]]
          else if (is.list(z) && length(z)) suppressWarnings(as.numeric(z[[1L]]))
          else suppressWarnings(as.numeric(z))
      if (length(v) == 1L && is.finite(v)) {
        key <- if (!is.null(nm) && nzchar(nm[i])) nm[i]
               else as.character(length(out) + 1L)
        out[[key]] <- v
      }
    }
    out
  }

  # Exportar todo (bibliotecas + escenarios) a un Excel multipestaña.
  output$dl_xlsx <- downloadHandler(
    filename = function()
      paste0("escenarios_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      libs <- list(itbis = comp_lib$itbis, isr = comp_lib$isr,
                   sub = comp_lib$sub, comp = comp_lib$comp)
      scs  <- lapply(scen_rv$scenarios, function(s) { s$uid <- NULL; s })
      scenario_store_save_xlsx(file, libs, scs, catalog = itbis_catalog)
    }
  )

  # Importar bibliotecas + escenarios desde Excel (reemplaza lo actual).
  observeEvent(input$up_xlsx, {
    req(input$up_xlsx)
    st <- tryCatch(scenario_store_read_xlsx(input$up_xlsx$datapath),
                   error = function(e) NULL)
    if (is.null(st)) {
      showNotification("No se pudo leer el archivo Excel.", type = "error")
      return(NULL)
    }
    comp_lib$itbis <- st$libs$itbis %||% list()
    comp_lib$isr   <- st$libs$isr   %||% list()
    comp_lib$sub   <- st$libs$sub   %||% list()
    comp_lib$comp  <- st$libs$comp  %||% list()
    scen_rv$scenarios <- list()
    for (s in (st$scenarios %||% list())) {
      s$uid <- NULL
      add_scenario(s)
    }
    showNotification(
      paste0("Importado: ", length(st$scenarios %||% list()),
             " escenario(s) y sus componentes."),
      type = "message")
  }, ignoreInit = TRUE)

  # Ejecutar uno o más escenarios en secuencia
  run_multi_dom <- function(slot_list, paths) {
    filled <- Filter(Negate(is.null), slot_list)
    n <- length(filled)
    if (n == 0) {
      return(list(.error =
        "No hay escenarios guardados. Defina al menos uno en la pantalla 'Revisar'."))
    }
    withProgress(message = "Microsimulando\u2026", value = 0, {
      all_esc   <- list()
      labels    <- list()
      insumos1  <- NULL

      for (i in seq_along(filled)) {
        incProgress(1 / n,
          message = paste0("Escenario ", i, " de ", n, "\u2026"))
        inp <- filled[[i]]
        inp$escenario <- i
        res <- tryCatch(
          run_dom_scenario(inp, paths = paths, return = "both"),
          error = function(e) list(.error = conditionMessage(e))
        )
        if (!is.null(res$.error)) return(res)
        if (i == 1L) insumos1 <- res$insumos_live
        esc_key <- paste0("escenario_", i)
        # Elegir clave del resultado para este índice (la tubería puede forzar escenario_1)
        res_keys <- names(res$resultados_escenarios)
        non_zero <- res_keys[res_keys != "escenario_0"]
        taken    <- if (length(non_zero) == 1L) non_zero[1] else esc_key
        all_esc[[esc_key]] <- res$resultados_escenarios[[taken]]
        labels[[esc_key]]  <- as.character(inp$label %||% paste0("Escenario ", i))
      }

      list(
        resultados_escenarios = all_esc,
        insumos_live          = insumos1,
        scenario_labels       = labels,
        paths                 = paths
      )
    })
  }

  sim_res_rv <- reactiveVal(NULL)

  observeEvent(input$run, {
    sel    <- cmp_rv$sel
    marked <- Filter(function(s) s$uid %in% sel, scen_rv$scenarios)
    if (length(marked)) {
      if (length(marked) > 4L) marked <- marked[1:4]
      slots <- lapply(marked, compose_scenario_inputs)
    } else if (length(scen_rv$scenarios)) {
      # Hay escenarios pero ninguno marcado: comparar los primeros (hasta 4).
      slots <- lapply(utils::head(scen_rv$scenarios, 4L), compose_scenario_inputs)
    } else {
      # Sin escenarios guardados: usar el estado actual del formulario.
      slots <- list(collect_scenario_inputs())
    }
    sim_res_rv(run_multi_dom(slots, paths()))
  })

  observeEvent(input$run_test, {
    inp <- read_analyst_fixture_scenario_inputs(root)
    sim_res_rv(run_multi_dom(list(inp), paths()))
  })

  sim_res <- reactive(sim_res_rv())

  output$btn_export_rds <- downloadHandler(
    filename = function() {
      paste0("simfi_resultados_", format(Sys.Date(), "%Y-%m-%d"), ".rds")
    },
    content = function(file) {
      r <- sim_res()
      if (is.null(r) || !is.null(r$.error)) {
        saveRDS(list(.error = "No hay resultados disponibles."), file)
        return()
      }
      saveRDS(
        list(
          resultados_escenarios = r$resultados_escenarios,
          dom_results           = r$dom_results,
          scenario_labels       = r$scenario_labels,
          exported_at           = Sys.time()
        ),
        file
      )
    }
  )

  output$run_msg <- renderUI({
    r <- sim_res()
    if (is.null(r))           return(NULL)
    if (!is.null(r$.error))
      return(tags$div(class = "alert alert-danger",
                      strip_cli_markup(r$.error)))
    n <- length(r$resultados_escenarios)
    tags$div(class = "alert alert-success",
             icon("check-circle"),
             paste0(" Simulaci\u00f3n lista: ", n, " escenario(s)."))
  })

  # Reactivos derivados de la simulación
  dom_list_ready <- reactive({
    r <- sim_res()
    !is.null(r) && is.null(r$.error) && is.list(r$resultados_escenarios) &&
      length(r$resultados_escenarios) > 0
  })

  scenario_headers_named <- reactive({
    req(dom_list_ready())
    dl     <- sim_res()$resultados_escenarios
    labels <- sim_res()$scenario_labels %||% list()
    stats::setNames(
      vapply(names(dl), function(k) {
        i   <- scenario_index_from_key(k)
        lbl <- labels[[k]] %||% paste0("Escenario ", i)
        sprintf("%d. %s", i, lbl)
      }, character(1)),
      names(dl)
    )
  })

  summ <- reactive({
    req(dom_list_ready())
    dl <- sim_res()$resultados_escenarios
    procesar_fila_dom(
      dl, 1L, 3L,
      post_display = unname(scenario_headers_named()[names(dl)])
    )
  })

  summ_col_tooltips <- reactive({
    req(summ())
    st <- unname(scenario_headers_named()[names(sim_res()$resultados_escenarios)])
    c("Pre-reforma" = "L\u00ednea base antes de aplicar la reforma simulada.",
      stats::setNames(rep("", length(st)), st))
  })

  inc_col_tooltips <- reactive({
    st <- unname(scenario_headers_named()[names(sim_res()$resultados_escenarios)])
    stats::setNames(rep("", length(st)), st)
  })

  kw_summ <- reactive({
    req(dom_list_ready())
    dl <- sim_res()$resultados_escenarios
    if (!dom_list_is_pipeline_layout(dl))
      return(list(kak_ic = NULL, kak_kw = NULL))
    kakwani_summary_matrices(dl, unname(scenario_headers_named()[names(dl)]))
  })

  fiscal_mat <- reactive({
    req(dom_list_ready())
    dl <- sim_res()$resultados_escenarios
    if (!dom_list_is_pipeline_layout(dl)) return(NULL)
    fiscal_post_measures_mat(dl, scenario_headers_named())
  })

  resultados_inc <- reactive({
    req(dom_list_ready())
    dl <- sim_res()$resultados_escenarios
    if (!dom_list_is_pipeline_layout(dl)) return(NULL)
    cn <- unname(scenario_headers_named()[names(dl)])
    list(
      decinc  = incidencia_columna(dl, "decinc",  scenario_colnames = cn),
      estrinc = incidencia_columna(dl, "estrinc", scenario_colnames = cn),
      urbinc  = incidencia_columna(dl, "urbinc",  scenario_colnames = cn),
      sexinc  = incidencia_columna(dl, "sexinc",  scenario_colnames = cn),
      catinc  = incidencia_columna(dl, "catinc",  scenario_colnames = cn),
      depinc  = incidencia_columna(dl, "depinc",  scenario_colnames = cn)
    )
  })

  # Per-instrument incidence across ALL scenarios (multi-scenario aware).
  # Each entry is a named list of tables (decinc, estrinc, …) with one column
  # per scenario labelled with the human-readable scenario header.
  resultados_inst_inc <- reactive({
    req(dom_list_ready())
    dl <- sim_res()$resultados_escenarios
    if (!dom_list_is_pipeline_layout(dl)) return(NULL)
    cn <- unname(scenario_headers_named()[names(dl)])
    tablas <- c("decinc", "estrinc", "urbinc", "sexinc", "catinc", "depinc")

    build_block <- function(col_fn) {
      stats::setNames(
        lapply(tablas, function(tbl) {
          incidencia_columna(dl, tbl, scenario_colnames = cn, col_fn = col_fn)
        }),
        tablas
      )
    }

    list(
      efecto_neto  = build_block(function(k) paste0("nitx",     k, "_pc")),
      isr          = build_block(function(k) paste0("ddtx_isr", k, "_pc")),
      itbis        = build_block(function(k) paste0("ditx_itb", k, "_pc")),
      subsidios    = build_block(function(k) paste0("dsub_ele", k, "_pc")),
      compensacion = build_block(function(k) paste0("dcomp",    k, "_pc"))
    )
  })

  insumos_template <- reactive({
    load_insumos_catalog(paths())$template
  })

  ins_live_display <- reactive({
    req(dom_list_ready())
    enrich_insumos_live(sim_res()$insumos_live, insumos_template())
  })

  labels_inc <- labels_inc_dom()

  observeEvent(sim_res(), {
    r <- sim_res()
    if (is.null(r) || !is.null(r$.error) || !length(r$resultados_escenarios))
      return(invisible(NULL))
    dl  <- r$resultados_escenarios
    hdr <- scenario_headers_named()
    ch  <- stats::setNames(names(dl), unname(hdr[names(dl)]))
    updateSelectInput(session, "dash_macro_esc",
                      choices = ch, selected = names(dl)[1])
  }, ignoreNULL = TRUE)

  # Utilidades para tablas DT del tablero
  dash_fmt_mat_dt <- function(m, round_digits = 2) {
    tg <- as.data.frame(m, optional = TRUE) %>%
      tibble::rownames_to_column("Indicador")
    datatable(tg, rownames = FALSE,
              options = list(scrollX = TRUE, dom = "ftip", pageLength = 12),
              class = "compact stripe hover") %>%
      formatRound(columns = seq(2, ncol(tg)), digits = round_digits)
  }

  need_run <- "Ejecute la simulaci\u00f3n para ver los cuadros."

  output$dash_tbl_povr <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    dash_fmt_mat_dt(summ()$povr, 2)
  })
  output$dash_tbl_npov <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    dash_fmt_mat_dt(summ()$npov, 0)
  })
  output$dash_tbl_povb <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    dash_fmt_mat_dt(summ()$povb, 0)
  })
  output$dash_tbl_desr <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    dash_fmt_mat_dt(summ()$desr, 4)
  })
  output$dash_tbl_kak_ic <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    k <- kw_summ()$kak_ic
    shiny::validate(shiny::need(!is.null(k), "IC no disponible."))
    dash_fmt_mat_dt(k, 4)
  })
  output$dash_tbl_kak_kw <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    k <- kw_summ()$kak_kw
    shiny::validate(shiny::need(!is.null(k), "Kakwani no disponible."))
    dash_fmt_mat_dt(k, 4)
  })
  output$dash_tbl_gini <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    tg <- as.data.frame(summ()$desr, optional = TRUE) %>%
      tibble::rownames_to_column("Indicador")
    datatable(tg, rownames = FALSE,
              options = list(dom = "ftip", scrollX = TRUE)) %>%
      formatRound(columns = seq(2, ncol(tg)), digits = 4)
  })
  output$dash_tbl_fiscal <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    fm <- fiscal_mat()
    shiny::validate(shiny::need(!is.null(fm),
                                "Cuadro fiscal no disponible."))
    dash_fmt_mat_dt(fm, 3)
  })

  # Gráficos Plotly de resumen
  output$dash_plot_npov_bar <- renderPlotly({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    plotly_npov_horizontal(summ()$npov, column_tooltips = summ_col_tooltips())
  })

  mk_comb01 <- function(mat_nm, dec, y_label, idx) {
    renderPlotly({
      shiny::validate(shiny::need(dom_list_ready(), need_run))
      comb01_plotly_native(summ()[[mat_nm]], dec, "", y_label, idx,
                           base_font_size = 11,
                           column_tooltips = summ_col_tooltips())
    })
  }
  output$dash_plot_povr_1 <- mk_comb01("povr", 1, "Porcentaje", 1)
  output$dash_plot_povb_1 <- mk_comb01("povb", 0,
    "Pesos mensuales (promedio hogares pobres)", 1)
  output$dash_plot_povr_2 <- mk_comb01("povr", 1, "Porcentaje", 2)
  output$dash_plot_povb_2 <- mk_comb01("povb", 0,
    "Pesos mensuales (promedio hogares pobres)", 2)
  output$dash_plot_povr_3 <- mk_comb01("povr", 1, "Porcentaje", 3)
  output$dash_plot_povb_3 <- mk_comb01("povb", 0,
    "Pesos mensuales (promedio hogares pobres)", 3)

  mk_desr_plot <- function(idx, y_label, title_txt) {
    renderPlotly({
      shiny::validate(shiny::need(dom_list_ready(), need_run))
      if (nrow(summ()$desr) < 4L)
        return(plotly::plot_ly() %>%
                 plotly::layout(title = list(text = paste(title_txt, "no disponible."),
                                             x = 0.5)))
      comb01_plotly_native(summ()$desr, 4, "", y_label, idx,
                           column_tooltips = summ_col_tooltips())
    })
  }
  output$dash_plot_gini  <- mk_comb01("desr", 4, "\u00cdndice de Gini", 1)
  output$dash_plot_dgini <- mk_desr_plot(2, "\u0394 Gini", "\u0394 Gini")
  output$dash_plot_palma <- mk_desr_plot(3, "\u00cdndice de Palma", "Palma")
  output$dash_plot_theil <- mk_desr_plot(4, "\u00cdndice de Theil", "Theil")

  output$dash_plot_kak_ic <- renderPlotly({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    k <- kw_summ()$kak_ic
    shiny::validate(shiny::need(!is.null(k), "IC no disponible."))
    graficar_incidencia_plotly(k, rownames(k), colnames(k),
      plot_main_title = "\u00cdndice de concentraci\u00f3n por instrumento",
      y_axis_label_str = "IC", hover_decimal_places = 4,
      font_size_base = 11, scenario_tooltips = summ_col_tooltips(),
      y_as_percent = FALSE)
  })
  output$dash_plot_kak_kw <- renderPlotly({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    k <- kw_summ()$kak_kw
    shiny::validate(shiny::need(!is.null(k), "Kakwani no disponible."))
    graficar_incidencia_plotly(k, rownames(k), colnames(k),
      plot_main_title = "\u00cdndice de Kakwani por instrumento",
      y_axis_label_str = "Kakwani", hover_decimal_places = 4,
      font_size_base = 11, scenario_tooltips = summ_col_tooltips(),
      y_as_percent = FALSE)
  })
  output$dash_plot_comp <- renderPlotly({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    dl <- sim_res()$resultados_escenarios
    plotly_compensacion_bars(dl, unname(scenario_headers_named()[names(dl)]),
                             scenario_tooltips = inc_col_tooltips())
  })

  # Gráficos Plotly de incidencia
  mk_dash_inc <- function(key, title) {
    renderPlotly({
      shiny::validate(shiny::need(dom_list_ready(), need_run))
      ri <- resultados_inc()
      shiny::validate(shiny::need(!is.null(ri), "Incidencia no disponible."))
      M <- ri[[key]]
      shiny::validate(shiny::need(!is.null(M),
                                  "Sin datos de incidencia para este corte."))
      graficar_incidencia_plotly(
        M, labels_inc[[key]], colnames(M),
        plot_main_title       = title,
        y_axis_label_str      = "Porcentaje del ingreso disponible",
        hover_decimal_places  = 2,
        font_size_base        = 11,
        scenario_tooltips     = inc_col_tooltips(),
        y_as_percent          = TRUE
      )
    })
  }
  output$dash_inc_dec  <- mk_dash_inc("decinc",  "Incidencia neta: deciles")
  output$dash_inc_estr <- mk_dash_inc("estrinc", "Incidencia neta: estrato")
  output$dash_inc_urb  <- mk_dash_inc("urbinc",  "Incidencia neta: \u00e1rea")
  output$dash_inc_sex  <- mk_dash_inc("sexinc",  "Incidencia neta: sexo del jefe")
  output$dash_inc_cat  <- mk_dash_inc("catinc",  "Incidencia neta: tipo de hogar")

  # Rename raw pipeline column names in depinc to Spanish labels.
  rename_depinc_cols <- function(df, scen_headers) {
    era <- function(k_str) {
      k <- suppressWarnings(as.integer(k_str))
      if (!is.na(k) && k == 0L) return("Pre-reforma")
      lbl <- scen_headers[paste0("escenario_", k)]
      if (length(lbl) && !is.na(lbl) && nzchar(lbl)) return(unname(lbl))
      paste0("Escenario ", k)
    }
    rename_one <- function(nm) {
      n <- tolower(nm)
      if (grepl("^ddtx_isr(\\d+)_pc$",  n)) return(paste0("\u0394 ISR (",                era(sub("^ddtx_isr(\\d+)_pc$",  "\\1", n)), ")"))
      if (grepl("^dtx_isr(\\d+)_pc$",   n)) return(paste0("ISR (",                       era(sub("^dtx_isr(\\d+)_pc$",   "\\1", n)), ")"))
      if (grepl("^ditx_itb(\\d+)_pc$",  n)) return(paste0("\u0394 ITBIS (",              era(sub("^ditx_itb(\\d+)_pc$",  "\\1", n)), ")"))
      if (grepl("^itx_itb(\\d+)_pc$",   n)) return(paste0("ITBIS (",                     era(sub("^itx_itb(\\d+)_pc$",   "\\1", n)), ")"))
      if (grepl("^dsub_ele(\\d+)_pc$",  n)) return(paste0("\u0394 Subsidio (",           era(sub("^dsub_ele(\\d+)_pc$",  "\\1", n)), ")"))
      if (grepl("^sub_ele(\\d+)_pc$",   n)) return(paste0("Subsidio el\u00e9ctrico (", era(sub("^sub_ele(\\d+)_pc$",   "\\1", n)), ")"))
      if (grepl("^dcomp(\\d+)_pc$",     n)) return(paste0("\u0394 Compensaci\u00f3n (", era(sub("^dcomp(\\d+)_pc$",     "\\1", n)), ")"))
      if (grepl("^comp_(\\d+)_pc$",     n)) return(paste0("Compensaci\u00f3n (",        era(sub("^comp_(\\d+)_pc$",     "\\1", n)), ")"))
      if (grepl("^(neto|nitx)(\\d+)_pc$", n)) return(paste0("Efecto neto (",            era(sub("^(?:neto|nitx)(\\d+)_pc$", "\\1", n, perl = TRUE)), ")"))
      nm
    }
    colnames(df) <- vapply(colnames(df), rename_one, character(1))
    df
  }

  output$dash_tbl_reg <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    req(input$dash_macro_esc)
    dl  <- sim_res()$resultados_escenarios
    one <- dl[input$dash_macro_esc]
    shiny::validate(shiny::need(length(one) == 1, "Escenario no v\u00e1lido."))
    tab <- tabla_macro_region(one,
                              escenario_headers = scenario_headers_named())
    tab <- rename_depinc_cols(tab, scenario_headers_named())
    datatable(tab, rownames = FALSE,
              options = list(scrollX = TRUE, dom = "ftip", pageLength = 12))
  })

  mk_inst_plot <- function(inst_key, title) {
    renderPlotly({
      shiny::validate(shiny::need(dom_list_ready(), need_run))
      ri <- resultados_inst_inc()
      shiny::validate(shiny::need(!is.null(ri), "Sin datos para este instrumento."))
      M <- ri[[inst_key]]$decinc
      shiny::validate(shiny::need(!is.null(M), "Sin datos para este instrumento."))
      graficar_incidencia_plotly(
        M, labels_inc$decinc, colnames(M),
        plot_main_title      = title,
        y_axis_label_str     = "Porcentaje del ingreso disponible",
        hover_decimal_places = 2,
        font_size_base       = 11,
        scenario_tooltips    = inc_col_tooltips(),
        y_as_percent         = TRUE
      )
    })
  }
  output$dash_inst_net_dec  <- mk_inst_plot("efecto_neto", "Efecto neto: deciles")
  output$dash_inst_isr_dec  <- mk_inst_plot("isr",         "ISR: deciles")
  output$dash_inst_itb_dec  <- mk_inst_plot("itbis",       "ITBIS: deciles")
  output$dash_inst_sub_dec  <- mk_inst_plot("subsidios",   "Subsidio el\u00e9ctrico: deciles")
  output$dash_inst_comp_dec <- mk_inst_plot("compensacion","Compensaci\u00f3n: deciles")

  # Tablas del glosario
  read_glosario_csv <- function(nm) {
    p <- file.path(root, "data", nm)
    if (!file.exists(p)) return(data.frame())
    utils::read.csv(p, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  }
  glosario_ind_tbl <- read_glosario_csv("glosario_indicadores.csv")
  glosario_grp_tbl <- read_glosario_csv("glosario_grupos.csv")
  glosario_var_tbl <- read_glosario_csv("glosario_variables.csv")

  output$dash_tbl_glos_esc <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(),
      "Ejecute la simulaci\u00f3n para ver el escenario activo."))
    dl   <- sim_res()$resultados_escenarios
    lbs  <- sim_res()$scenario_labels %||% list()
    scns <- scen_rv$scenarios

    build_detail <- function(label) {
      sc <- Filter(function(s) identical(s$name, label), scns)
      if (!length(sc)) return("Par\u00e1metros de referencia")
      sc <- sc[[1]]
      parts <- c(
        if (nzchar(sc$itbis %||% "")) paste("ITBIS:", sc$itbis),
        if (nzchar(sc$isr   %||% "")) paste("Renta:", sc$isr),
        if (nzchar(sc$sub   %||% "")) paste("Subsidio:", sc$sub),
        if (nzchar(sc$comp  %||% "")) paste("Compensaci\u00f3n:", sc$comp)
      )
      if (!length(parts)) "Par\u00e1metros de referencia" else paste(parts, collapse = " | ")
    }

    tab <- tibble::tibble(
      Espacio  = seq_along(dl),
      Etiqueta = vapply(names(dl), function(k) lbs[[k]] %||% k, character(1)),
      Detalle  = vapply(names(dl), function(k) build_detail(lbs[[k]] %||% k), character(1))
    )
    datatable(tab, options = list(dom = "t", pageLength = 5),
              rownames = FALSE)
  })
  output$dash_tbl_glos_ind <- renderDT({
    datatable(glosario_ind_tbl,
              options = list(scrollX = TRUE, pageLength = 15),
              rownames = FALSE)
  })
  output$dash_tbl_glos_grp <- renderDT({
    datatable(glosario_grp_tbl,
              options = list(scrollX = TRUE, pageLength = 12),
              rownames = FALSE)
  })
  output$dash_tbl_glos_var <- renderDT({
    datatable(glosario_var_tbl,
              options = list(scrollX = TRUE, pageLength = 15),
              rownames = FALSE)
  })
}

shinyApp(ui, server)
