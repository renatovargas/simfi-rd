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
  for (cx in c("costsur","costnorte","costeste")) {
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
                     "Rep\u00fablica Dominicana: Microsimulaci\u00f3n fiscal"),
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
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Tasas por producto"),
          textInput("scen_nombre", "Nombre del escenario",
                    value = "Reforma ilustrativa", width = "100%"),
          hr(),
          checkboxInput(
            "itbis_solo_exentos",
            tags$span(
              "Mostrar solo productos ",
              tags$strong("exentos"),
              " (campo \u2018grupo = exentos\u2019)"
            ),
            value = FALSE
          ),
          h6("Clasificaci\u00f3n"),
          selectInput("itbis_grupo", "Grupo",
                      choices  = grupo_choice_vals_full,
                      selected = itbis_grupo_full$COD_GRUPO[1],
                      width    = "100%"),
          selectInput("itbis_subclase", "Subclase",
                      choices = NULL, width = "100%"),
          selectizeInput("itbis_variedad", "Variedad",
                         choices = NULL, width = "100%"),
          radioButtons(
            "itbis_nivel_aplicar", "Aplicar tasa a",
            choiceNames  = list("Todo el grupo",
                                "La subclase",
                                "Solo esta variedad"),
            choiceValues = c("grupo", "subclase", "variedad"),
            selected     = "subclase"
          ),
          numericInput("tasa_aplicar", "Tasa (%)",
                       value = 18, min = 0, max = 100, step = 0.25,
                       width = "100%"),
          layout_columns(
            col_widths = c(6, 6),
            actionButton("btn_aplicar_tasa", "Aplicar tasa",
                         class = "btn-outline-primary w-100"),
            actionButton("btn_reset_rama", "Restablecer rama",
                         class = "btn-outline-warning w-100")
          ),
          actionButton("btn_limpiar_itbis", "Limpiar todo el ITBIS",
                       class = "btn-outline-danger w-100 mt-2")
        ),
        card(
          card_header("Vista previa de cambios"),
          p(class = "small text-muted",
            "Productos con tasa diferente a la de referencia."),
          DTOutput("tbl_itbis_resumen", height = "420px")
        )
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
        conditionalPanel(
          condition = "input.custom_isr == true",
          p(class = "text-muted small",
            "Por tramo indique l\u00edmite inferior, l\u00edmite superior y tasa (%). ",
            "El primer l\u00edmite inferior debe ser 0. ",
            "Deje el \u00faltimo superior vac\u00edo si el tramo no tiene tope."),
          textInput("nom_renta", "Nombre de la escala",
                    value = "Escala personalizada", width = "100%"),
          tags$table(
            class = "isr-tbl",
            tags$thead(tags$tr(
              tags$th(style = "width:50px;", "#"),
              tags$th("L\u00edmite inferior (RD$)"),
              tags$th("L\u00edmite superior (RD$)"),
              tags$th(style = "width:110px;", "Tasa (%)")
            )),
            tags$tbody(lapply(1:6, function(i) {
              def <- list(list(0, 301444, 0), list(301444, 416220, 4),
                          list(416220, 624329, 15), list(624329, 867123, 20),
                          list(867123, NA, 25), list(NA, NA, NA))[[i]]
              tags$tr(
                tags$td(i, style = "text-align:center;font-weight:bold;"),
                tags$td(numericInput(paste0("isr_li_", i), NULL,
                                    value = def[[1]], min = 0, step = 1000)),
                tags$td(numericInput(paste0("isr_ls_", i), NULL,
                                    value = def[[2]], min = 0, step = 1000)),
                tags$td(numericInput(paste0("isr_tp_", i), NULL,
                                    value = def[[3]], min = 0, max = 100,
                                    step = 0.5))
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
            "La referencia es la pol\u00edtica vigente (sim\u00fatil 1). ",
            "Active la personalizaci\u00f3n para ajustar bloques y tarifas."),
          checkboxInput("custom_sub_ele",
                      "Personalizar bloques y tarifas por distribuidora",
                      value = FALSE),
        conditionalPanel(
          condition = "input.custom_sub_ele == true",
          p(class = "text-muted small",
            "Los valores iniciales corresponden a la pol\u00edtica seleccionada. ",
            "Cambiar la pol\u00edtica actualiza la plantilla mientras esta opci\u00f3n est\u00e1 activa."),
          tags$table(
            class = "sub-ele-tbl",
            tags$thead(tags$tr(
              tags$th(style = "width:40px;", "#"),
              tags$th("Tope kWh"),
              tags$th("Edesur: cargo fijo"),
              tags$th("Edesur: RD$/kWh"),
              tags$th("Edenorte: cargo fijo"),
              tags$th("Edenorte: RD$/kWh"),
              tags$th("Edeeste: cargo fijo"),
              tags$th("Edeeste: RD$/kWh")
            )),
            tags$tbody(lapply(1:7, function(j) {
              tags$tr(
                tags$td(j, style = "text-align:center;font-weight:600;"),
                tags$td(numericInput(paste0("sub_block_",  j), NULL,
                                    value = NA_real_, min = 0, step = 1)),
                tags$td(numericInput(paste0("sub_bsur_",   j), NULL,
                                    value = NA_real_, min = 0, step = 0.01)),
                tags$td(numericInput(paste0("sub_tsur_",   j), NULL,
                                    value = NA_real_, min = 0, step = 0.01)),
                tags$td(numericInput(paste0("sub_bnorte_", j), NULL,
                                    value = NA_real_, min = 0, step = 0.01)),
                tags$td(numericInput(paste0("sub_tnorte_", j), NULL,
                                    value = NA_real_, min = 0, step = 0.01)),
                tags$td(numericInput(paste0("sub_beste_",  j), NULL,
                                    value = NA_real_, min = 0, step = 0.01)),
                tags$td(numericInput(paste0("sub_teste_",  j), NULL,
                                    value = NA_real_, min = 0, step = 0.01))
              )
            }))
          ),
          textInput("sub_nom", "Nombre descriptivo de la pol\u00edtica",
                    value = "", width = "100%"),
          fluidRow(
            column(4, numericInput("sub_costsur",
                                   "Costo ref. Sur (RD$/kWh)",
                                   value = NA_real_, min = 0, step = 0.0001)),
            column(4, numericInput("sub_costnorte",
                                   "Costo ref. Norte (RD$/kWh)",
                                   value = NA_real_, min = 0, step = 0.0001)),
            column(4, numericInput("sub_costeste",
                                   "Costo ref. Este (RD$/kWh)",
                                   value = NA_real_, min = 0, step = 0.0001))
          )
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
          conditionalPanel(
            condition = "input.comp_con_comp == true",
          hr(),
          fluidRow(
            column(4, selectInput("comp_grupo", "Grupo beneficiario",
                                  choices = c(
                                    "Beneficiarios Sup\u00e9rate" = "1",
                                    "Decil de ingreso"            = "2",
                                    "ICV"                          = "3"
                                  ), selected = "1")),
            column(4, selectInput("comp_metodo", "M\u00e9todo",
                                  choices = c(
                                    "P\u00e9rdida neta (promedio en ventana de deciles)" = "1",
                                    "Valor fijo mensual \u00d7 12"                        = "2"
                                  ), selected = "1")),
            column(4, numericInput("comp_valor",
                                   "Valor fijo (RD$/mes, m\u00e9todo 2)",
                                   value = 0, min = 0, step = 10))
          ),
          fluidRow(
            column(4, selectInput("comp_decil_est",
                                  "Hasta qu\u00e9 decil entra el promedio (m\u00e9todo 1)",
                                  choices = c(
                                    "Sin seleccionar" = "",
                                    stats::setNames(as.character(1:10),
                                                    paste("Decil", 1:10))
                                  ), selected = "4")),
            column(4, selectInput("comp_decil_com",
                                  "Tope decil de ingreso (grupo 2)",
                                  choices = c(
                                    "Sin seleccionar" = "",
                                    stats::setNames(as.character(1:10),
                                                    paste("Decil", 1:10))
                                  ), selected = "2")),
            column(4, numericInput("comp_icv",
                                   "Umbral ICV (grupo 3)",
                                   value = NA, min = 0, step = 0.1))
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
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Guardar escenario"),
          p(class = "text-muted small",
            "Asigne un nombre y gu\u00e1rdelo en uno de los tres espacios. ",
            "Se pueden comparar hasta tres escenarios en la pantalla de simulaci\u00f3n."),
          textInput("scen_nombre_rev",
                    "Nombre del escenario (confirmar)",
                    value = "", width = "100%"),
          selectInput("slot_selector", "Guardar en espacio",
                      choices = stats::setNames(
                        1:3, paste("Espacio", 1:3)
                      ), selected = 1, width = "100%"),
          actionButton("btn_save_slot",
                       icon("floppy-disk"),
                       label = " Guardar en espacio seleccionado",
                       class = "btn-primary w-100"),
          hr(),
          downloadButton("dl_json", "Exportar configuraci\u00f3n (.json)",
                         class = "w-100 btn-outline-secondary mt-1"),
          fileInput("up_json", "Importar configuraci\u00f3n (.json)",
                    accept = c(".json","application/json"), width = "100%")
        ),
        card(
          card_header("Espacios activos"),
          uiOutput("slots_display")
        )
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
        col_widths = c(3, 9),
        gap = "1rem",
        card(
          card_header("Ejecuci\u00f3n"),
          actionButton("run",
                       icon("play"),
                       label = " Ejecutar microsimulaci\u00f3n",
                       class = "btn-primary w-100"),
          p(class = "small text-muted mt-1 mb-2",
            "Corre todos los escenarios guardados en los espacios."),
          actionButton("run_test",
                       icon("flask"),
                       label = " Ejecutar prueba",
                       class = "btn-outline-primary w-100 mt-1"),
          p(class = "small text-muted mt-1 mb-0",
            "Escenario de referencia del repositorio."),
          hr(),
          uiOutput("run_msg"),
          hr(),
          tags$div(class = "small text-muted mt-2",
            tags$strong("Espacios en cola:"),
            uiOutput("run_slots_summary")
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
              nav_panel("Recaudaci\u00f3n",
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
                        plotlyOutput("dash_inc_estr", height = "400px")),
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
          )
        )
      ),
      nav_row(prev_id = "nav_6_prev", prev_label = "\u2190 Volver a Revisar")
    )
  ),

  nav_panel(
    "Acerca de",
    p("Este panel construye escenarios fiscales paso a paso y ejecuta la ",
      "microsimulaci\u00f3n en este equipo. Pueden definirse hasta tres escenarios ",
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

  # Hasta tres espacios: NULL o lista devuelta por collect_scenario_inputs()
  scenarios_rv <- reactiveValues(slot1 = NULL, slot2 = NULL, slot3 = NULL)

  paths <- reactive(get_dom_paths(root))

  # Sincronización bidireccional del nombre entre pantalla 1 y 5
  observe({
    req(input$scen_nombre)
    updateTextInput(session, "scen_nombre_rev", value = input$scen_nombre)
  })
  observeEvent(input$scen_nombre_rev, {
    updateTextInput(session, "scen_nombre", value = input$scen_nombre_rev)
  }, ignoreInit = TRUE)

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

  # Sin desplegables pick_sim_sub y pick_sim_com: políticas desde fila referencia
  # (sim_sub = 1, sim_com = 1) o definidas por el usuario con las casillas de personalización.

  # Filtro ITBIS por productos exentos
  # Catálogo reactivo según la casilla de solo exentos
  itbis_catalog_display <- reactive({
    if (isTRUE(input$itbis_solo_exentos)) {
      itbis_catalog %>% filter(tolower(.data$grupo) == "exentos")
    } else {
      itbis_catalog
    }
  })

  # Al cambiar el filtro de exentos, reconstruir las opciones de grupo
  observe({
    cat <- itbis_catalog_display()
    gdf <- cat %>%
      distinct(.data$COD_GRUPO, .data$DES_GRUPO) %>%
      arrange(.data$COD_GRUPO)
    ch  <- make_grupo_choices(gdf)
    sel <- if (length(ch)) ch[1] else character(0)
    updateSelectInput(session, "itbis_grupo", choices = ch, selected = sel)
  })

  # Selects ITBIS en cascada
  observe({
    req(input$itbis_grupo)
    cat <- itbis_catalog_display()
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

  # Aplicar o limpiar tasas ITBIS
  observeEvent(input$btn_aplicar_tasa, {
    req(input$itbis_nivel_aplicar, input$itbis_grupo)
    lvl  <- input$itbis_nivel_aplicar
    rate <- input$tasa_aplicar
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
    showNotification("Se limpiaron las tasas de ITBIS.", type = "message")
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
    showNotification("Rama restablecida.", type = "message")
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
      `Tasa (%)` = round(tasa_eff[muestra], 2),
      `Diferencia vs referencia` = round(delta[muestra], 2)
    )
  })

  output$tbl_itbis_resumen <- renderDT({
    datatable(tbl_itbis_preview(), rownames = FALSE,
              options = list(dom = "ftip", scrollX = TRUE, pageLength = 15),
              class = "compact stripe hover")
  })

  # Subsidio eléctrico: rellenar desde fila plantilla (sim_sub = 1)
  observeEvent(input$custom_sub_ele, {
    if (!isTRUE(input$custom_sub_ele)) return(invisible(NULL))
    p <- paths()
    r <- read_sim_sub_template_row(p$param_csv, 1L)
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
        costeste    = suppressWarnings(as.numeric(input$sub_costeste))
      )
    }

    nm <- input$scen_nombre %||% "Escenario"
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

  # Guardar en un espacio
  observeEvent(input$btn_save_slot, {
    slot_key <- paste0("slot", as.integer(input$slot_selector))
    sc       <- collect_scenario_inputs()
    nm <- trimws(input$scen_nombre_rev %||% "")
    if (nzchar(nm)) {
      sc$label         <- nm
      sc$des_corto     <- nm
      sc$des_escenario <- nm
    }
    scenarios_rv[[slot_key]] <- sc

    # Nombre sugerido para el siguiente escenario
    filled_count <- sum(!vapply(1:3, function(i)
      is.null(scenarios_rv[[paste0("slot", i)]]), logical(1)))
    next_nm <- paste0("Escenario ", filled_count + 1L)

    # Reiniciar el formulario para armar otro escenario desde cero
    tax_rv$rate_grupo    <- list()
    tax_rv$rate_subclase <- list()
    tax_rv$rate_variedad <- list()
    updateTextInput(session,    "scen_nombre",     value = next_nm)
    updateTextInput(session,    "scen_nombre_rev", value = next_nm)
    updateCheckboxInput(session, "custom_isr",      value = FALSE)
    updateCheckboxInput(session, "custom_sub_ele",  value = FALSE)
    updateCheckboxInput(session, "comp_con_comp",   value = FALSE)

    showNotification(
      paste0("Guardado en espacio ", input$slot_selector,
             ". Formulario listo para un nuevo escenario."),
      type = "message"
    )
    # Volver a la pantalla 1 para el siguiente escenario
    go_to("1 \u00b7 ITBIS")
  })

  # Tarjetas de espacios en pantalla Revisar
  output$slots_display <- renderUI({
    slots <- list(
      `1` = scenarios_rv$slot1,
      `2` = scenarios_rv$slot2,
      `3` = scenarios_rv$slot3
    )
    tagList(
      lapply(1:3, function(i) {
        sc   <- slots[[as.character(i)]]
        filled <- !is.null(sc)
        card(
          class = paste("mb-2 slot-card", if (filled) "filled" else ""),
          card_body(
            class = "py-2",
            tags$div(
              class = "d-flex justify-content-between align-items-start",
              tags$div(
                tags$span(
                  class = paste("badge slot-badge",
                                if (filled) "bg-primary" else "bg-secondary"),
                  paste("Espacio", i)
                ),
                tags$div(
                  class = "mt-1",
                  if (filled) {
                    tagList(
                      tags$strong(sc$label %||% paste("Escenario", i)),
                      br(),
                      tags$span(
                        class = "text-muted small",
                        paste0(
                          "ITBIS: ",
                          if (length(sc$itbis$rate_variedad) +
                              length(sc$itbis$rate_subclase) +
                              length(sc$itbis$rate_grupo) > 0)
                            paste0(
                              length(sc$itbis$rate_variedad) +
                              length(sc$itbis$rate_subclase) +
                              length(sc$itbis$rate_grupo),
                              " cambio(s)"
                            ) else "sin cambios",
                          " | ISR: ",
                          if (isTRUE(sc$custom_isr)) "personalizado"
                          else "referencia",
                          " | Sub: opci\u00f3n ", sc$sim_sub %||% "?",
                          " | Comp: ",
                          if (isTRUE(sc$comp$sin_compensacion)) "ninguna"
                          else paste0("opci\u00f3n ", sc$sim_com %||% "?")
                        )
                      )
                    )
                  } else {
                    tags$span(class = "text-muted small fst-italic",
                              "Vac\u00eda")
                  }
                )
              ),
              if (filled)
                actionButton(
                  paste0("btn_clear_slot_", i),
                  icon("trash"),
                  class = "btn-sm btn-outline-danger",
                  title = "Limpiar espacio"
                )
            )
          )
        )
      })
    )
  })

  # Botones Limpiar por espacio
  lapply(1:3, function(i) {
    observeEvent(input[[paste0("btn_clear_slot_", i)]], {
      scenarios_rv[[paste0("slot", i)]] <- NULL
      showNotification(paste0("Espacio ", i, " limpiado."), type = "warning")
    })
  })

  # Resumen de espacios en pantalla Simular
  output$run_slots_summary <- renderUI({
    cnt <- sum(!vapply(1:3, function(i) {
      is.null(scenarios_rv[[paste0("slot", i)]])
    }, logical(1)))
    if (cnt == 0) {
      return(tags$span(class = "text-warning",
                       "Ningún espacio guardado. Se ejecutar\u00e1 el escenario actual."))
    }
    tagList(lapply(1:3, function(i) {
      sc <- scenarios_rv[[paste0("slot", i)]]
      if (is.null(sc)) return(NULL)
      tags$div(class = "scenario-chip",
               tags$strong(paste0("Espacio ", i, ": ")),
               sc$label %||% paste0("Escenario ", i))
    }))
  })

  # Normalización de listas de tasas al importar JSON
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

  json_scalar_num <- function(x) {
    if (is.null(x))          return(NA_real_)
    if (is.numeric(x))       return(as.numeric(x)[[1L]])
    if (is.list(x) && length(x)) return(suppressWarnings(as.numeric(x[[1L]])))
    suppressWarnings(as.numeric(x))
  }

  # Descarga de configuración JSON
  output$dl_json <- downloadHandler(
    filename = function()
      paste0("escenario_", format(Sys.Date(), "%Y%m%d"), ".json"),
    content = function(file)
      write_json(collect_scenario_inputs(), file,
                 pretty = TRUE, auto_unbox = TRUE)
  )

  # Carga de configuración JSON
  observeEvent(input$up_json, {
    req(input$up_json)
    j <- jsonlite::read_json(input$up_json$datapath, simplifyVector = FALSE)
    if (!is.null(j$itbis)) {
      if (!is.null(j$itbis$rate_grupo))
        tax_rv$rate_grupo    <- normalize_rate_list(j$itbis$rate_grupo)
      if (!is.null(j$itbis$rate_subclase))
        tax_rv$rate_subclase <- normalize_rate_list(j$itbis$rate_subclase)
      if (!is.null(j$itbis$rate_variedad))
        tax_rv$rate_variedad <- normalize_rate_list(j$itbis$rate_variedad)
    }
    if (!is.null(j$label)) {
      lv <- if (is.list(j$label)) as.character(j$label[[1]]) else as.character(j$label)
      updateTextInput(session, "scen_nombre", value = lv)
    }
    cust <- isTRUE(j$custom_isr) || (!is.null(j$isr) && isTRUE(j$isr$custom))
    updateCheckboxInput(session, "custom_isr", value = cust)
    if (!is.null(j$isr) && isTRUE(j$isr$custom)) {
      if (!is.null(j$isr$nom_renta))
        updateTextInput(session, "nom_renta",
                        value = as.character(j$isr$nom_renta[[1]] %||%
                                             j$isr$nom_renta))
      lim_inf <- j$isr$lim_inf;  lim_sup <- j$isr$lim_sup;  tpu <- j$isr$tasa_pct
      if (!is.null(lim_inf) && length(lim_inf) && !is.null(tpu) && length(tpu)) {
        for (i in seq_len(6L)) {
          if (length(lim_inf) >= i) {
            vi <- json_scalar_num(lim_inf[[i]])
            if (is.finite(vi)) updateNumericInput(session, paste0("isr_li_", i), value = vi)
          }
          if (!is.null(lim_sup) && length(lim_sup) >= i) {
            vs <- json_scalar_num(lim_sup[[i]])
            if (is.finite(vs)) updateNumericInput(session, paste0("isr_ls_", i), value = vs)
          }
          if (length(tpu) >= i) {
            vp <- json_scalar_num(tpu[[i]])
            if (is.finite(vp)) updateNumericInput(session, paste0("isr_tp_", i), value = vp)
          }
        }
      }
    }
    # sim_sub fijo en 1, sin desplegable que actualizar
    if (!is.null(j$sub_ele)) {
      se <- j$sub_ele
      cu <- if (is.null(se$custom)) FALSE
            else if (is.list(se$custom)) isTRUE(as.logical(se$custom[[1]]))
            else isTRUE(as.logical(se$custom))
      updateCheckboxInput(session, "custom_sub_ele", value = cu)
      if (isTRUE(cu)) {
        upd_sub7 <- function(nm, key) {
          vec <- se[[key]]
          if (is.null(vec)) return(invisible(NULL))
          for (jj in seq_len(7L)) {
            vj <- if (length(vec) >= jj) vec[[jj]] else NULL
            if (!is.null(vj))
              updateNumericInput(session, paste0(nm, jj),
                                 value = json_scalar_num(vj))
          }
        }
        upd_sub7("sub_block_", "block")
        upd_sub7("sub_bsur_",  "bsur");  upd_sub7("sub_bnorte_", "bnorte")
        upd_sub7("sub_beste_", "beste"); upd_sub7("sub_tsur_",   "tsur")
        upd_sub7("sub_tnorte_", "tnorte"); upd_sub7("sub_teste_", "teste")
        if (!is.null(se$nom_subsidio))
          updateTextInput(session, "sub_nom",
                          value = as.character(se$nom_subsidio[[1]] %||%
                                               se$nom_subsidio))
        for (cx in c("costsur","costnorte","costeste")) {
          if (!is.null(se[[cx]]))
            updateNumericInput(session, paste0("sub_", cx),
                               value = json_scalar_num(se[[cx]]))
        }
      }
    }
    # sim_com fijo en 1, sin desplegable que actualizar
    if (!is.null(j$comp)) {
      # Compatibilidad con JSON antiguo (enabled, sin_compensacion) y con con_comp
      enabled <- if (!is.null(j$comp$enabled)) {
        en <- j$comp$enabled
        if (is.list(en)) isTRUE(as.logical(en[[1]])) else isTRUE(as.logical(en))
      } else if (!is.null(j$comp$sin_compensacion)) {
        sn <- j$comp$sin_compensacion
        !isTRUE(if (is.list(sn)) as.logical(sn[[1]]) else as.logical(sn))
      } else FALSE
      updateCheckboxInput(session, "comp_con_comp", value = enabled)
      sn <- !enabled
      if (!sn) {
        if (!is.null(j$comp$grupo_com)) {
          gv <- if (is.list(j$comp$grupo_com)) j$comp$grupo_com[[1]] else j$comp$grupo_com
          updateSelectInput(session, "comp_grupo", selected = as.character(gv))
        }
        if (!is.null(j$comp$metodo_com)) {
          mv <- if (is.list(j$comp$metodo_com)) j$comp$metodo_com[[1]] else j$comp$metodo_com
          updateSelectInput(session, "comp_metodo", selected = as.character(mv))
        }
        if (!is.null(j$comp$valor_com))
          updateNumericInput(session, "comp_valor",
                             value = json_scalar_num(j$comp$valor_com))
        if (!is.null(j$comp$decil_est)) {
          dv <- if (is.list(j$comp$decil_est)) j$comp$decil_est[[1]] else j$comp$decil_est
          if (!is.na(suppressWarnings(as.integer(dv))))
            updateSelectInput(session, "comp_decil_est", selected = as.character(dv))
        }
        if (!is.null(j$comp$decil_com)) {
          dv <- if (is.list(j$comp$decil_com)) j$comp$decil_com[[1]] else j$comp$decil_com
          if (!is.na(suppressWarnings(as.integer(dv))))
            updateSelectInput(session, "comp_decil_com", selected = as.character(dv))
        }
        if (!is.null(j$comp$icv_com))
          updateNumericInput(session, "comp_icv",
                             value = json_scalar_num(j$comp$icv_com))
      }
    }
    showNotification("Configuraci\u00f3n cargada.", type = "message")
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
    slots <- list(scenarios_rv$slot1, scenarios_rv$slot2, scenarios_rv$slot3)
    # Si no hay espacios guardados, usar el estado actual de la interfaz
    if (all(vapply(slots, is.null, logical(1)))) {
      slots <- list(collect_scenario_inputs())
    }
    sim_res_rv(run_multi_dom(slots, paths()))
  })

  observeEvent(input$run_test, {
    inp <- read_analyst_fixture_scenario_inputs(root)
    sim_res_rv(run_multi_dom(list(inp), paths()))
  })

  sim_res <- reactive(sim_res_rv())

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

  output$dash_tbl_reg <- renderDT({
    shiny::validate(shiny::need(dom_list_ready(), need_run))
    req(input$dash_macro_esc)
    dl  <- sim_res()$resultados_escenarios
    one <- dl[input$dash_macro_esc]
    shiny::validate(shiny::need(length(one) == 1, "Escenario no v\u00e1lido."))
    tab <- tabla_macro_region(one,
                              escenario_headers = scenario_headers_named())
    datatable(tab, rownames = FALSE,
              options = list(scrollX = TRUE, dom = "ftip", pageLength = 12))
  })

  mk_inst_plot <- function(inst_key, title) {
    renderPlotly({
      shiny::validate(shiny::need(dom_list_ready(), need_run))
      ins <- ins_live_display()$incidencia[[inst_key]]$decinc
      shiny::validate(shiny::need(!is.null(ins),
                                  "Sin datos para este instrumento."))
      graficar_incidencia_plotly(
        ins, labels_inc$decinc, colnames(ins),
        plot_main_title      = title,
        y_axis_label_str     = "Porcentaje del ingreso disponible",
        hover_decimal_places = 2,
        font_size_base       = 11,
        scenario_tooltips    = NULL,
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
    dl  <- sim_res()$resultados_escenarios
    lbs <- sim_res()$scenario_labels %||% list()
    tab <- tibble::tibble(
      Espacio    = seq_along(dl),
      Etiqueta  = vapply(names(dl),
                         function(k) lbs[[k]] %||% k, character(1)),
      Detalle   = "Escenario definido en el constructor."
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
