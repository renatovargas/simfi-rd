library(shiny)
library(openxlsx)
library(DT)

# ── Datos base ───────────────────────────────────────────────────────────────
BASE_TRAMOS <- data.frame(
  Lim_inf  = c(0,      301444, 416220, 624329,  867123,  2400000),
  Lim_sup  = c(301444, 416220, 624329, 867123,  2400000, NA),
  Tasa_pct = c(0,      5,      7,      9,       11,      13),
  stringsAsFactors = FALSE
)
N_BASE <- nrow(BASE_TRAMOS)

BASE_SUBSIDIOS <- data.frame(
  Bloque    = 1:7,
  Tasa_base = c(2, 4, 6, 8, 10, 12, 14),
  Tasa_kWh  = c(0.50, 1.00, 1.50, 2.00, 2.50, 3.00, 3.50),
  stringsAsFactors = FALSE
)

COMPANIAS <- c("Edesur", "Edenorte", "Edeste")

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: Arial, sans-serif; background-color: #f4f6f9; }
    .navbar { background-color: #c0392b !important; }
    .navbar-default .navbar-brand,
    .navbar-default .navbar-nav > li > a { color:white !important; font-weight:bold; }
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:hover { background-color:#922b21 !important; color:white !important; }
    .panel { border-radius:8px; box-shadow:0 2px 6px rgba(0,0,0,.12); margin-bottom:18px; }
    .panel-heading { color:white !important; font-weight:bold; border-radius:8px 8px 0 0 !important; font-size:15px; }
    .ph-base { background-color:#2c3e50 !important; }
    .ph-sim  { background-color:#c0392b !important; }
    .btn-confirmar { background-color:#c0392b; color:white; font-weight:bold; border:none; padding:10px 28px; border-radius:6px; margin-top:10px; }
    .btn-confirmar:hover { background-color:#922b21; color:white; }
    .btn-agregar { background-color:#2c3e50; color:white; border:none; border-radius:4px; padding:5px 13px; }
    .btn-agregar:hover { background-color:#1a252f; color:white; }
    .btn-quitar { background-color:#7f8c8d; color:white; border:none; border-radius:4px; padding:5px 13px; margin-left:5px; }
    .btn-quitar:hover { background-color:#566573; color:white; }
    .btn-copiar { background-color:#27ae60; color:white; border:none; border-radius:4px; padding:5px 13px; margin-left:5px; }
    .btn-copiar:hover { background-color:#1e8449; color:white; }
    .confirmed-badge { background-color:#27ae60; color:white; padding:4px 12px; border-radius:12px; font-size:13px; margin-left:10px; }
    .seccion-titulo { color:#c0392b; font-weight:bold; font-size:15px; border-bottom:2px solid #c0392b; padding-bottom:4px; margin-bottom:14px; margin-top:8px; }
    .disabled-input .form-control { background-color:#e9ecef !important; color:#6c757d !important; cursor:not-allowed; }
    table.dataTable thead th { background-color:#2c3e50; color:white; }
    hr { border-top:1px solid #ddd; }
    .readonly-cell { background-color:#f0f0f0 !important; color:#333; padding:6px 10px;
                     border:1px solid #ddd; border-radius:3px; text-align:right; font-size:13px; }
    .badge-readonly { background-color:#566573; color:white; font-size:10px;
                      padding:2px 7px; border-radius:8px; margin-left:8px; vertical-align:middle; }

    /* Tabs de escenario (compartido ISR / Subsidios / Compensaciones) */
    .esc-tabs { display:flex; gap:8px; margin-bottom:16px; flex-wrap:wrap; }
    .esc-tab-btn { background:#ecf0f1; color:#2c3e50; border:2px solid #bdc3c7; border-radius:6px;
                   padding:7px 18px; font-weight:bold; cursor:pointer; font-size:14px; transition:all .2s; }
    .esc-tab-btn.active { background:#c0392b; color:white; border-color:#c0392b; }
    .esc-tab-btn:hover:not(.active) { background:#d5d8dc; }
    .esc-panel { display:none; }
    .esc-panel.active { display:block; }

    /* Grilla ISR */
    .isr-tbl { width:100%; border-collapse:collapse; font-size:13px; }
    .isr-tbl th { padding:8px 10px; text-align:center; border:1px solid #bbb; }
    .isr-tbl th.th-num  { background-color:#566573; color:white; width:60px; }
    .isr-tbl th.th-inf  { background-color:#2c3e50; color:white; }
    .isr-tbl th.th-sup  { background-color:#2c3e50; color:white; }
    .isr-tbl th.th-tasa { background-color:#c0392b; color:white; width:120px; }
    .isr-tbl td { padding:3px 6px; border:1px solid #e0e0e0; vertical-align:middle; }
    .isr-tbl tr:nth-child(even) td { background-color:#f9f9f9; }
    .isr-tbl td.td-num { text-align:center; font-weight:bold; color:#555; background-color:#ecf0f1; }
    .isr-tbl .form-group { margin-bottom:0; }
    .isr-tbl .form-control { height:28px; padding:3px 7px; font-size:13px; }

    /* Grilla Subsidios */
    .sub-compania-header { background:#2c3e50; color:white; font-weight:bold; font-size:14px;
                           border-radius:6px; padding:8px 14px; margin-bottom:12px; }
    .sub-tbl { width:100%; border-collapse:collapse; font-size:13px; }
    .sub-tbl th { padding:8px 10px; text-align:center; border:1px solid #bbb; }
    .sub-tbl th.th-blq  { background-color:#566573; color:white; width:80px; }
    .sub-tbl th.th-tb   { background-color:#2c3e50; color:white; }
    .sub-tbl th.th-tkwh { background-color:#c0392b; color:white; }
    .sub-tbl td { padding:3px 6px; border:1px solid #e0e0e0; vertical-align:middle; }
    .sub-tbl tr:nth-child(even) td { background-color:#f9f9f9; }
    .sub-tbl td.td-blq { text-align:center; font-weight:bold; color:#555; background-color:#ecf0f1; }
    .sub-tbl .form-group { margin-bottom:0; }
    .sub-tbl .form-control { height:28px; padding:3px 7px; font-size:13px; }
    .sub-tbl td.inactivo { background-color:#f8f8f8; opacity:0.4; pointer-events:none; }

    /* Sin compensación */
    .sin-comp-badge { background:#e67e22; color:white; padding:6px 14px; border-radius:6px;
                      font-weight:bold; display:inline-block; margin-bottom:10px; font-size:13px; }
  "))),
  
  # JS genérico: activarTab(namespace, panelId)
  tags$script(HTML("
    function activarTab(ns, id) {
      document.querySelectorAll('.' + ns + '-panel').forEach(function(p) {
        p.classList.remove('active');
      });
      document.querySelectorAll('.' + ns + '-tab-btn').forEach(function(b) {
        b.classList.remove('active');
      });
      var panel = document.getElementById(ns + '-panel-' + id);
      if (panel) panel.classList.add('active');
      event.target.classList.add('active');
    }
  ")),
  
  navbarPage(title = "Simulador Fiscal", id = "tabs",
             
             # ══════════════════════════════════════════════════════════════════════
             # TAB 1: ISR  — tabs de escenario igual que Subsidios
             # ══════════════════════════════════════════════════════════════════════
             tabPanel("Impuesto sobre la Renta",
                      br(),
                      fluidRow(column(12,
                                      div(class = "panel panel-default",
                                          div(class = "panel-heading ph-sim", icon("table"), " Configuración de ISR — Escenarios"),
                                          div(class = "panel-body",
                                              
                                              # Tabs ISR
                                              tags$div(class = "esc-tabs",
                                                       tags$button("Base (solo lectura)", class = "isr-tab-btn esc-tab-btn active",
                                                                   onclick = "activarTab('isr','base')"),
                                                       tags$button("Escenario 1", class = "isr-tab-btn esc-tab-btn",
                                                                   onclick = "activarTab('isr','sim1')"),
                                                       tags$button("Escenario 2", class = "isr-tab-btn esc-tab-btn",
                                                                   onclick = "activarTab('isr','sim2')"),
                                                       tags$button("Escenario 3", class = "isr-tab-btn esc-tab-btn",
                                                                   onclick = "activarTab('isr','sim3')")
                                              ),
                                              
                                              # Panel Base ISR (solo lectura)
                                              tags$div(id = "isr-panel-base", class = "isr-panel esc-panel active",
                                                       div(style = "max-width:600px;", uiOutput("isr_base_tabla"))
                                              ),
                                              
                                              # Paneles ISR 1-3
                                              lapply(1:3, function(s) {
                                                tags$div(id = paste0("isr-panel-sim", s), class = "isr-panel esc-panel",
                                                         fluidRow(
                                                           column(5,
                                                                  textInput(paste0("isr_sim_nom", s), "Nombre del escenario",
                                                                            value = paste("Escenario", s))
                                                           ),
                                                           column(7, br(),
                                                                  actionButton(paste0("isr_copiar_base", s), "Copiar desde Base",
                                                                               class = "btn-copiar"),
                                                                  if (s > 1) span(lapply(seq_len(s - 1), function(prev) {
                                                                    actionButton(paste0("isr_copiar_sim", prev, "_to_", s),
                                                                                 paste("Copiar desde Esc.", prev),
                                                                                 class = "btn-copiar", style = "margin-left:5px;")
                                                                  })),
                                                                  actionButton(paste0("isr_add", s), "＋ Tramo",
                                                                               class = "btn-agregar", style = "margin-left:10px;"),
                                                                  actionButton(paste0("isr_rem", s), "－ Tramo",
                                                                               class = "btn-quitar")
                                                           )
                                                         ),
                                                         br(),
                                                         div(style = "max-width:600px;", uiOutput(paste0("isr_sim_tabla", s)))
                                                )
                                              })
                                          )
                                      ),
                                      
                                      # Vista previa ISR (sin botón de exportar individual)
                                      div(class = "panel panel-default",
                                          div(class = "panel-heading ph-base", icon("table"), " Vista Previa ISR"),
                                          div(class = "panel-body",
                                              DTOutput("isr_preview")
                                          )
                                      )
                      ))
             ),
             
             # ══════════════════════════════════════════════════════════════════════
             # TAB 2: SUBSIDIOS
             # ══════════════════════════════════════════════════════════════════════
             tabPanel("Subsidios",
                      br(),
                      fluidRow(column(12,
                                      div(class = "panel panel-default",
                                          div(class = "panel-heading ph-sim",
                                              "Configuración de Subsidios — Escenarios por Compañía"),
                                          div(class = "panel-body",
                                              
                                              tags$div(class = "esc-tabs",
                                                       tags$button("Base (solo lectura)", class = "sub-tab-btn esc-tab-btn active",
                                                                   onclick = "activarTab('sub','base')"),
                                                       tags$button("Escenario 1", class = "sub-tab-btn esc-tab-btn",
                                                                   onclick = "activarTab('sub','sim1')"),
                                                       tags$button("Escenario 2", class = "sub-tab-btn esc-tab-btn",
                                                                   onclick = "activarTab('sub','sim2')"),
                                                       tags$button("Escenario 3", class = "sub-tab-btn esc-tab-btn",
                                                                   onclick = "activarTab('sub','sim3')")
                                              ),
                                              
                                              # Panel Base Subsidios
                                              tags$div(id = "sub-panel-base", class = "sub-panel esc-panel active",
                                                       fluidRow(lapply(1:3, function(cid) {
                                                         column(4,
                                                                div(class = "sub-compania-header", icon("building"), " ", COMPANIAS[cid]),
                                                                uiOutput(paste0("sub_base_tbl_", cid))
                                                         )
                                                       }))
                                              ),
                                              
                                              # Paneles Subsidios 1-3
                                              lapply(1:3, function(s) {
                                                tags$div(id = paste0("sub-panel-sim", s), class = "sub-panel esc-panel",
                                                         fluidRow(
                                                           column(6,
                                                                  textInput(paste0("sub_sim_nom", s), "Nombre del escenario",
                                                                            value = paste("Escenario", s))
                                                           ),
                                                           column(6, br(),
                                                                  actionButton(paste0("sub_copiar_base", s), "Copiar desde Base",
                                                                               class = "btn-copiar"),
                                                                  if (s > 1) span(lapply(seq_len(s - 1), function(prev) {
                                                                    actionButton(paste0("sub_copiar_sim", prev, "_to_", s),
                                                                                 paste("Copiar desde Esc.", prev),
                                                                                 class = "btn-copiar", style = "margin-left:5px;")
                                                                  }))
                                                           )
                                                         ),
                                                         hr(),
                                                         fluidRow(lapply(1:3, function(cid) {
                                                           column(4,
                                                                  div(class = "sub-compania-header", icon("building"), " ", COMPANIAS[cid]),
                                                                  numericInput(paste0("sub_n", s, "_", cid),
                                                                               "Bloques activos (1-7)", value = 5, min = 1, max = 7),
                                                                  uiOutput(paste0("sub_sim_tbl_", s, "_", cid))
                                                           )
                                                         }))
                                                )
                                              })
                                          )
                                      ),
                                      
                                      # Vista previa Subsidios (sin botón de exportar individual)
                                      div(class = "panel panel-default",
                                          div(class = "panel-heading ph-base", icon("table"), " Vista Previa Subsidios"),
                                          div(class = "panel-body",
                                              DTOutput("sub_preview")
                                          )
                                      )
                      ))
             ),
             
             # ══════════════════════════════════════════════════════════════════════
             # TAB 3: COMPENSACIONES — 3 escenarios fijos, sin base
             # ══════════════════════════════════════════════════════════════════════
             tabPanel("Compensaciones",
                      br(),
                      fluidRow(column(12,
                                      div(class = "panel panel-default",
                                          div(class = "panel-heading ph-sim",
                                              "Configuración de Compensaciones — 3 Escenarios"),
                                          div(class = "panel-body",
                                              
                                              tags$div(class = "esc-tabs",
                                                       tags$button("Escenario 1", class = "comp-tab-btn esc-tab-btn active",
                                                                   onclick = "activarTab('comp','sim1')"),
                                                       tags$button("Escenario 2", class = "comp-tab-btn esc-tab-btn",
                                                                   onclick = "activarTab('comp','sim2')"),
                                                       tags$button("Escenario 3", class = "comp-tab-btn esc-tab-btn",
                                                                   onclick = "activarTab('comp','sim3')")
                                              ),
                                              
                                              lapply(1:3, function(s) {
                                                tags$div(
                                                  id    = paste0("comp-panel-sim", s),
                                                  class = paste("comp-panel esc-panel", if (s == 1) "active" else ""),
                                                  
                                                  # Nombre + copiar
                                                  fluidRow(
                                                    column(5,
                                                           textInput(paste0("comp_sim_nom", s), "Nombre del escenario",
                                                                     value = paste("Escenario", s))
                                                    ),
                                                    column(7, br(),
                                                           if (s > 1) span(lapply(seq_len(s - 1), function(prev) {
                                                             actionButton(paste0("comp_copiar_sim", prev, "_to_", s),
                                                                          paste("Copiar desde Esc.", prev),
                                                                          class = "btn-copiar", style = "margin-left:5px;")
                                                           }))
                                                    )
                                                  ),
                                                  hr(),
                                                  
                                                  # Checkbox Sin compensación
                                                  checkboxInput(paste0("comp_sin_comp", s),
                                                                tags$strong("Sin compensación (desactiva todos los campos)"),
                                                                value = FALSE),
                                                  br(),
                                                  
                                                  # Campos dinámicos
                                                  uiOutput(paste0("comp_campos_ui", s))
                                                )
                                              })
                                          )
                                      ),
                                      
                                      # Vista previa Compensaciones (sin botón de exportar individual)
                                      div(class = "panel panel-default",
                                          div(class = "panel-heading ph-base", icon("table"), " Vista Previa Compensaciones"),
                                          div(class = "panel-body",
                                              DTOutput("comp_preview")
                                          )
                                      )
                      ))
             ),
             
             # ══════════════════════════════════════════════════════════════════════
             # TAB 4: EXPORTAR — botón único para todo
             # ══════════════════════════════════════════════════════════════════════
             tabPanel("Exportar",
                      br(),
                      fluidRow(column(12,
                                      div(class = "panel panel-default",
                                          div(class = "panel-heading ph-sim", icon("download"), " Exportar todos los parámetros"),
                                          div(class = "panel-body",
                                              
                                              p(style = "color:#555; font-size:14px;",
                                                "Genera un único archivo Excel con tres hojas: ",
                                                tags$b("ISR"), ", ", tags$b("Subsidios"), " y ", tags$b("Compensaciones"), "."),
                                              br(),
                                              
                                              div(class = "seccion-titulo", "ISR — Vista Previa"),
                                              DTOutput("export_isr_preview"),
                                              br(),
                                              div(class = "seccion-titulo", "Subsidios — Vista Previa"),
                                              DTOutput("export_sub_preview"),
                                              br(),
                                              div(class = "seccion-titulo", "Compensaciones — Vista Previa"),
                                              DTOutput("export_comp_preview"),
                                              br(),
                                              downloadButton("exportar_todo", "⬇ Descargar Excel (todas las hojas)",
                                                             class = "btn-confirmar"),
                                              uiOutput("export_badge")
                                          )
                                      )
                      ))
             )
  )
)

# ── SERVER ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ════════════════════════════════════════════════════════════════════════
  # ISR
  # ════════════════════════════════════════════════════════════════════════
  isr_state <- reactiveValues(
    n    = rep(N_BASE, 3),
    data = lapply(1:3, function(x) BASE_TRAMOS)
  )
  
  output$isr_base_tabla <- renderUI({
    rows <- lapply(seq_len(N_BASE), function(i) {
      tags$tr(
        tags$td(class = "td-num", i),
        tags$td(div(class = "readonly-cell",
                    formatC(BASE_TRAMOS$Lim_inf[i], format = "f", digits = 0, big.mark = ","))),
        tags$td(div(class = "readonly-cell",
                    if (is.na(BASE_TRAMOS$Lim_sup[i])) "Sin límite"
                    else formatC(BASE_TRAMOS$Lim_sup[i], format = "f", digits = 0, big.mark = ","))),
        tags$td(div(class = "readonly-cell", paste0(BASE_TRAMOS$Tasa_pct[i], " %")))
      )
    })
    tags$table(class = "isr-tbl",
               tags$thead(tags$tr(
                 tags$th(class = "th-num",  "#"),
                 tags$th(class = "th-inf",  "Límite inferior (Q)"),
                 tags$th(class = "th-sup",  "Límite superior (Q)"),
                 tags$th(class = "th-tasa", "Tasa (%)")
               )),
               tags$tbody(rows)
    )
  })
  
  make_isr_tabla <- function(s) {
    renderUI({
      n   <- isr_state$n[s]
      dat <- isr_state$data[[s]]
      rows <- lapply(seq_len(n), function(i) {
        li  <- if (i <= nrow(dat)) dat$Lim_inf[i]  else 0
        ls  <- if (i <= nrow(dat)) dat$Lim_sup[i]  else NA
        tas <- if (i <= nrow(dat)) dat$Tasa_pct[i] else 0
        tags$tr(
          tags$td(class = "td-num", i),
          tags$td(numericInput(paste0("isr_li_",   s, "_", i), NULL,
                               value = li,  min = 0, step = 1000)),
          tags$td(numericInput(paste0("isr_ls_",   s, "_", i), NULL,
                               value = ls,  min = 0, step = 1000)),
          tags$td(numericInput(paste0("isr_tasa_", s, "_", i), NULL,
                               value = tas, min = 0, max = 100, step = 0.5))
        )
      })
      tags$table(class = "isr-tbl",
                 tags$thead(tags$tr(
                   tags$th(class = "th-num",  "#"),
                   tags$th(class = "th-inf",  "Límite inferior (Q)"),
                   tags$th(class = "th-sup",  "Límite superior (Q)"),
                   tags$th(class = "th-tasa", "Tasa (%)")
                 )),
                 tags$tbody(rows)
      )
    })
  }
  output$isr_sim_tabla1 <- make_isr_tabla(1)
  output$isr_sim_tabla2 <- make_isr_tabla(2)
  output$isr_sim_tabla3 <- make_isr_tabla(3)
  
  isr_leer_inputs <- function(s) {
    n  <- isr_state$n[s]
    df <- data.frame(Lim_inf = numeric(n), Lim_sup = numeric(n), Tasa_pct = numeric(n))
    for (i in seq_len(n)) {
      df$Lim_inf[i]  <- input[[paste0("isr_li_",   s, "_", i)]] %||% 0
      df$Lim_sup[i]  <- input[[paste0("isr_ls_",   s, "_", i)]] %||% NA
      df$Tasa_pct[i] <- input[[paste0("isr_tasa_", s, "_", i)]] %||% 0
    }
    df
  }
  
  isr_copiar_a <- function(dest, src_data, src_n) {
    isr_state$n[dest]      <- src_n
    isr_state$data[[dest]] <- src_data
    for (i in seq_len(src_n)) {
      updateNumericInput(session, paste0("isr_li_",   dest, "_", i), value = src_data$Lim_inf[i])
      updateNumericInput(session, paste0("isr_ls_",   dest, "_", i), value = src_data$Lim_sup[i])
      updateNumericInput(session, paste0("isr_tasa_", dest, "_", i), value = src_data$Tasa_pct[i])
    }
    showNotification(paste("Escenario ISR", dest, "actualizado."), type = "message")
  }
  
  for (s in 1:3) {
    local({
      ss <- s
      observeEvent(input[[paste0("isr_add", ss)]], {
        isr_state$n[ss] <- min(isr_state$n[ss] + 1, 10)
      })
      observeEvent(input[[paste0("isr_rem", ss)]], {
        isr_state$n[ss] <- max(isr_state$n[ss] - 1, 1)
      })
      observeEvent(input[[paste0("isr_copiar_base", ss)]], {
        isr_copiar_a(ss, BASE_TRAMOS, N_BASE)
      })
    })
  }
  for (s in 2:3) {
    for (prev in seq_len(s - 1)) {
      local({
        ss <- s; pp <- prev
        observeEvent(input[[paste0("isr_copiar_sim", pp, "_to_", ss)]], {
          isr_copiar_a(ss, isr_leer_inputs(pp), isr_state$n[pp])
        })
      })
    }
  }
  
  isr_leer_escenario <- function(s) {
    n   <- isr_state$n[s]
    nom <- input[[paste0("isr_sim_nom", s)]] %||% paste("Escenario", s)
    rows <- list()
    for (i in seq_len(n)) {
      li  <- input[[paste0("isr_li_",   s, "_", i)]]
      ls  <- input[[paste0("isr_ls_",   s, "_", i)]]
      tas <- input[[paste0("isr_tasa_", s, "_", i)]]
      if (is.null(li) || is.null(tas)) next
      rows[[length(rows)+1]] <- data.frame(
        Escenario = nom, Tramo = i,
        Lim_inf  = li,
        Lim_sup  = ifelse(is.null(ls) || is.na(ls), NA, ls),
        Tasa_pct = tas, stringsAsFactors = FALSE)
    }
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  }
  
  isr_data <- reactive({
    base_df <- data.frame(Escenario = "Base", Tramo = seq_len(N_BASE),
                          BASE_TRAMOS, stringsAsFactors = FALSE)
    sims <- Filter(Negate(is.null), lapply(1:3, isr_leer_escenario))
    if (length(sims) == 0) return(base_df)
    rbind(base_df, do.call(rbind, sims))
  })
  
  output$isr_preview <- renderDT({
    df <- isr_data()
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(c("Lim_inf", "Lim_sup"), currency = "Q", digits = 0) %>%
      formatString("Tasa_pct", suffix = " %")
  })
  
  # Previews en pestaña Exportar
  output$export_isr_preview <- renderDT({
    df <- isr_data()
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE) %>%
      formatCurrency(c("Lim_inf", "Lim_sup"), currency = "Q", digits = 0) %>%
      formatString("Tasa_pct", suffix = " %")
  })
  
  # ════════════════════════════════════════════════════════════════════════
  # SUBSIDIOS
  # ════════════════════════════════════════════════════════════════════════
  sub_state <- reactiveValues(
    n    = matrix(5, nrow = 3, ncol = 3),
    data = lapply(1:3, function(s) lapply(1:3, function(cid) BASE_SUBSIDIOS))
  )
  
  make_sub_base_tbl <- function(cid) {
    renderUI({
      rows <- lapply(1:7, function(i) {
        tags$tr(
          tags$td(class = "td-blq", i),
          tags$td(div(class = "readonly-cell", paste0(BASE_SUBSIDIOS$Tasa_base[i], " %"))),
          tags$td(div(class = "readonly-cell", paste0("Q ", BASE_SUBSIDIOS$Tasa_kWh[i])))
        )
      })
      tags$table(class = "sub-tbl",
                 tags$thead(tags$tr(
                   tags$th(class = "th-blq",  "Bloque"),
                   tags$th(class = "th-tb",   "Tasa base (%)"),
                   tags$th(class = "th-tkwh", "Tasa (Q/kWh)")
                 )),
                 tags$tbody(rows)
      )
    })
  }
  output$sub_base_tbl_1 <- make_sub_base_tbl(1)
  output$sub_base_tbl_2 <- make_sub_base_tbl(2)
  output$sub_base_tbl_3 <- make_sub_base_tbl(3)
  
  make_sub_sim_tbl <- function(s, cid) {
    renderUI({
      n_act <- sub_state$n[s, cid]
      dat   <- sub_state$data[[s]][[cid]]
      rows <- lapply(1:7, function(i) {
        activo <- i <= n_act
        tb   <- if (i <= nrow(dat)) dat$Tasa_base[i] else i * 2
        tkwh <- if (i <= nrow(dat)) dat$Tasa_kWh[i]  else round(0.5 * i, 2)
        td_cl <- if (!activo) "inactivo" else ""
        tags$tr(
          tags$td(class = paste("td-blq", td_cl), i,
                  if (!activo) tags$small(" (inact.)") else NULL),
          tags$td(class = td_cl,
                  numericInput(paste0("sub_tb_",   s, "_", cid, "_", i), NULL,
                               value = tb,   min = 0, max = 100, step = 0.1)),
          tags$td(class = td_cl,
                  numericInput(paste0("sub_tkwh_", s, "_", cid, "_", i), NULL,
                               value = tkwh, min = 0, step = 0.01))
        )
      })
      tags$table(class = "sub-tbl",
                 tags$thead(tags$tr(
                   tags$th(class = "th-blq",  "Bloque"),
                   tags$th(class = "th-tb",   "Tasa base (%)"),
                   tags$th(class = "th-tkwh", "Tasa (Q/kWh)")
                 )),
                 tags$tbody(rows)
      )
    })
  }
  
  for (s in 1:3) {
    for (cid in 1:3) {
      local({
        ss <- s; cc <- cid
        output[[paste0("sub_sim_tbl_", ss, "_", cc)]] <- make_sub_sim_tbl(ss, cc)
        observeEvent(input[[paste0("sub_n", ss, "_", cc)]], {
          sub_state$n[ss, cc] <- input[[paste0("sub_n", ss, "_", cc)]]
        })
      })
    }
  }
  
  sub_leer_inputs <- function(s) {
    lapply(1:3, function(cid) {
      df <- data.frame(Bloque = 1:7, Tasa_base = NA_real_, Tasa_kWh = NA_real_)
      for (i in 1:7) {
        df$Tasa_base[i] <- input[[paste0("sub_tb_",   s, "_", cid, "_", i)]] %||%
          BASE_SUBSIDIOS$Tasa_base[i]
        df$Tasa_kWh[i]  <- input[[paste0("sub_tkwh_", s, "_", cid, "_", i)]] %||%
          BASE_SUBSIDIOS$Tasa_kWh[i]
      }
      df
    })
  }
  
  sub_copiar_a <- function(dest, origen_data, origen_n) {
    for (cid in 1:3) {
      sub_state$data[[dest]][[cid]] <- origen_data[[cid]]
      sub_state$n[dest, cid]        <- origen_n[cid]
      updateNumericInput(session, paste0("sub_n", dest, "_", cid), value = origen_n[cid])
      for (i in 1:7) {
        updateNumericInput(session, paste0("sub_tb_",   dest, "_", cid, "_", i),
                           value = origen_data[[cid]]$Tasa_base[i])
        updateNumericInput(session, paste0("sub_tkwh_", dest, "_", cid, "_", i),
                           value = origen_data[[cid]]$Tasa_kWh[i])
      }
    }
    showNotification(paste("Escenario Subsidios", dest, "actualizado."), type = "message")
  }
  
  for (s in 1:3) {
    local({
      ss <- s
      observeEvent(input[[paste0("sub_copiar_base", ss)]], {
        sub_copiar_a(ss,
                     origen_data = lapply(1:3, function(x) BASE_SUBSIDIOS),
                     origen_n    = rep(5, 3))
      })
    })
  }
  for (s in 2:3) {
    for (prev in seq_len(s - 1)) {
      local({
        ss <- s; pp <- prev
        observeEvent(input[[paste0("sub_copiar_sim", pp, "_to_", ss)]], {
          sub_copiar_a(ss,
                       origen_data = sub_leer_inputs(pp),
                       origen_n    = sapply(1:3, function(cid)
                         input[[paste0("sub_n", pp, "_", cid)]] %||% 5))
        })
      })
    }
  }
  
  leer_sub_escenario <- function(s) {
    nom <- input[[paste0("sub_sim_nom", s)]] %||% paste("Escenario", s)
    rows <- list()
    for (cid in 1:3) {
      n_act <- sub_state$n[s, cid]
      for (i in 1:n_act) {
        tb   <- input[[paste0("sub_tb_",   s, "_", cid, "_", i)]]
        tkwh <- input[[paste0("sub_tkwh_", s, "_", cid, "_", i)]]
        if (is.null(tb) || is.null(tkwh)) next
        rows[[length(rows)+1]] <- data.frame(
          Escenario = nom, Compania = COMPANIAS[cid],
          Bloque = i, Tasa_base = tb, Tasa_kWh = tkwh,
          stringsAsFactors = FALSE)
      }
    }
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  }
  
  sub_data <- reactive({
    base_rows <- do.call(rbind, lapply(1:3, function(cid) {
      do.call(rbind, lapply(1:7, function(i) {
        data.frame(Escenario = "Base", Compania = COMPANIAS[cid],
                   Bloque = i, Tasa_base = BASE_SUBSIDIOS$Tasa_base[i],
                   Tasa_kWh = BASE_SUBSIDIOS$Tasa_kWh[i], stringsAsFactors = FALSE)
      }))
    }))
    sims <- Filter(Negate(is.null), lapply(1:3, leer_sub_escenario))
    if (length(sims) == 0) return(base_rows)
    rbind(base_rows, do.call(rbind, sims))
  })
  
  output$sub_preview <- renderDT({
    df <- sub_data()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "Sin datos")))
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  output$export_sub_preview <- renderDT({
    df <- sub_data()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "Sin datos")))
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  # ════════════════════════════════════════════════════════════════════════
  # COMPENSACIONES — 3 escenarios fijos, sin base, opción sin compensación
  # ════════════════════════════════════════════════════════════════════════
  
  # UI dinámica de campos por escenario
  make_comp_campos_ui <- function(s) {
    renderUI({
      sin_comp <- isTRUE(input[[paste0("comp_sin_comp", s)]])
      
      if (sin_comp) {
        return(div(
          div(class = "sin-comp-badge", icon("ban"), " Sin compensación activa en este escenario"),
          p(style = "color:#7f8c8d; font-size:13px; margin-top:6px;",
            "Se exportará con todos los campos en blanco.")
        ))
      }
      
      metodo <- input[[paste0("comp_metodo", s)]] %||% "1"
      grupo  <- input[[paste0("comp_grupo",  s)]] %||% "1"
      
      tagList(
        fluidRow(
          column(4,
                 div(class = "seccion-titulo", "Grupo de Compensación"),
                 selectInput(paste0("comp_grupo", s), NULL,
                             choices = c("1. Beneficiarios Supérate" = "1",
                                         "2. Decil de Ingresos"      = "2",
                                         "3. ICV"                    = "3"),
                             selected = grupo)
          ),
          column(4,
                 div(class = "seccion-titulo", "Método de Compensación"),
                 selectInput(paste0("comp_metodo", s), NULL,
                             choices = c("1. Pérdida neta" = "1", "2. Valor fijo" = "2"),
                             selected = metodo)
          ),
          column(4,
                 div(class = "seccion-titulo", "Valor Fijo"),
                 if (metodo == "2")
                   numericInput(paste0("comp_valor", s), "valor_com (Q)", value = 0, min = 0, step = 10)
                 else
                   div(class = "disabled-input",
                       numericInput(paste0("comp_valor_dis", s), "valor_com (Q)", value = NA),
                       helpText("Se activa con Método 2"))
          )
        ),
        fluidRow(
          column(4, {
            ok <- grupo == "2" && metodo == "2"
            if (ok)
              div(div(class = "seccion-titulo", "Decil de Ingresos"),
                  selectInput(paste0("comp_decil", s), NULL,
                              choices = c("Seleccione..." = "", paste0("Decil ", 1:10))))
            else
              div(div(class = "seccion-titulo", style = "color:#999;border-color:#ccc;",
                      "Decil de Ingresos"),
                  div(class = "disabled-input",
                      selectInput(paste0("comp_decil_dis", s), NULL,
                                  choices = c("(inactivo)" = "")),
                      helpText("Requiere Grupo 2 + Método 2")))
          }),
          column(4, {
            ok <- grupo == "3" && metodo == "2"
            if (ok)
              div(div(class = "seccion-titulo", "ICV"),
                  numericInput(paste0("comp_icv", s), "icv_com", value = 0, min = 0, step = 0.1))
            else
              div(div(class = "seccion-titulo", style = "color:#999;border-color:#ccc;", "ICV"),
                  div(class = "disabled-input",
                      numericInput(paste0("comp_icv_dis", s), "icv_com", value = NA),
                      helpText("Requiere Grupo 3 + Método 2")))
          }),
          column(4, {
            if (metodo == "2")
              div(div(class = "seccion-titulo", "Decil Estatal"),
                  selectInput(paste0("comp_decil_est", s), NULL,
                              choices = c("Seleccione..." = "", paste0("Decil ", 1:10))))
            else
              div(div(class = "seccion-titulo", style = "color:#999;border-color:#ccc;",
                      "Decil Estatal"),
                  div(class = "disabled-input",
                      selectInput(paste0("comp_decil_est_dis", s), NULL,
                                  choices = c("(inactivo)" = "")),
                      helpText("Requiere Método 2")))
          })
        )
      )
    })
  }
  
  output$comp_campos_ui1 <- make_comp_campos_ui(1)
  output$comp_campos_ui2 <- make_comp_campos_ui(2)
  output$comp_campos_ui3 <- make_comp_campos_ui(3)
  
  # Copiar entre escenarios de compensaciones
  comp_copiar_a <- function(dest, src) {
    # Checkbox
    val_sin <- input[[paste0("comp_sin_comp", src)]]
    if (!is.null(val_sin))
      updateCheckboxInput(session, paste0("comp_sin_comp", dest), value = val_sin)
    # Selects y numerics (solo si hay campos visibles)
    for (campo in c("comp_grupo", "comp_metodo", "comp_decil", "comp_decil_est")) {
      val <- input[[paste0(campo, src)]]
      if (!is.null(val)) updateSelectInput(session, paste0(campo, dest), selected = val)
    }
    for (campo in c("comp_valor", "comp_icv")) {
      val <- input[[paste0(campo, src)]]
      if (!is.null(val)) updateNumericInput(session, paste0(campo, dest), value = val)
    }
    showNotification(paste("Escenario Compensaciones", dest, "actualizado."), type = "message")
  }
  
  for (s in 2:3) {
    for (prev in seq_len(s - 1)) {
      local({
        ss <- s; pp <- prev
        observeEvent(input[[paste0("comp_copiar_sim", pp, "_to_", ss)]], {
          comp_copiar_a(ss, pp)
        })
      })
    }
  }
  
  leer_comp_escenario <- function(s) {
    nom      <- input[[paste0("comp_sim_nom", s)]] %||% paste("Escenario", s)
    sin_comp <- isTRUE(input[[paste0("comp_sin_comp", s)]])
    if (sin_comp) {
      return(data.frame(
        Escenario = nom, sin_compensacion = "Sí",
        grupo_com = NA_character_, metodo_com = NA_character_,
        valor_com = NA_real_, decil_com = NA_character_,
        icv_com   = NA_real_, decil_est = NA_character_,
        stringsAsFactors = FALSE))
    }
    met <- input[[paste0("comp_metodo", s)]] %||% "1"
    grp <- input[[paste0("comp_grupo",  s)]] %||% "1"
    g   <- switch(grp, "1" = "Beneficiarios Supérate", "2" = "Decil de Ingresos", "3" = "ICV")
    m   <- switch(met, "1" = "Pérdida neta", "2" = "Valor fijo")
    data.frame(
      Escenario        = nom,
      sin_compensacion = "No",
      grupo_com        = g,
      metodo_com       = m,
      valor_com        = if (met == "2") input[[paste0("comp_valor", s)]] %||% 0 else NA,
      decil_com        = if (grp == "2" && met == "2")
        input[[paste0("comp_decil",     s)]] %||% NA else NA,
      icv_com          = if (grp == "3" && met == "2")
        input[[paste0("comp_icv",       s)]] %||% NA else NA,
      decil_est        = if (met == "2")
        input[[paste0("comp_decil_est", s)]] %||% NA else NA,
      stringsAsFactors = FALSE
    )
  }
  
  comp_data <- reactive({
    do.call(rbind, lapply(1:3, leer_comp_escenario))
  })
  
  output$comp_preview <- renderDT({
    datatable(comp_data(), options = list(scrollX = TRUE, dom = "t"), rownames = FALSE)
  })
  
  output$export_comp_preview <- renderDT({
    datatable(comp_data(), options = list(scrollX = TRUE, dom = "t"), rownames = FALSE)
  })
  
  # ════════════════════════════════════════════════════════════════════════
  # EXPORTAR UNIFICADO — un solo Excel con 3 hojas
  # ════════════════════════════════════════════════════════════════════════
  export_badge_val <- reactiveVal(FALSE)
  
  output$exportar_todo <- downloadHandler(
    filename = function() {
      paste0("SimuladorFiscal_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      # ── Hoja 1: ISR ──────────────────────────────────────────────────────
      df_isr <- isr_data()
      addWorksheet(wb, "ISR")
      hs_isr <- createStyle(fgFill = "#2C3E50", fontColour = "white",
                            textDecoration = "bold", halign = "center")
      writeData(wb, "ISR", df_isr, headerStyle = hs_isr)
      base_rows_isr <- which(df_isr$Escenario == "Base") + 1
      if (length(base_rows_isr) > 0)
        addStyle(wb, "ISR", createStyle(fgFill = "#ECF0F1"),
                 rows = base_rows_isr, cols = 1:ncol(df_isr), gridExpand = TRUE)
      setColWidths(wb, "ISR", cols = 1:ncol(df_isr), widths = "auto")
      
      # ── Hoja 2: Subsidios ─────────────────────────────────────────────────
      df_sub <- sub_data()
      addWorksheet(wb, "Subsidios")
      hs_sub <- createStyle(fgFill = "#C0392B", fontColour = "white",
                            textDecoration = "bold", halign = "center")
      writeData(wb, "Subsidios", df_sub, headerStyle = hs_sub)
      base_rows_sub <- which(df_sub$Escenario == "Base") + 1
      if (length(base_rows_sub) > 0)
        addStyle(wb, "Subsidios", createStyle(fgFill = "#ECF0F1"),
                 rows = base_rows_sub, cols = 1:ncol(df_sub), gridExpand = TRUE)
      setColWidths(wb, "Subsidios", cols = 1:ncol(df_sub), widths = "auto")
      
      # ── Hoja 3: Compensaciones ────────────────────────────────────────────
      df_comp <- comp_data()
      addWorksheet(wb, "Compensaciones")
      hs_comp <- createStyle(fgFill = "#8E44AD", fontColour = "white",
                             textDecoration = "bold", halign = "center")
      writeData(wb, "Compensaciones", df_comp, headerStyle = hs_comp)
      sin_rows <- which(df_comp$sin_compensacion == "Sí") + 1
      if (length(sin_rows) > 0)
        addStyle(wb, "Compensaciones", createStyle(fgFill = "#FAD7A0"),
                 rows = sin_rows, cols = 1:ncol(df_comp), gridExpand = TRUE)
      setColWidths(wb, "Compensaciones", cols = 1:ncol(df_comp), widths = "auto")
      
      saveWorkbook(wb, file, overwrite = TRUE)
      export_badge_val(TRUE)
    }
  )
  
  output$export_badge <- renderUI({
    if (export_badge_val()) span(class = "confirmed-badge", "✔ Archivo descargado")
  })
}

shinyApp(ui, server)