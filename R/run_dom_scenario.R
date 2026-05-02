prepare_pipeline_extras <- function(inputs, paths) {
  extras <- list()
  cat <- load_itbis_catalog(paths$root)
  tpl <- read_param_csv("itbis_sim", paths$param_csv)
  it <- inputs$itbis %||% list()
  extras$detalle_itbis_override <- build_detalle_itbis_override(
    cat,
    tpl,
    it$rate_grupo %||% list(),
    it$rate_subclase %||% list(),
    it$rate_variedad %||% list(),
    it$marco %||% "actual"
  )
  isr <- inputs$isr
  if (!is.null(isr) && isTRUE(isr$custom)) {
    extras$sim_renta_override <- build_sim_renta_override(
      paths$param_csv,
      isr$tramos,
      isr$bases,
      isr$tasas,
      isr$nom_renta %||% "Renta personalizada",
      sim_inc = as.integer(inputs$sim_renta %||% 1L)
    )
  }
  sub_el <- inputs$sub_ele %||% list()
  if (isTRUE(sub_el$custom)) {
    tmpl <- read_sim_sub_template_row(
      paths$param_csv,
      as.integer(inputs$sim_sub %||% 0L)
    )
    extras$sim_sub_row_override <- patch_sim_sub_row_from_ui(tmpl, sub_el)
  }

  comp <- inputs[["comp"]] %||% list()
  if (isTRUE(comp$enabled) && !isTRUE(comp$sin_compensacion)) {
    extras$sim_comp_override <- build_sim_comp_override_row(
      sim_comp_id = as.integer(comp$sim_comp_id %||% inputs$sim_com %||% 1L),
      grupo_com = as.integer(comp$grupo_com %||% 1L),
      metodo_com = as.integer(comp$metodo_com %||% 1L),
      valor_com = as.numeric(comp$valor_com %||% 0),
      decil_com = if (is.null(comp$decil_com) || is.na(comp$decil_com)) {
        NA_integer_
      } else {
        as.integer(comp$decil_com)
      },
      icv_com = if (is.null(comp$icv_com) || is.na(comp$icv_com)) {
        NA_real_
      } else {
        as.numeric(comp$icv_com)
      },
      decil_est = if (is.null(comp$decil_est) || is.na(comp$decil_est)) {
        NA_integer_
      } else {
        as.integer(comp$decil_est)
      }
    )
  }
  extras
}

#' Run baseline + one counterfactual scenario through the full pipeline.
#'
#' @param inputs Scenario fields; see [default_scenario_inputs()] and
#'   [scenario_inputs_to_escenarios()]. Debe incluir `itbis` (listas de tasas)
#'   y opcionalmente `isr` con `custom=TRUE` y vectores `tramos`, `bases`, `tasas`.
#' @param paths Optional; from [get_dom_paths()].
#' @param root Used if `paths` is NULL.
#' @param return `"summary"` (named list of scenario summaries), `"micro"`
#'   (`DOM_results` microdata), or `"both"` (list with both plus metadata).
run_dom_scenario <- function(
    inputs = list(),
    paths = NULL,
    root = ".",
    return = c("summary", "micro", "both")
) {
  return <- match.arg(return)
  if (is.null(paths)) {
    paths <- get_dom_paths(root)
  }
  escenarios <- scenario_inputs_to_escenarios(inputs)
  extras <- prepare_pipeline_extras(inputs, paths)
  res <- run_pipeline_cached(
    paths,
    escenarios,
    write_intermediate = isTRUE(getOption("dom.pipeline.write_rds", FALSE)),
    extras = extras
  )
  scenario_label <- as.character(inputs$label %||% escenarios$des_corto[1])

  if (return == "summary") {
    return(prepare_dom_list(res$resultados_escenarios))
  }
  if (return == "micro") {
    return(res$dom_results)
  }

  list(
    resultados_escenarios = prepare_dom_list(res$resultados_escenarios),
    dom_results = res$dom_results,
    insumos_live = build_insumos_live(
      get_escenario_summary(res$resultados_escenarios, escenarios$escenario[1]),
      escenario_num = as.integer(escenarios$escenario[1])
    ),
    scenario_label = scenario_label,
    escenarios = escenarios,
    paths = paths
  )
}
