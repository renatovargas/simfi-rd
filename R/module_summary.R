# Summary / consolidation is executed in `05_DOM26_ConsolidaEscenarios.R`.
# Helpers here adapt its output for the Shiny UI.

#' @param resultados_escenarios Named list (`escenario_k` -> summary lists).
#' @return Same list (identity); kept for a stable extension point.
prepare_dom_list <- function(resultados_escenarios) {
  resultados_escenarios
}

#' @param resultados_escenarios From [run_dom_scenario()] when `return = "both"`.
#' @param escenario_num Integer scenario id (usually `1`).
#' @return List suitable for [build_insumos_live()] input.
get_escenario_summary <- function(resultados_escenarios, escenario_num = 1L) {
  key <- paste0("escenario_", escenario_num)
  if (!key %in% names(resultados_escenarios)) {
    key <- names(resultados_escenarios)[1]
  }
  resultados_escenarios[[key]]
}
