# Escenario de prueba = contraste amplio (sim_itbis=2 → columnas itbis_alt2 en itbis_sim).

#' @param root Raíz del repositorio.
#' @return Lista compatible con [run_dom_scenario()] / `collect_scenario_inputs()`.
read_analyst_fixture_scenario_inputs <- function(root = ".") {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  p <- file.path(root, "tests", "fixtures", "scenario_analyst_escenario_2.json")
  if (!file.exists(p)) {
    stop("No se encontró el escenario de prueba: ", p)
  }
  j <- jsonlite::read_json(p, simplifyVector = TRUE)
  inp <- as.list(j)
  empty_rates <- list()
  if (is.null(inp$itbis)) {
    inp$itbis <- list(
      marco = "actual",
      rate_grupo = empty_rates,
      rate_subclase = empty_rates,
      rate_variedad = empty_rates
    )
  } else {
    it <- as.list(inp$itbis)
    it$rate_grupo <- if (length(it$rate_grupo)) as.list(it$rate_grupo) else empty_rates
    it$rate_subclase <- if (length(it$rate_subclase)) as.list(it$rate_subclase) else empty_rates
    it$rate_variedad <- if (length(it$rate_variedad)) as.list(it$rate_variedad) else empty_rates
    inp$itbis <- it
  }
  if (length(inp$isr) == 0L || identical(inp$isr, NA)) {
    inp$isr <- NULL
  }
  inp
}
