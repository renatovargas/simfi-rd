# Source all engine files in dependency order (for Shiny / scripts).
autoload_dom_r <- function(root = ".") {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  files <- c(
    "config.R",
    "param_csv.R",
    "load_parameters.R",
    "isr_bracket_helpers.R",
    "sim_sub_helpers.R",
    "pipeline_runner.R",
    "insumos_dictionary.R",
    "module_summary.R",
    "analyst_fixture_scenario.R",
    "dom_dashboard_charts.R",
    "itbis_catalog_builder.R",
    "run_dom_scenario.R",
    "scenario_store.R"
  )
  for (f in files) {
    src <- file.path(root, "R", f)
    if (!file.exists(src)) {
      stop("Falta archivo R del motor: ", src)
    }
    source(src, local = FALSE)
  }
  invisible(TRUE)
}
