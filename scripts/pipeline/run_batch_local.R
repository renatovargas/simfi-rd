## Local batch entry point: same engine as the Shiny app (no `escenarios` sheet
## required for orchestration — pass scenario fields in `inputs`).
##
## Usage from repo root:
##   Rscript scripts/pipeline/run_batch_local.R
##
## Optional: options(dom.pipeline.write_rds = TRUE) before sourcing to persist
## intermediate RDS under data/mod/.

root <- normalizePath(".", winslash = "/", mustWork = FALSE)
source(file.path(root, "R", "00_autoload.R"))
autoload_dom_r(root)

paths <- get_dom_paths(root)
inputs <- default_scenario_inputs()
inputs$label <- "batch_run"

res <- run_dom_scenario(inputs, paths = paths, return = "both")
cat("Escenario:", res$scenario_label, "\n")
cat("Claves resumen:", paste(names(res$resultados_escenarios), collapse = ", "), "\n")
invisible(res)
