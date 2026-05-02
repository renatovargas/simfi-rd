# Run scripts/pipeline/01–05 in an isolated environment with cached RDS I/O so
# intermediate files are not required on disk unless requested.

#' @param paths From [get_dom_paths()].
#' @param escenarios One-row tibble from [scenario_inputs_to_escenarios()].
#' @param write_intermediate If TRUE, still writes RDS under `paths$fdbmod`.
#' @return List with `resultados_escenarios`, `dom_results`, `pipeline_env`.
dom_attach_pipeline_libs <- function() {
  suppressPackageStartupMessages({
    library(readr)
    library(readxl)
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(stringr)
    library(tibble)
    library(janitor)
    library(collapse)
    library(rlang)
    library(data.table)
    library(statar)
    library(labelled)
    library(scales)
    library(KernSmooth)
    library(gt)
  })
}

run_pipeline_cached <- function(
    paths,
    escenarios,
    write_intermediate = FALSE,
    extras = NULL
) {
  assert_required_packages()
  dom_attach_pipeline_libs()
  source(file.path(paths$root, "R", "param_csv.R"), local = FALSE)
  if (nrow(escenarios) != 1L) {
    stop("run_pipeline_cached: se requiere exactamente una fila en `escenarios`.")
  }

  e <- new.env(parent = globalenv())
  e$.dom_rds_cache <- new.env(parent = emptyenv())
  e$.dom_captured <- list()

  e$finput <- paths$finput
  e$fpresim <- paths$fpresim
  e$fdbmod <- paths$fdbmod
  e$fparam_csv <- paths$param_csv
  e$fgeodata <- paths$fgeodata
  e$fr_params <- paths$fr_params
  e$path_o <- paths$root
  e$escenarios <- escenarios
  e$parm_glob <- load_global_params(paths)

  if (length(extras)) {
    for (nm in names(extras)) {
      e[[nm]] <- extras[[nm]]
    }
  }

  e$saveRDS <- function(object, file, ...) {
    key <- basename(as.character(file))
    e$.dom_rds_cache[[key]] <- object
    if (grepl("^DOM_resumen", key)) {
      e$.dom_captured$resultados_escenarios <- object
    }
    if (grepl("^DOM_resultados", key)) {
      e$.dom_captured$dom_resultados <- object
    }
    if (grepl("^DOM_simulaciones", key)) {
      e$.dom_captured$dom_simulaciones <- object
    }
    if (isTRUE(write_intermediate)) {
      base::saveRDS(object, file, ...)
    }
    invisible(object)
  }

  e$readRDS <- function(file, ...) {
    key <- basename(as.character(file))
    if (!is.null(e$.dom_rds_cache[[key]])) {
      return(e$.dom_rds_cache[[key]])
    }
    base::readRDS(file, ...)
  }

  opts <- options(dom.skip_escenarios_reload = TRUE)

  on.exit(options(opts), add = TRUE)

  steps <- c(
    "01_DOM26_itbis.R",
    "02_DOM26_irenta.R",
    "03_DOM26_subs.R",
    "04_DOM26_compensacion.R",
    "05_DOM26_ConsolidaEscenarios.R"
  )

  for (step in steps) {
    src <- file.path(paths$pipeline_dir, step)
    if (!file.exists(src)) {
      stop("No existe el script del pipeline: ", src)
    }
    sys.source(src, envir = e, chdir = FALSE)
  }

  if (is.null(e$.dom_captured$resultados_escenarios)) {
    stop(
      "El pipeline no produjo `resultados_escenarios` ",
      "(revisar 05_DOM26_ConsolidaEscenarios.R / captura de saveRDS)."
    )
  }

  dom_results <- e$.dom_rds_cache[["DOM_simcompensacion.rds"]]
  if (is.null(dom_results)) {
    stop("No se capturó DOM_simcompensacion.rds en la caché del pipeline.")
  }

  list(
    resultados_escenarios = e$.dom_captured$resultados_escenarios,
    dom_results = dom_results,
    pipeline_env = e
  )
}
