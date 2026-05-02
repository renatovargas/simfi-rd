# Paths and package checks for the DOM microsimulation engine (local Shiny).
# Parámetros CSV: data/params/
# Layout del repo:
#   - data/params    — parámetros CSV + dom_params.R
#   - data/presim    — microdatos previos a la simulación (RDS)
#   - data/mod       — salidas opcionales del pipeline (DOM_insumos.rds, etc.)
#   - data/geodata   — shapefiles / capas

trail <- function(...) paste0(file.path(...), .Platform$file.sep)

#' @param root Repository or project root (working directory containing `data/`).
#' @return Named list of directory and file paths (directorios con barra final).
get_dom_paths <- function(root = ".") {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  fpresim   <- trail(root, "data", "presim")
  fdbmod    <- trail(root, "data", "mod")
  fr_params <- trail(root, "data", "params")
  fgeodata  <- trail(root, "data", "geodata")

  # Los scripts históricos usan `finput` en paste0(finput, "DOMCEQ_DIncome.RDS").
  # En este repo esos insumos viven en data/presim/.
  finput <- fpresim

  param_csv <- trail(root, "data", "params")
  if (!dir.exists(sub("/$", "", param_csv))) {
    stop(
      "No existe el directorio de parámetros CSV: ",
      sub("/$", "", param_csv),
      "\nEjecute: Rscript scripts/export_param_xlsx_to_csv.R ",
      "(si aún tiene el .xlsx) o restaure data/params/."
    )
  }

  list(
    root             = root,
    finput           = finput,
    fpresim          = fpresim,
    fdbmod           = fdbmod,
    fr_params        = fr_params,
    fgeodata         = fgeodata,
    param_csv        = param_csv,
    pipeline_dir     = file.path(root, "scripts", "pipeline"),
    insumos_rds      = file.path(fdbmod, "DOM_insumos.rds"),
    diccionario_xlsx = file.path(fdbmod, "diccionario.xlsx")
  )
}

REQUIRED_DOM_PACKAGES <- c(
  "readr", "readxl", "dplyr", "tidyr", "tibble", "stringr", "purrr",
  "janitor", "openxlsx", "data.table", "scales", "labelled",
  "KernSmooth", "collapse", "rlang", "ggplot2",
  "statar", "sf", "Hmisc", "gt"
)

assert_required_packages <- function(extra = character()) {
  req     <- unique(c(REQUIRED_DOM_PACKAGES, extra))
  missing <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Faltan paquetes R: ", paste(missing, collapse = ", "),
      ". Instálelos antes de ejecutar la simulación."
    )
  }
  invisible(TRUE)
}
