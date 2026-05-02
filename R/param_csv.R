# Tablas de parámetros en CSV (reemplazo de DOM_parametros_simulaciones.xlsx).

#' @param stem Nombre del archivo sin extensión (p. ej. `"global_params"`).
#' @param param_csv_dir Directorio con barra final (`paths$param_csv`).
read_param_csv <- function(stem, param_csv_dir) {
  fp <- file.path(param_csv_dir, paste0(stem, ".csv"))
  if (!file.exists(fp)) {
    stop("No existe tabla de parámetros: ", fp)
  }
  df <- readr::read_csv(
    fp,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
  janitor::clean_names(df)
}
