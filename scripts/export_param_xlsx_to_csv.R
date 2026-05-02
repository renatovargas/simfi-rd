## Exporta todas las hojas de DOM_parametros_simulaciones.xlsx a CSV
## en data/params/ (UTF-8).
##
## Uso (desde la raíz del repo):
##   Rscript scripts/export_param_xlsx_to_csv.R [ruta/al/archivo.xlsx]
##
## Si no pasa ruta, busca el .xlsx en data/mod/.

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
})

root <- normalizePath(".", winslash = "/", mustWork = FALSE)
args <- commandArgs(trailingOnly = TRUE)
candidates <- c(
  args,
  file.path(root, "data", "mod", "DOM_parametros_simulaciones.xlsx")
)
wb <- candidates[nzchar(candidates) & file.exists(candidates)][1]
if (is.na(wb)) {
  stop("No se encontró DOM_parametros_simulaciones.xlsx; pase la ruta como argumento.")
}
out <- file.path(root, "data", "params")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
sh <- readxl::excel_sheets(wb)
for (s in sh) {
  d <- readxl::read_excel(wb, sheet = s, col_names = TRUE)
  readr::write_csv(d, file.path(out, paste0(s, ".csv")), na = "")
}
message("Exportadas ", length(sh), " tablas a ", out)
