## Exporta hojas de DOM_parametros_simulaciones.xlsx a CSV en data/params/
## (UTF-8).
##
## Uso (desde la raíz del repo):
##   Rscript scripts/export_param_xlsx_to_csv.R [ruta/al/archivo.xlsx] [--sheets hoja1,hoja2]
##
## Si no pasa ruta, busca el .xlsx en data/mod/.
##
## Sin --sheets exporta todas las hojas y sobrescribe los CSV existentes, incluidos
## los que la app ajustó a mano (sim_renta, sim_comp, escenarios, global_params).
## Para actualizar solo una línea base, por ejemplo la del subsidio a
## combustibles, use:
##   Rscript scripts/export_param_xlsx_to_csv.R ruta.xlsx --sheets sim_sgas

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
})

root <- normalizePath(".", winslash = "/", mustWork = FALSE)
args <- commandArgs(trailingOnly = TRUE)

sheets_sel <- NULL
i_sh <- which(args == "--sheets")
if (length(i_sh)) {
  if (i_sh[1] == length(args)) stop("--sheets requiere una lista de hojas.")
  sheets_sel <- trimws(strsplit(args[i_sh[1] + 1L], ",", fixed = TRUE)[[1]])
  args <- args[-c(i_sh[1], i_sh[1] + 1L)]
}

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
if (!is.null(sheets_sel)) {
  faltan <- setdiff(sheets_sel, sh)
  if (length(faltan)) {
    stop("Hojas inexistentes en ", basename(wb), ": ", paste(faltan, collapse = ", "))
  }
  sh <- sheets_sel
}
for (s in sh) {
  d <- readxl::read_excel(wb, sheet = s, col_names = TRUE)
  celdas <- as.matrix(d)
  d <- d[rowSums(!is.na(celdas) & nzchar(trimws(celdas))) > 0L, , drop = FALSE]
  # Tablas por escenario (primera columna con el nombre de la hoja): las filas
  # sin identificador son notas auxiliares debajo de la tabla.
  if (ncol(d) && identical(names(d)[1], s)) {
    d <- d[!is.na(d[[1]]), , drop = FALSE]
  }
  readr::write_csv(d, file.path(out, paste0(s, ".csv")), na = "")
}
message("Exportadas ", length(sh), " tablas a ", out, ": ", paste(sh, collapse = ", "))
