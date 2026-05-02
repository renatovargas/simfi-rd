# Regenera data/itbis_base_catalog.csv con códigos rellenados a la izquierda
# (texto) para orden estable. Ejecutar desde la raíz del repo:
#   Rscript scripts/build_itbis_base_catalog.R

root <- Sys.getenv("DOM_TAX_ROOT", unset = ".")
path <- file.path(root, "data", "itbis_base_catalog.csv")

pad_num_field <- function(x, w) {
  x <- trimws(as.character(x))
  empty <- is.na(x) | !nzchar(x)
  out <- x
  is_digits <- !empty & grepl("^[0-9]+$", x)
  if (any(is_digits)) {
    out[is_digits] <- sprintf(paste0("%0", w, "d"), as.integer(x[is_digits]))
  }
  out[empty] <- NA_character_
  out
}

d <- utils::read.csv(
  path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  check.names = FALSE
)

d$COD_GRUPO <- pad_num_field(d$COD_GRUPO, 2L)
d$COD_SUBGRUPO <- pad_num_field(d$COD_SUBGRUPO, 3L)
d$COD_CLASE <- pad_num_field(d$COD_CLASE, 4L)
d$COD_SUBCLASE <- pad_num_field(d$COD_SUBCLASE, 5L)
d$COD_ARTICULO <- pad_num_field(d$COD_ARTICULO, 6L)
d$ID_VARIEDAD <- pad_num_field(d$ID_VARIEDAD, 4L)

utils::write.csv(
  d,
  path,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  quote = TRUE
)

message("Escrito: ", normalizePath(path))
