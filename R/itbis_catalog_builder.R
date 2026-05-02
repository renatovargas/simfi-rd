# Construye la tabla itbis_sim (detalle_itbis) desde el catálogo y listas de tasas
# del constructor de escenarios (misma lógica que app-legacy.R).

itbis_row_key <- function(row) {
  paste(
    as.character(row$COD_GRUPO),
    as.character(row$COD_SUBGRUPO),
    as.character(row$COD_CLASE),
    as.character(row$COD_SUBCLASE),
    as.character(row$COD_ARTICULO),
    as.character(row$ID_VARIEDAD),
    sep = "|"
  )
}

itbis_subclase_key <- function(row) {
  paste(
    as.character(row$COD_GRUPO),
    as.character(row$COD_SUBGRUPO),
    as.character(row$COD_CLASE),
    as.character(row$COD_SUBCLASE),
    sep = "|"
  )
}

load_itbis_catalog <- function(root = ".") {
  path <- file.path(root, "data", "itbis_base_catalog.csv")
  if (!file.exists(path)) {
    stop("No se encontró el catálogo de productos ITBIS.")
  }
  readr::read_csv(path, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      tasa = suppressWarnings(as.numeric(.data$tasa)),
      gravado = suppressWarnings(as.integer(.data$gravado))
    )
}

tasa_efectiva_desde_listas <- function(row, rate_variedad, rate_subclase, rate_grupo, marco) {
  rk <- itbis_row_key(row)
  sk <- itbis_subclase_key(row)
  gk <- as.character(row$COD_GRUPO)
  if (!is.null(rate_variedad[[rk]])) {
    return(as.numeric(rate_variedad[[rk]]))
  }
  if (!is.null(rate_subclase[[sk]])) {
    return(as.numeric(rate_subclase[[sk]]))
  }
  if (!is.null(rate_grupo[[gk]])) {
    return(as.numeric(rate_grupo[[gk]]))
  }
  if (identical(marco, "blank")) {
    return(0)
  }
  v <- suppressWarnings(as.numeric(row$tasa))
  if (length(v) != 1 || is.na(v)) 0 else v
}

#' @param detalle_plantilla Tibble desde `itbis_sim.csv` (estructura de columnas).
#' @param rate_* Listas nombradas como en Shiny (`stats::setNames` desde inputs).
#' @param marco `"actual"` o `"blank"`.
build_detalle_itbis_override <- function(
    catalog,
    detalle_plantilla,
    rate_grupo = list(),
    rate_subclase = list(),
    rate_variedad = list(),
    marco = "actual"
) {
  n <- nrow(catalog)
  eff <- vapply(
    seq_len(n),
    function(i) {
      tasa_efectiva_desde_listas(
        catalog[i, , drop = FALSE],
        rate_variedad,
        rate_subclase,
        rate_grupo,
        marco
      )
    },
    numeric(1)
  )
  cod_art <- as.character(catalog$COD_ARTICULO)
  variedad_lab <- as.character(catalog$DES_VARIEDAD)
  idv <- suppressWarnings(as.integer(catalog$ID_VARIEDAD))
  out <- tibble::tibble(
    cod_variedad = idv,
    cod_articulo = suppressWarnings(as.integer(cod_art)),
    variedad = variedad_lab,
    itbis_alt1 = eff
  )
  alt_cols <- grep("^itbis_alt[0-9]+$", names(detalle_plantilla), value = TRUE)
  alt_cols <- setdiff(alt_cols, "itbis_alt1")
  tpl_cv <- suppressWarnings(as.integer(detalle_plantilla$cod_variedad))
  tpl_ca <- suppressWarnings(as.integer(detalle_plantilla$cod_articulo))
  key_tpl <- paste(tpl_cv, tpl_ca, sep = "\1")
  key_out <- paste(out$cod_variedad, out$cod_articulo, sep = "\1")
  ix <- match(key_out, key_tpl)
  for (cn in alt_cols) {
    if (!cn %in% names(detalle_plantilla)) {
      out[[cn]] <- 0
      next
    }
    raw <- suppressWarnings(as.numeric(detalle_plantilla[[cn]]))
    vals <- raw[ix]
    vals[is.na(vals)] <- 0
    out[[cn]] <- vals
  }
  cols_ord <- intersect(names(detalle_plantilla), names(out))
  out[, cols_ord, drop = FALSE]
}

default_sim_itbis_policy_row <- function() {
  tibble::tibble(
    sim_itbis = 1L,
    activo = 1L,
    tasa_itbis = 18,
    exentos_gravados = 0L,
    uniforma_tasa = 0L,
    nom_itbis = "Reforma (constructor)",
    base_formal = 4L
  )
}

build_sim_renta_override <- function(
    fparam_csv,
    tramos,
    bases,
    tasas,
    nom_renta,
    sim_inc = 1L
) {
  full <- read_param_csv("sim_renta", fparam_csv)
  tpl <- full %>% dplyr::filter(.data$sim_inc == !!sim_inc) %>% dplyr::slice(1)
  if (nrow(tpl) == 0L) {
    tpl <- full %>% dplyr::filter(.data$sim_inc == 0L) %>% dplyr::slice(1)
  }
  tpl <- tpl[1, , drop = FALSE]
  tpl$sim_inc <- sim_inc
  tpl$activo <- 1L
  tpl$nom_renta <- as.character(nom_renta)
  for (i in seq_len(6L)) {
    tc <- paste0("tramo", i)
    bc <- paste0("base", i)
    sc <- paste0("tasa", i)
    tv <- tramos[[i]]
    bv <- bases[[i]]
    sv <- tasas[[i]]
    if (tc %in% names(tpl) && length(tv) == 1L && is.finite(suppressWarnings(as.numeric(tv)))) {
      tpl[[tc]] <- as.numeric(tv)
    }
    if (bc %in% names(tpl) && length(bv) == 1L && is.finite(suppressWarnings(as.numeric(bv)))) {
      tpl[[bc]] <- as.numeric(bv)
    }
    if (sc %in% names(tpl) && length(sv) == 1L && is.finite(suppressWarnings(as.numeric(sv)))) {
      tpl[[sc]] <- as.numeric(sv)
    }
  }
  tpl
}
