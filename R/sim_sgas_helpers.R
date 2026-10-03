# Helpers: subsidio a combustibles (sim_sgas) — fila desde param_csv y parche desde la UI.
#
# La persona usuaria edita un solo valor por combustible: el subsidio en RD$ por
# galón durante los meses en que aplica. Precios, estacionalidad y calendario
# (m1..m12) se mantienen como en la fila activa de `sim_sgas.csv`.

#' Combustibles editables, en el orden en que se muestran en pantalla.
SGAS_COMBUSTIBLES <- c(
  gasolina_premium = "Gasolina premium",
  gasolina_regular = "Gasolina regular",
  gasoil_premium   = "Gasoil premium",
  gasoil_regular   = "Gasoil regular",
  glp              = "GLP"
)

#' @param param_csv_dir Directorio `paths$param_csv`.
#' @param sim_sgas_id Índice `sim_sgas` (0 = sin subsidio).
#' @return Una fila (`tibble`) con columnas como en `sim_sgas.csv`.
read_sim_sgas_template_row <- function(param_csv_dir, sim_sgas_id = 1L) {
  df <- read_param_csv("sim_sgas", param_csv_dir) %>%
    dplyr::filter(.data$sim_sgas == as.integer(sim_sgas_id))
  if (nrow(df) != 1L) {
    stop("sim_sgas: se requiere exactamente una fila para sim_sgas=", sim_sgas_id)
  }
  df
}

#' Meses (1..12) en que aplica el subsidio según la fila plantilla.
sgas_meses_activos <- function(template_row) {
  m <- vapply(1:12, function(j) {
    as.numeric(template_row[[paste0("m", j)]] %||% 0)
  }, numeric(1))
  which(!is.na(m) & m > 0)
}

#' Subsidio por galón de la plantilla (primer mes activo), por combustible.
sgas_default_values <- function(template_row) {
  meses <- sgas_meses_activos(template_row)
  vapply(names(SGAS_COMBUSTIBLES), function(cb) {
    if (!length(meses)) return(0)
    v <- as.numeric(template_row[[paste0(cb, "_sub_", meses[1])]])
    if (length(v) && is.finite(v)) v else 0
  }, numeric(1))
}

#' Precio de referencia por galón (promedio de los meses activos), por combustible.
sgas_reference_prices <- function(template_row) {
  meses <- sgas_meses_activos(template_row)
  if (!length(meses)) meses <- 1:12
  vapply(names(SGAS_COMBUSTIBLES), function(cb) {
    p <- vapply(meses, function(j) {
      as.numeric(template_row[[paste0(cb, "_precio_", j)]] %||% NA_real_)
    }, numeric(1))
    mean(p, na.rm = TRUE)
  }, numeric(1))
}

#' Fila `sim_sgas` con el subsidio por galón editado en la UI.
#'
#' @param template_row Fila activa de [read_sim_sgas_template_row()].
#' @param valores Vector con nombres de `SGAS_COMBUSTIBLES` (RD$/galón).
#' @param sim_sgas_id Índice que recibirá la fila (la app usa 1).
build_sim_sgas_override <- function(template_row, valores, sim_sgas_id = 1L) {
  out <- template_row
  out$sim_sgas <- as.integer(sim_sgas_id)
  for (cb in names(SGAS_COMBUSTIBLES)) {
    v <- suppressWarnings(as.numeric(valores[[cb]] %||% 0))
    if (!length(v) || !is.finite(v)) v <- 0
    for (j in 1:12) {
      nm <- paste0(cb, "_sub_", j)
      if (nm %in% names(out)) {
        mj <- as.numeric(out[[paste0("m", j)]] %||% 0)
        out[[nm]] <- v * (if (is.finite(mj)) mj else 0)
      }
    }
  }
  out
}

#' Fila neutra: copia de la fila base (sin subsidio) con índice `sim_sgas_id`.
build_sim_sgas_neutral <- function(param_csv_dir, sim_sgas_id = 1L) {
  out <- read_sim_sgas_template_row(param_csv_dir, 0L)
  out$sim_sgas <- as.integer(sim_sgas_id)
  out
}
