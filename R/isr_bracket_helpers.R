# Convierte límites y tasas (% en 0–100) al formato tramo/base/tasa del pipeline 02.

#' @param lim_inf,lim_sup,tasa_pct Vectores de igual longitud; `lim_inf[1]` debe ser 0;
#'   `tasa_pct` en puntos porcentuales (p. ej. 15 para 15 %).
#' @param max_slots Número de tramos en `sim_renta` (6).
#' @return Lista `tramos`, `bases`, `tasas` (tasas en 0–1) con `NA` en slots no usados.
pipeline_isr_from_brackets <- function(
    lim_inf,
    lim_sup,
    tasa_pct,
    max_slots = 6L
) {
  n <- length(lim_inf)
  if (n < 1L || length(lim_sup) != n || length(tasa_pct) != n) {
    stop("pipeline_isr_from_brackets: longitudes incompatibles.")
  }
  lim_inf <- suppressWarnings(as.numeric(lim_inf))
  lim_sup <- suppressWarnings(as.numeric(lim_sup))
  tasa_pct <- suppressWarnings(as.numeric(tasa_pct))

  keep <- !is.na(lim_inf) & !is.na(tasa_pct)
  if (!keep[1L] || !is.finite(lim_inf[1L]) || lim_inf[1L] != 0) {
    stop("pipeline_isr_from_brackets: el primer límite inferior debe ser 0.")
  }
  m <- 0L
  for (i in seq_len(n)) {
    if (!keep[i]) break
    m <- m + 1L
  }
  if (m < 1L) {
    stop("pipeline_isr_from_brackets: no hay tramos válidos.")
  }

  li <- lim_inf[seq_len(m)]
  tp <- tasa_pct[seq_len(m)]
  ls <- lim_sup[seq_len(m)]

  tramos <- rep(NA_real_, max_slots)
  bases <- rep(NA_real_, max_slots)
  tasas <- rep(NA_real_, max_slots)

  tramos[1L] <- 0
  bases[1L] <- 0
  tasas[1L] <- if (is.finite(tp[1L])) tp[1L] / 100 else 0

  cum_tax <- 0
  for (j in 2:m) {
    width <- li[j] - li[j - 1L]
    if (!is.finite(width) || width < 0) {
      stop("pipeline_isr_from_brackets: límites inferiores no crecientes.")
    }
    rate_prev <- tp[j - 1L] / 100
    cum_tax <- cum_tax + width * rate_prev
    tramos[j] <- li[j]
    bases[j] <- cum_tax
    tasas[j] <- tp[j] / 100
  }

  list(tramos = as.list(tramos), bases = as.list(bases), tasas = as.list(tasas))
}
