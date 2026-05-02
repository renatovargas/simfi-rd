# Helpers: subsidio eléctrico (sim_sub) — fila desde param_csv y parche desde la UI.

#' @param param_csv_dir Directorio `paths$param_csv`.
#' @param sim_sub_id Índice `sim_sub` (fila activa).
#' @return Una fila (`tibble`) con columnas como en `sim_sub.csv` (nombres limpios).
read_sim_sub_template_row <- function(param_csv_dir, sim_sub_id) {
  df <- read_param_csv("sim_sub", param_csv_dir) %>%
    dplyr::filter(.data$activo == 1L, .data$sim_sub == as.integer(sim_sub_id))
  if (nrow(df) != 1L) {
    stop(
      "sim_sub: se requiere exactamente una fila activa para sim_sub=",
      sim_sub_id
    )
  }
  df
}

#' Sustituye en la fila plantilla los campos editados en la lista `sub_list`.
#'
#' `sub_list` debe incluir vectores numéricos de longitud 7 (`block`, `bsur`, …)
#' y opcionalmente `nom_subsidio`, `costsur`, `costnorte`, `costeste`.
#' @param template_row Una fila como [read_sim_sub_template_row()].
#' @param sub_list Lista desde `collect_scenario_inputs()$sub_ele`.
#' @return `tibble` de una fila lista para [sim_sub_row_override].
patch_sim_sub_row_from_ui <- function(template_row, sub_list) {
  out <- template_row
  J <- 7L
  blk <- sub_list$block
  if (length(blk)) {
    for (j in seq_len(min(J, length(blk)))) {
      nm <- paste0("block", j)
      if (nm %in% names(out)) {
        v <- blk[j]
        out[[nm]] <- if (length(v) && is.finite(v)) as.numeric(v) else NA_real_
      }
    }
  }
  for (pref in c(
    "bsur",
    "bnorte",
    "beste",
    "tsur",
    "tnorte",
    "teste"
  )) {
    vec <- sub_list[[pref]]
    if (length(vec)) {
      for (j in seq_len(min(J, length(vec)))) {
        nm <- paste0(pref, j)
        if (nm %in% names(out)) {
          v <- vec[j]
          out[[nm]] <- if (length(v) && is.finite(v)) as.numeric(v) else NA_real_
        }
      }
    }
  }
  nm <- sub_list$nom_subsidio
  if (length(nm) && nzchar(trimws(paste0(nm[[1L]], collapse = "")))) {
    out$nom_subsidio <- as.character(nm[[1L]])
  }
  for (cx in c("costsur", "costnorte", "costeste")) {
    if (!is.null(sub_list[[cx]]) && cx %in% names(out)) {
      v <- as.numeric(sub_list[[cx]])
      if (length(v) && is.finite(v[[1L]])) {
        out[[cx]] <- v[[1L]]
      }
    }
  }
  out
}
