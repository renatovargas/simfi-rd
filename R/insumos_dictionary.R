# Analyst-facing catalog: DOM_insumos.rds (what to emphasize) and
# diccionario.xlsx (short definitions).

#' @param paths Output of [get_dom_paths()].
#' @return List with `template` (RDS list or NULL) and `diccionario` (tibble or NULL).
load_insumos_catalog <- function(paths) {
  template <- NULL
  if (file.exists(paths$insumos_rds)) {
    template <- readRDS(paths$insumos_rds)
  }
  diccionario <- NULL
  if (file.exists(paths$diccionario_xlsx)) {
    diccionario <- readxl::read_excel(paths$diccionario_xlsx) %>%
      janitor::clean_names()
  }
  list(template = template, diccionario = diccionario)
}

infer_escenario_num <- function(escenario) {
  kk <- colnames(escenario$kakwani)
  hit <- grep("^kakwani_Esc[0-9]+$", kk, value = TRUE)
  if (length(hit)) {
    return(as.integer(sub("^kakwani_Esc([0-9]+)$", "\\1", hit[1])))
  }
  1L
}

extract_incidence_col <- function(escenario, tabla_nm, colnm, mult = -1) {
  if (is.null(colnm) || !nzchar(colnm)) {
    return(NULL)
  }
  tab <- escenario[[tabla_nm]]
  if (is.null(tab)) {
    return(NULL)
  }
  mat <- as.data.frame(tab)
  if (!colnm %in% names(mat)) {
    return(NULL)
  }
  nr <- nrow(mat) - 1L
  if (nr < 1L) {
    return(NULL)
  }
  as.matrix(mat[seq_len(nr), colnm, drop = FALSE]) * mult
}

incidence_block_for_col <- function(escenario, colnm, mult = -1) {
  tabs <- c("decinc", "estrinc", "urbinc", "sexinc", "catinc", "depinc")
  stats::setNames(
    lapply(tabs, function(t) extract_incidence_col(escenario, t, colnm, mult)),
    tabs
  )
}

concentracion_two_col <- function(escenario, col_a, col_b) {
  tabs <- c("deccon", "estrcon", "urbcon", "sexcon", "catcon", "depcon")
  stats::setNames(
    lapply(tabs, function(t) {
      df <- as.data.frame(escenario[[t]])
      if (is.null(df) || nrow(df) < 2L) {
        return(NULL)
      }
      if (!all(c(col_a, col_b) %in% names(df))) {
        return(NULL)
      }
      nr <- nrow(df) - 1L
      out <- cbind(
        as.matrix(df[seq_len(nr), col_a, drop = FALSE]),
        as.matrix(df[seq_len(nr), col_b, drop = FALSE])
      )
      colnames(out) <- c("Base", "Posreforma")
      out
    }),
    tabs
  )
}

concentracion_comp_single <- function(escenario, k) {
  col <- paste0("comp_", k, "_pc")
  tabs <- c("deccon", "estrcon", "urbcon", "sexcon", "catcon", "depcon")
  stats::setNames(
    lapply(tabs, function(t) {
      df <- as.data.frame(escenario[[t]])
      if (is.null(df) || nrow(df) < 2L) {
        return(NULL)
      }
      if (!col %in% names(df)) {
        return(NULL)
      }
      nr <- nrow(df) - 1L
      as.matrix(df[seq_len(nr), col, drop = FALSE])
    }),
    tabs
  )
}

#' @param escenario Resumen de un escenario (`resultados_escenarios$escenario_k`).
#' @param escenario_num Índice del escenario (p. ej. columna `yd_k`, `nitxk_pc`).
#' @return Lista alineada con `DOM_insumos.rds`: `resumen`, `incidencia`, `concentracion`.
build_insumos_live <- function(escenario, escenario_num = NULL) {
  k <- escenario_num %||% infer_escenario_num(escenario)
  nitx_col <- paste0("nitx", k, "_pc")
  col_isr <- paste0("ddtx_isr", k, "_pc")
  col_itb <- paste0("ditx_itb", k, "_pc")
  col_sub <- paste0("dsub_ele", k, "_pc")
  decinc_nms <- names(as.data.frame(escenario$decinc))
  col_comp <- grep(paste0("^dcomp", k, "_pc$"), decinc_nms, value = TRUE)[1]

  inc_efecto <- incidence_block_for_col(escenario, nitx_col, -1)
  inc_isr <- incidence_block_for_col(escenario, col_isr, -1)
  inc_itb <- incidence_block_for_col(escenario, col_itb, -1)
  inc_sub <- incidence_block_for_col(escenario, col_sub, -1)
  inc_comp <- incidence_block_for_col(escenario, col_comp, -1)

  conc_isr <- concentracion_two_col(escenario, "dtx_isr0_pc", paste0("dtx_isr", k, "_pc"))
  conc_itb <- concentracion_two_col(escenario, "itx_itb0_pc", paste0("itx_itb", k, "_pc"))
  conc_sub <- concentracion_two_col(escenario, "sub_ele0_pc", paste0("sub_ele", k, "_pc"))
  conc_comp <- concentracion_comp_single(escenario, k)

  list(
    resumen = list(
      pobreza = as.matrix(escenario$pov_gral),
      nuevos_pobres = NULL,
      brecha_pobreza = NULL,
      desigualdad = as.matrix(escenario$ineq),
      progresividad = as.matrix(escenario$kakwani)
    ),
    incidencia = list(
      efecto_neto = inc_efecto,
      isr = inc_isr,
      itbis = inc_itb,
      subsidios = inc_sub,
      compensacion = inc_comp
    ),
    concentracion = list(
      efecto_neto = NULL,
      isr = conc_isr,
      itbis = conc_itb,
      subsidios = conc_sub,
      compensacion = conc_comp
    )
  )
}

copy_matrix_dimnames_if_missing <- function(x, ref) {
  if (is.null(x) || !is.matrix(x)) {
    return(x)
  }
  if (!is.null(ref) && is.matrix(ref) && identical(dim(x), dim(ref))) {
    if (is.null(rownames(x)) && !is.null(rownames(ref))) {
      rownames(x) <- rownames(ref)
    }
    if (is.null(colnames(x)) && !is.null(colnames(ref))) {
      colnames(x) <- colnames(ref)
    }
  }
  x
}

enrich_nested_matrices <- function(live_sec, template_sec) {
  if (is.null(live_sec) || is.null(template_sec)) {
    return(live_sec)
  }
  for (nm in names(live_sec)) {
    if (!nm %in% names(template_sec)) {
      next
    }
    lv <- live_sec[[nm]]
    tp <- template_sec[[nm]]
    if (is.matrix(lv) && is.matrix(tp)) {
      live_sec[[nm]] <- copy_matrix_dimnames_if_missing(lv, tp)
    } else if (is.list(lv) && is.list(tp)) {
      live_sec[[nm]] <- enrich_nested_matrices(lv, tp)
    }
  }
  live_sec
}

#' Copy filas/columnas nombradas desde `DOM_insumos.rds` cuando el resumen en vivo
#' viene sin `dimnames` (misma dimensión).
#'
#' @param live Salida de [build_insumos_live()].
#' @param template Lista leída de `DOM_insumos.rds`, o `NULL`.
#' @return `live` enriquecido.
enrich_insumos_live <- function(live, template) {
  if (is.null(template) || is.null(live)) {
    return(live)
  }
  if (!is.null(live$resumen) && !is.null(template$resumen)) {
    live$resumen <- enrich_nested_matrices(live$resumen, template$resumen)
  }
  if (!is.null(live$incidencia) && !is.null(template$incidencia)) {
    live$incidencia <- enrich_nested_matrices(live$incidencia, template$incidencia)
  }
  if (!is.null(live$concentracion) && !is.null(template$concentracion)) {
    live$concentracion <- enrich_nested_matrices(live$concentracion, template$concentracion)
  }
  live
}
