# Almacén de componentes y escenarios para la pantalla "Revisar".
#
# Modelo de datos
# ----------------
# Una *biblioteca* (`libs`) es una lista con cuatro tipos de componente, cada uno
# una lista nombrada `nombre -> payload`:
#   libs$itbis[[nombre]] = list(marco, rate_grupo, rate_subclase, rate_variedad)
#   libs$isr[[nombre]]   = list(custom, nom_renta, lim_inf[6], lim_sup[6], tasa_pct[6])
#   libs$sub[[nombre]]   = list(custom, nom_subsidio, block[7], bsur[7], ..., costsur, otsur, ...)
#   libs$comp[[nombre]]  = list(enabled, sin_compensacion, grupo_com, metodo_com,
#                               valor_com, decil_com, decil_est, icv_com)
#
# Un *escenario* compone un componente de cada tipo por nombre (o "" = referencia):
#   list(name, itbis, isr, sub, comp, comparar)
#
# La persistencia para compartir es un Excel con una pestaña por tipo de componente
# más una pestaña `escenarios` (índice). El autoguardado local usa RDS.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# --- utilidades de payload <-> data.frame -----------------------------------

.num7 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  length(v) <- 7L
  v
}
.num6 <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  length(v) <- 6L
  v
}

# ITBIS: filas (componente, nivel, clave, tasa)
.itbis_to_df <- function(libs_itbis) {
  rows <- list()
  for (nm in names(libs_itbis)) {
    p <- libs_itbis[[nm]]
    for (lvl in c("grupo", "subclase", "variedad")) {
      lst <- p[[paste0("rate_", lvl)]] %||% list()
      for (k in names(lst)) {
        rows[[length(rows) + 1L]] <- data.frame(
          componente = nm, nivel = lvl, clave = k,
          tasa = suppressWarnings(as.numeric(lst[[k]])),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(componente = character(), nivel = character(),
                      clave = character(), tasa = numeric(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.itbis_from_df <- function(df) {
  out <- list()
  if (is.null(df) || !nrow(df)) return(out)
  for (nm in unique(df$componente)) {
    sub <- df[df$componente == nm, , drop = FALSE]
    p <- list(marco = "actual", rate_grupo = list(),
              rate_subclase = list(), rate_variedad = list())
    for (lvl in c("grupo", "subclase", "variedad")) {
      ss <- sub[sub$nivel == lvl, , drop = FALSE]
      if (nrow(ss)) {
        l <- as.list(suppressWarnings(as.numeric(ss$tasa)))
        names(l) <- as.character(ss$clave)
        p[[paste0("rate_", lvl)]] <- l
      }
    }
    out[[nm]] <- p
  }
  out
}

# ISR: filas (componente, custom, nom_renta, tramo, lim_inf, lim_sup, tasa_pct)
.isr_to_df <- function(libs_isr) {
  rows <- list()
  for (nm in names(libs_isr)) {
    p <- libs_isr[[nm]]
    li <- .num6(p$lim_inf); ls <- .num6(p$lim_sup); tp <- .num6(p$tasa_pct)
    rows[[length(rows) + 1L]] <- data.frame(
      componente = nm,
      custom = isTRUE(p$custom),
      nom_renta = as.character(p$nom_renta %||% nm),
      tramo = 1:6, lim_inf = li, lim_sup = ls, tasa_pct = tp,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(data.frame(componente = character(), custom = logical(),
                      nom_renta = character(), tramo = integer(),
                      lim_inf = numeric(), lim_sup = numeric(),
                      tasa_pct = numeric(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.isr_from_df <- function(df) {
  out <- list()
  if (is.null(df) || !nrow(df)) return(out)
  for (nm in unique(df$componente)) {
    sub <- df[df$componente == nm, , drop = FALSE]
    sub <- sub[order(suppressWarnings(as.integer(sub$tramo))), , drop = FALSE]
    out[[nm]] <- list(
      custom    = isTRUE(as.logical(sub$custom[1])),
      nom_renta = as.character(sub$nom_renta[1]),
      lim_inf   = .num6(sub$lim_inf),
      lim_sup   = .num6(sub$lim_sup),
      tasa_pct  = .num6(sub$tasa_pct)
    )
  }
  out
}

# Subsidio: filas (componente, custom, nom_subsidio, campo, indice, valor)
.sub_vec_fields <- c("block", "bsur", "bnorte", "beste", "tsur", "tnorte", "teste")
.sub_sca_fields <- c("costsur", "costnorte", "costeste", "otsur", "otnorte", "oteste")

.sub_to_df <- function(libs_sub) {
  rows <- list()
  add <- function(nm, custom, nom, campo, indice, valor) {
    rows[[length(rows) + 1L]] <<- data.frame(
      componente = nm, custom = isTRUE(custom),
      nom_subsidio = as.character(nom %||% ""),
      campo = campo, indice = indice,
      valor = suppressWarnings(as.numeric(valor)),
      stringsAsFactors = FALSE
    )
  }
  for (nm in names(libs_sub)) {
    p <- libs_sub[[nm]]
    nom <- p$nom_subsidio %||% nm
    for (f in .sub_vec_fields) {
      v <- .num7(p[[f]])
      for (i in 1:7) add(nm, p$custom, nom, f, i, v[i])
    }
    for (f in .sub_sca_fields) add(nm, p$custom, nom, f, NA_integer_, p[[f]])
  }
  if (!length(rows)) {
    return(data.frame(componente = character(), custom = logical(),
                      nom_subsidio = character(), campo = character(),
                      indice = integer(), valor = numeric(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.sub_from_df <- function(df) {
  out <- list()
  if (is.null(df) || !nrow(df)) return(out)
  for (nm in unique(df$componente)) {
    sub <- df[df$componente == nm, , drop = FALSE]
    p <- list(custom = isTRUE(as.logical(sub$custom[1])),
              nom_subsidio = as.character(sub$nom_subsidio[1]))
    for (f in .sub_vec_fields) {
      ss <- sub[sub$campo == f, , drop = FALSE]
      ss <- ss[order(suppressWarnings(as.integer(ss$indice))), , drop = FALSE]
      p[[f]] <- .num7(ss$valor)
    }
    for (f in .sub_sca_fields) {
      ss <- sub[sub$campo == f, , drop = FALSE]
      p[[f]] <- if (nrow(ss)) suppressWarnings(as.numeric(ss$valor[1])) else NA_real_
    }
    out[[nm]] <- p
  }
  out
}

# Compensación: una fila por componente (formato ancho)
.comp_to_df <- function(libs_comp) {
  rows <- list()
  for (nm in names(libs_comp)) {
    p <- libs_comp[[nm]]
    rows[[length(rows) + 1L]] <- data.frame(
      componente = nm,
      enabled = isTRUE(p$enabled),
      sin_compensacion = isTRUE(p$sin_compensacion),
      grupo_com  = suppressWarnings(as.numeric(p$grupo_com %||% NA)),
      metodo_com = suppressWarnings(as.numeric(p$metodo_com %||% NA)),
      valor_com  = suppressWarnings(as.numeric(p$valor_com %||% NA)),
      decil_com  = suppressWarnings(as.numeric(p$decil_com %||% NA)),
      decil_est  = suppressWarnings(as.numeric(p$decil_est %||% NA)),
      icv_com    = suppressWarnings(as.numeric(p$icv_com %||% NA)),
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(data.frame(componente = character(), enabled = logical(),
                      sin_compensacion = logical(), grupo_com = numeric(),
                      metodo_com = numeric(), valor_com = numeric(),
                      decil_com = numeric(), decil_est = numeric(),
                      icv_com = numeric(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.int_or_na <- function(x) {
  v <- suppressWarnings(as.integer(round(as.numeric(x))))
  if (length(v) != 1L) NA_integer_ else v
}

.comp_from_df <- function(df) {
  out <- list()
  if (is.null(df) || !nrow(df)) return(out)
  for (i in seq_len(nrow(df))) {
    nm <- as.character(df$componente[i])
    out[[nm]] <- list(
      enabled          = isTRUE(as.logical(df$enabled[i])),
      sin_compensacion = isTRUE(as.logical(df$sin_compensacion[i])),
      sim_comp_id      = 1L,
      grupo_com        = .int_or_na(df$grupo_com[i]),
      metodo_com       = .int_or_na(df$metodo_com[i]),
      valor_com        = suppressWarnings(as.numeric(df$valor_com[i])),
      decil_com        = .int_or_na(df$decil_com[i]),
      decil_est        = .int_or_na(df$decil_est[i]),
      icv_com          = suppressWarnings(as.numeric(df$icv_com[i]))
    )
  }
  out
}

# Escenarios: índice (escenario, itbis, isr, sub, comp, comparar)
.scen_to_df <- function(scenarios) {
  rows <- list()
  for (s in scenarios) {
    rows[[length(rows) + 1L]] <- data.frame(
      escenario = as.character(s$name %||% ""),
      itbis = as.character(s$itbis %||% ""),
      isr   = as.character(s$isr %||% ""),
      sub   = as.character(s$sub %||% ""),
      comp  = as.character(s$comp %||% ""),
      comparar = isTRUE(s$comparar),
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(data.frame(escenario = character(), itbis = character(),
                      isr = character(), sub = character(), comp = character(),
                      comparar = logical(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.scen_from_df <- function(df) {
  out <- list()
  if (is.null(df) || !nrow(df)) return(out)
  for (i in seq_len(nrow(df))) {
    out[[length(out) + 1L]] <- list(
      name     = as.character(df$escenario[i]),
      itbis    = as.character(df$itbis[i] %||% ""),
      isr      = as.character(df$isr[i] %||% ""),
      sub      = as.character(df$sub[i] %||% ""),
      comp     = as.character(df$comp[i] %||% ""),
      comparar = isTRUE(as.logical(df$comparar[i]))
    )
  }
  out
}

#' Guardar bibliotecas + escenarios en un Excel multipestaña.
scenario_store_save_xlsx <- function(path, libs, scenarios) {
  sheets <- list(
    itbis      = .itbis_to_df(libs$itbis %||% list()),
    isr        = .isr_to_df(libs$isr %||% list()),
    sub        = .sub_to_df(libs$sub %||% list()),
    comp       = .comp_to_df(libs$comp %||% list()),
    escenarios = .scen_to_df(scenarios %||% list())
  )
  openxlsx::write.xlsx(sheets, file = path, overwrite = TRUE)
  invisible(path)
}

#' Leer bibliotecas + escenarios desde un Excel multipestaña.
scenario_store_read_xlsx <- function(path) {
  rd <- function(sheet) {
    tryCatch(
      openxlsx::read.xlsx(path, sheet = sheet),
      error = function(e) NULL
    )
  }
  libs <- list(
    itbis = .itbis_from_df(rd("itbis")),
    isr   = .isr_from_df(rd("isr")),
    sub   = .sub_from_df(rd("sub")),
    comp  = .comp_from_df(rd("comp"))
  )
  scenarios <- .scen_from_df(rd("escenarios"))
  list(libs = libs, scenarios = scenarios)
}

# --- autoguardado local (RDS) -----------------------------------------------

scenario_autosave_path <- function(root = ".") {
  dir <- file.path(root, "app_state")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(dir, "autosave.rds")
}

scenario_autosave_write <- function(root, libs, scenarios) {
  tryCatch({
    saveRDS(list(libs = libs, scenarios = scenarios, ts = Sys.time()),
            scenario_autosave_path(root))
    TRUE
  }, error = function(e) FALSE)
}

scenario_autosave_read <- function(root) {
  p <- scenario_autosave_path(root)
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

scenario_autosave_clear <- function(root) {
  p <- scenario_autosave_path(root)
  if (file.exists(p)) unlink(p)
  invisible(TRUE)
}
