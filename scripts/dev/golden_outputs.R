## Salidas congeladas del pipeline para detectar cambios no intencionales.
##
## Uso (desde la raíz del repo):
##   Rscript scripts/dev/golden_outputs.R freeze   # guarda tests/golden/*.rds
##   Rscript scripts/dev/golden_outputs.R check    # compara contra lo guardado
##
## Cada escenario se arma con la misma estructura que compose_scenario_inputs()
## en app.R. Con el subsidio a combustibles desactivado, `check` debe pasar.

suppressPackageStartupMessages({
  library(dplyr)
})

root <- normalizePath(".", winslash = "/", mustWork = FALSE)
source(file.path(root, "R", "00_autoload.R"))
autoload_dom_r(root)
paths <- get_dom_paths(root)

mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "check"
golden_dir <- file.path(root, "tests", "golden")
dir.create(golden_dir, recursive = TRUE, showWarnings = FALSE)

itbis_vacio <- list(marco = "actual", rate_grupo = list(),
                    rate_subclase = list(), rate_variedad = list())
sin_comp <- list(enabled = FALSE, sin_compensacion = TRUE)

base_inputs <- function(nm) {
  list(label = nm, des_corto = nm, des_escenario = nm, escenario = 1L,
       sim_itbis = 1L, custom_isr = FALSE, sim_renta = 0L, sim_sub = 1L,
       sim_com = 0L, itbis = itbis_vacio, isr = NULL, comp = sin_comp,
       sub_ele = list(custom = FALSE))
}

con_comp <- function(inp, grupo, metodo, valor = 0, decil_com = NA,
                     icv_com = NA, decil_est = NA) {
  inp$sim_com <- 1L
  inp$comp <- list(enabled = TRUE, sin_compensacion = FALSE, sim_comp_id = 1L,
                   grupo_com = grupo, metodo_com = metodo, valor_com = valor,
                   decil_com = decil_com, icv_com = icv_com,
                   decil_est = decil_est)
  inp
}

escenarios_golden <- function() {
  out <- list()
  out$base <- base_inputs("base")

  it <- base_inputs("itbis")
  it$itbis$rate_grupo <- list("1" = 16, "7" = 16)
  out$itbis <- it

  isr <- base_inputs("isr")
  pv <- pipeline_isr_from_brackets(c(0, 478800, 624329, 867123, 4800000),
                                   c(478800, 624329, 867123, 4800000, NA),
                                   c(0, 15, 20, 25, 27))
  isr$custom_isr <- TRUE
  isr$sim_renta <- 1L
  isr$isr <- list(custom = TRUE, tramos = pv$tramos, bases = pv$bases,
                  tasas = pv$tasas, nom_renta = "ISR prueba")
  out$isr <- isr

  sub <- base_inputs("subsidio")
  r1 <- read_sim_sub_template_row(paths$param_csv, 1L)
  g <- function(pref) vapply(1:7, function(j) {
    v <- r1[[paste0(pref, j)]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }, numeric(1))
  sub$sub_ele <- list(custom = TRUE, block = g("block"), bsur = g("bsur"),
                      bnorte = g("bnorte"), beste = g("beste"),
                      tsur = g("tsur"), tnorte = g("tnorte"),
                      teste = g("teste"), nom_subsidio = "Subsidio prueba",
                      costsur = r1$costsur, costnorte = r1$costnorte,
                      costeste = r1$costeste, otsur = r1$otsur,
                      otnorte = r1$otnorte, oteste = r1$oteste)
  out$subsidio <- sub

  # grupo_com: 1 = Supérate, 2 = decil, 3 = ICV. metodo_com: 1 = pérdida
  # promedio hasta decil_est, 2 = monto fijo mensual.
  out$comp_decil <- con_comp(it, grupo = 2L, metodo = 2L, valor = 500,
                             decil_com = 3L)
  out$comp_icv <- con_comp(it, grupo = 3L, metodo = 1L, icv_com = 2,
                           decil_est = 4L)
  out$comp_superate <- con_comp(it, grupo = 1L, metodo = 2L, valor = 800)
  out
}

correr <- function(inp) {
  res <- run_dom_scenario(inp, paths = paths, return = "both")
  list(resultados = res$resultados_escenarios,
       micro = res$dom_results)
}

comparar <- function(old, new, path = "") {
  if (is.data.frame(old) || is.matrix(old)) {
    # Matrices con filas nombradas (p. ej. Kakwani): se aceptan filas nuevas.
    rn_old <- rownames(old)
    if (is.matrix(old) && !is.null(rn_old) && !is.null(rownames(new)) &&
        all(rn_old %in% rownames(new))) {
      new <- new[rn_old, , drop = FALSE]
    }
    old <- as.data.frame(old); new <- as.data.frame(new)
    faltan <- setdiff(names(old), names(new))
    if (length(faltan)) {
      return(paste0(path, ": faltan columnas ", paste(faltan, collapse = ", ")))
    }
    if (nrow(old) != nrow(new)) return(paste0(path, ": cambió el número de filas"))
    msgs <- character()
    for (nm in names(old)) {
      a <- old[[nm]]; b <- new[[nm]]
      if (is.numeric(a) && is.numeric(b)) {
        d <- max(abs(a - b), na.rm = TRUE)
        na_dif <- any(is.na(a) != is.na(b))
        if ((is.finite(d) && d > 1e-9) || na_dif) {
          msgs <- c(msgs, sprintf("%s$%s: dif máx %.3g", path, nm, d))
        }
      } else if (!identical(as.character(a), as.character(b))) {
        msgs <- c(msgs, paste0(path, "$", nm, ": valores distintos"))
      }
    }
    return(msgs)
  }
  if (is.list(old)) {
    msgs <- character()
    for (nm in names(old)) {
      if (is.null(new[[nm]])) {
        msgs <- c(msgs, paste0(path, "/", nm, ": falta"))
      } else {
        msgs <- c(msgs, comparar(old[[nm]], new[[nm]], paste0(path, "/", nm)))
      }
    }
    return(msgs)
  }
  if (is.numeric(old)) {
    if (length(old) != length(new) || max(abs(old - new), na.rm = TRUE) > 1e-9) {
      return(paste0(path, ": valores distintos"))
    }
    return(character())
  }
  if (!identical(old, new)) return(paste0(path, ": valores distintos"))
  character()
}

escs <- escenarios_golden()
fallas <- 0L
for (nm in names(escs)) {
  t0 <- Sys.time()
  out <- correr(escs[[nm]])
  seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  f <- file.path(golden_dir, paste0(nm, ".rds"))
  if (identical(mode, "freeze")) {
    saveRDS(out, f)
    message(sprintf("%-14s guardado (%.1f s)", nm, seg))
  } else {
    old <- readRDS(f)
    msgs <- comparar(old, out, nm)
    if (length(msgs)) {
      fallas <- fallas + 1L
      message(sprintf("%-14s DIFERENTE (%.1f s)", nm, seg))
      message(paste("  ", head(msgs, 25), collapse = "\n"))
    } else {
      message(sprintf("%-14s igual (%.1f s)", nm, seg))
    }
  }
}
if (!identical(mode, "freeze") && fallas > 0L) {
  stop(fallas, " escenario(s) con diferencias.")
}
