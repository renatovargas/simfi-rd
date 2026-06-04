suppressWarnings(suppressMessages({
  library(readr); library(dplyr); library(janitor); library(tibble)
  library(tidyr); library(purrr); library(stringr); library(rlang)
  library(KernSmooth); library(collapse); library(data.table)
  library(statar); library(labelled); library(scales); library(gt)
}))

root <- normalizePath(".", winslash = "/")
source("R/00_autoload.R", local = FALSE)
autoload_dom_r(root)

paths <- get_dom_paths(root)

cat("=== 1. ROW COUNT COMPARISON ===\n")
ctlg <- load_itbis_catalog(root)
tpl  <- read_param_csv("itbis_sim",  paths$param_csv)
base <- read_param_csv("itbis_base", paths$param_csv)
cat("catalog rows:", nrow(ctlg), "\n")
cat("itbis_sim rows:", nrow(tpl), "\n")
cat("itbis_base rows:", nrow(base), "\n")
cat("catalog ID_VARIEDAD range:", min(ctlg$ID_VARIEDAD), "-", max(ctlg$ID_VARIEDAD), "\n")
cat("itbis_sim cod_variedad range:", min(tpl$cod_variedad), "-", max(tpl$cod_variedad), "\n")
cat("itbis_base id_variedad range:", min(base$id_variedad), "-", max(base$id_variedad), "\n")

cat("\n=== 2. ITBIS OVERRIDE (rate_grupo group 1 = 100%) ===\n")
override <- build_detalle_itbis_override(
  ctlg, tpl,
  rate_grupo = list(`1` = 100),
  rate_subclase = list(), rate_variedad = list(),
  marco = "actual"
)
cat("override rows:", nrow(override), "\n")
cat("override cols:", paste(names(override), collapse=", "), "\n")
cat("itbis_alt1 max:", max(override$itbis_alt1, na.rm=TRUE), "\n")
cat("rows with itbis_alt1 = 100:", nrow(filter(override, itbis_alt1 == 100)), "\n")

cat("\n=== 3. JOIN MATCH CHECK ===\n")
join_check <- base %>%
  select(id_variedad, tasa) %>%
  left_join(override %>% select(cod_variedad, itbis_alt1),
            by = c("id_variedad" = "cod_variedad"))
cat("base rows:", nrow(base), "\n")
cat("matched (non-NA):", sum(!is.na(join_check$itbis_alt1)), "\n")
cat("unmatched (NA):", sum(is.na(join_check$itbis_alt1)), "\n")
cat("override rows not in base:", sum(!override$cod_variedad %in% base$id_variedad), "\n")
cat("group-1 products in base with itbis_alt1=100 after join:",
    sum(join_check$itbis_alt1 == 100, na.rm=TRUE), "\n")

cat("\n=== 4. FULL PIPELINE DIAGNOSTIC — ITBIS 100% group 1 ===\n")
inputs_test <- list(
  label = "Prueba 100% alimentos",
  escenario = 1L, sim_itbis = 1L, sim_renta = 0L, sim_sub = 1L, sim_com = 0L,
  itbis = list(marco = "actual",
               rate_grupo = list(`1` = 100),
               rate_subclase = list(), rate_variedad = list()),
  isr = NULL,
  sub_ele = list(custom = FALSE),
  comp = list(enabled = FALSE, sin_compensacion = TRUE)
)
extras <- prepare_pipeline_extras(inputs_test, paths)
escenarios_t <- scenario_inputs_to_escenarios(inputs_test)
res <- run_pipeline_cached(paths, escenarios_t, extras = extras)

# Get micro data from cache (DOM_simcompensacion.rds)
dr <- res$dom_results
cat("dom_results cols (grep itx/sub/sitx):",
    paste(grep("itx|sitx|ssub|sub_ele", names(dr), value=TRUE)[1:10], collapse=", "), "\n")
cat("itx_itb0_pc mean:", mean(dr$itx_itb0_pc, na.rm=TRUE), "\n")
cat("sitx_itb1_pc mean:", mean(dr$sitx_itb1_pc %||% NA, na.rm=TRUE), "\n")
# itx_itb1_pc comes from the yd computation
cat("itx_itb1_pc mean:", mean(dr$itx_itb1_pc %||% NA, na.rm=TRUE), "\n")

esc1 <- res$resultados_escenarios[[1]]
if (!is.null(esc1$pov_gral)) {
  cat("pov_gral cols:", paste(colnames(esc1$pov_gral), collapse=", "), "\n")
  cat("pov_gral Base rate:", esc1$pov_gral["rate", "Base"], "\n")
  yd_col <- grep("^yd_", colnames(esc1$pov_gral), value=TRUE)[1]
  if (!is.na(yd_col))
    cat("pov_gral", yd_col, "rate:", esc1$pov_gral["rate", yd_col], "\n")
  cat("ITBIS changed poverty?", !isTRUE(all.equal(
    esc1$pov_gral["rate", "Base"],
    esc1$pov_gral["rate", yd_col])), "\n")
}

cat("\n=== 5. COMPENSATION DIAGNOSTIC ===\n")
inputs_comp <- list(
  label = "Prueba comp fuerte",
  escenario = 1L, sim_itbis = 1L, sim_renta = 0L, sim_sub = 1L, sim_com = 1L,
  itbis = list(marco = "actual", rate_grupo = list(), rate_subclase = list(), rate_variedad = list()),
  isr = NULL,
  sub_ele = list(custom = FALSE),
  comp = list(enabled = TRUE, sin_compensacion = FALSE, sim_comp_id = 1L,
              grupo_com = 2L, metodo_com = 1L, valor_com = 0,
              decil_com = 5L, icv_com = NA, decil_est = 5L)
)
extras_c <- prepare_pipeline_extras(inputs_comp, paths)
cat("sim_comp_override:\n"); print(extras_c$sim_comp_override)
escenarios_c <- scenario_inputs_to_escenarios(inputs_comp)
cat("escenarios sim_com:", escenarios_c$sim_com, "\n")
res_c <- run_pipeline_cached(paths, escenarios_c, extras = extras_c)
dr_c <- res_c$dom_results
cat("comp cols:", paste(grep("^comp", names(dr_c), value=TRUE), collapse=", "), "\n")
if ("comp_1_pc" %in% names(dr_c)) {
  cat("comp_1_pc mean:", mean(dr_c$comp_1_pc, na.rm=TRUE), "\n")
  cat("comp_1_pc nonzero rows:", sum(dr_c$comp_1_pc != 0, na.rm=TRUE), "\n")
}
esc1c <- res_c$resultados_escenarios[[1]]
if (!is.null(esc1c$pov_gral)) {
  yd_col <- grep("^yd_", colnames(esc1c$pov_gral), value=TRUE)[1]
  cat("poverty base:", esc1c$pov_gral["rate", "Base"], "\n")
  if (!is.na(yd_col))
    cat("poverty yd:", esc1c$pov_gral["rate", yd_col], "\n")
  cat("Comp changed poverty?", !isTRUE(all.equal(
    esc1c$pov_gral["rate", "Base"], esc1c$pov_gral["rate", yd_col])), "\n")
}
cat("DIAGNOSTIC DONE\n")
