# Load global and table parameters from Excel. Scenario *selection* is driven by
# the Shiny UI (`scenario_inputs`), not by `escenarios` / `sim_itbis` sheets.

#' @param paths Output of [get_dom_paths()].
load_global_params <- function(paths) {
  read_param_csv("global_params", paths$param_csv)
}

#' Read all parameter CSVs once (for inspection / defaults).
#' @param paths Output of [get_dom_paths()].
load_parameter_tables <- function(paths) {
  files <- list.files(
    paths$param_csv,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  tab <- lapply(files, function(f) {
    janitor::clean_names(readr::read_csv(f, show_col_types = FALSE))
  })
  stats::setNames(tab, tools::file_path_sans_ext(basename(files)))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Build the one-row `escenarios` tibble expected by pipeline scripts 01–05.
#'
#' @param inputs List from Shiny or [default_scenario_inputs()].
#' @return A `tibble` with one row compatible with `escenarios` in Excel.
scenario_inputs_to_escenarios <- function(inputs = list()) {
  inpt <- default_scenario_inputs()
  if (length(inputs)) {
    for (nm in names(inputs)) {
      inpt[[nm]] <- inputs[[nm]]
    }
  }
  tibble::tibble(
    escenario = as.integer(inpt$escenario %||% 1L),
    activo = 1L,
    comparativo = as.integer(inpt$comparativo %||% 1L),
    sim_itbis = as.integer(inpt$sim_itbis %||% 1L),
    sim_renta = as.integer(inpt$sim_renta %||% 0L),
    sim_sub = as.integer(inpt$sim_sub %||% 0L),
    sim_com = as.integer(inpt$sim_com %||% 1L),
    des_escenario = as.character(inpt$des_escenario %||% "Simulación"),
    des_corto = as.character(inpt$des_corto %||% inpt$label %||% "Esc. 1")
  )
}

#' Default scenario indices (aligned with typical rows in param_csv/*.csv).
default_scenario_inputs <- function() {
  list(
    label = "Contrafactual",
    escenario = 1L,
    comparativo = 1L,
    sim_itbis = 1L,
    sim_renta = 0L,
    sim_sub = 0L,
    sim_com = 1L,
    des_escenario = "Escenario simulado",
    des_corto = "Reforma simulada"
  )
}

#' Una fila `sim_comp` para sustituir la lectura desde CSV (app personalizada).
#' Alineado con [scripts/pipeline/04_DOM26_compensacion.R].
build_sim_comp_override_row <- function(
    sim_comp_id,
    grupo_com,
    metodo_com,
    valor_com = 0,
    decil_com = NA_integer_,
    icv_com = NA_real_,
    decil_est = NA_integer_
) {
  tibble::tibble(
    sim_comp = as.integer(sim_comp_id),
    activo = 1L,
    grupo_com = as.integer(grupo_com),
    metodo_com = as.integer(metodo_com),
    valor_com = as.numeric(valor_com),
    decil_com = as.integer(decil_com),
    icv_com = as.numeric(icv_com),
    decil_est = as.integer(decil_est)
  )
}
