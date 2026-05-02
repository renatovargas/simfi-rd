## Banco Mundial
## Herramienta de Microsimulación del IVA
## Country: República Dominicana 2026
## Authors: Maynor Cabrera, Renato Vargas
## 0_DOM26_Master.R
##
## Ejecución local en el clon del repositorio: rutas vía R/config.R
## (data/presim, data/mod, data/params). Sin Dropbox ni setwd().

## ==================================================

rm(list = ls())

## 0. Paquetes -----------------------------------------------------------------
list_libraries <- c(
  "readxl", "tidyverse", "janitor", "stringr", "openxlsx",
  "data.table", "statar", "scales", "labelled", "KernSmooth", "sf", "collapse",
  "gt", "Hmisc", "rlang", "ragg"
)

ok <- sapply(list_libraries, requireNamespace, quietly = TRUE)
if (!all(ok)) {
  stop(
    "Faltan paquetes: ",
    paste(names(ok)[!ok], collapse = ", "),
    ". Instálelos antes de correr el master."
  )
}
invisible(lapply(list_libraries, library, character.only = TRUE))

## 1. Carpetas (repo) ----------------------------------------------------------

root <- normalizePath(".", winslash = "/", mustWork = FALSE)
source(file.path(root, "R", "config.R"))
source(file.path(root, "R", "param_csv.R"))
paths <- get_dom_paths(root)

finput <- paths$finput
fpresim <- paths$fpresim
fdbmod <- paths$fdbmod
fparam_csv <- paths$param_csv
fgeodata <- paths$fgeodata
fr_params <- paths$fr_params
path_o <- root ## compat: 06_DOM26_Reportes.R usa path_o/resultados/

## 2. Importar archivos --------------------------------------------------------

parm_glob <- read_param_csv("global_params", fparam_csv)

escenarios <- read_param_csv("escenarios", fparam_csv) %>%
  filter(activo == 1)

## 3. Ejecutar scripts  ---------------------------------------------------------

script_dir <- file.path(root, "scripts", "pipeline")

ptm1 <- proc.time()
source(file.path(script_dir, "01_DOM26_itbis.R"))
proc.time() - ptm1

ptm2 <- proc.time()
source(file.path(script_dir, "02_DOM26_irenta.R"))
proc.time() - ptm2

ptm3 <- proc.time()
source(file.path(script_dir, "03_DOM26_subs.R"))
proc.time() - ptm3

ptm4 <- proc.time()
source(file.path(script_dir, "04_DOM26_compensacion.R"))
proc.time() - ptm4

ptm5 <- proc.time()
source(file.path(script_dir, "05_DOM26_ConsolidaEscenarios.R"))
proc.time() - ptm5

ptm6 <- proc.time()
source(file.path(script_dir, "06_DOM26_Reportes.R"))
proc.time() - ptm6

proc.time() - ptm1
