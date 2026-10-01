## Banco Mundial

## Herramienta de Microsimulación del IVA
## Country: República Dominicana 2026
## Authors: Maynor Cabrera, Renato Vargas
## Script: Maynor Cabrera
## 0_DOM26_Master.R
## E-email:       mynorvc@gmail.com
##  ----------------------------------------------------
##  Fecha de creación:    2 Mar 2026 - 12:53
## Do-file version:    	  1.0
## Modification Date:     2 Mar 2026

## ==================================================

rm(list = ls())

## 0. Paquetes -----------------------------------------------------------------
list_libraries <- c(
  "readxl", "tidyverse", "janitor", "stringr", "openxlsx",
  "data.table", "statar", "scales", "labelled", "KernSmooth", "sf", "collapse",
  "gt", "Hmisc", "rlang", "ragg"
)


# Función para instalar paquetes si no están instalados
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    require(package, character.only = TRUE) # Cargar después de instalar
  }
}

# Aplicar la función a cada paquete en la lista
lapply(list_libraries, install_if_missing)

# Opcional: Verificar si todos los paquetes se cargaron correctamente
loaded_successfully <- sapply(list_libraries, require, character.only = TRUE)
if (all(loaded_successfully)) {
  print("Todos los paquetes se instalaron y cargaron correctamente.")
} else {
  print("Algunos paquetes no se pudieron cargar. Revisa los mensajes de error")
}

## 1. Folders ------------------------------------------------------------------

### folder base
path_o <- case_when(
  Sys.getenv("USERNAME") == "mynor" ~
    "C:/Users/mynor/Dropbox (Personal)/Dropbox/zotros/BM/DOM_26/SIM/",
  Sys.getenv("USERNAME") == "RNDM" ~
    "D:/BM/DOM_26/SIM/"
)

### Cambiar a folder base
setwd(path_o)

finput  <- paste0(path_o, "/data/input/")
fpresim <- paste0(path_o, "/data/r_presim/")
fdbmod  <- paste0(path_o, "/data/r_mod/")
fparams <- paste0(finput, "DOM_parametros_simulaciones.xlsx")

## 2. Importar archivos --------------------------------------------------------

### Parametros globales
parm_glob <- read_excel(fparams, col_names = TRUE,
                        sheet = "global_params") %>%
  janitor::clean_names()
  
## Escenarios
escenarios <- read_excel(fparams, col_names = TRUE, sheet = "escenarios") %>%
  janitor::clean_names() %>%
  filter(activo == 1)

## 3. Ejecutar scripts  --------------------------------------------------------

ptm1 <- proc.time() ## tiempo de ejecución
source("./scripts_r/01_DOM26_itbis.R")
proc.time() - ptm1

ptm2 <- proc.time()
source("./scripts_r/02_DOM26_irenta.R")
proc.time() - ptm2

ptm3 <- proc.time()
sink(nullfile())
source("./scripts_r/03_DOM26_subs.R")
sink()
proc.time() - ptm3

ptm4 <- proc.time()
source("./scripts_r/04_DOM26_compensacion.R")
proc.time() - ptm4

ptm5 <- proc.time()
source("./scripts_r/05_DOM26_ConsolidaEscenarios.R")
proc.time() - ptm5

ptm6 <- proc.time()
source("./scripts_r/06_DOM26_Reportes.R")
proc.time() - ptm6

proc.time() - ptm1



