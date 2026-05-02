# Simulador de escenarios fiscales (simfi-rd)

Panel Shiny para construcción y comparación de escenarios de reforma fiscal para República Dominicana. Corre localmente; no requiere servidor Shiny.

---

## Instalación de paquetes R

Ejecutar **una sola vez** en la consola de R antes de lanzar la app:

```r
install.packages(c(
  # Shiny / UI
  "shiny", "bslib", "DT", "jsonlite", "plotly", "RColorBrewer",
  # Tidyverse core
  "dplyr", "tidyr", "tibble", "readr", "stringr", "purrr",
  # Datos
  "readxl", "openxlsx", "data.table", "janitor", "labelled",
  # Estadística / econometría
  "scales", "KernSmooth", "collapse", "rlang", "statar", "Hmisc",
  # Espacial
  "sf",
  # Reportes
  "ggplot2", "gt"
), dependencies = TRUE)
```

---

## Lanzar la app

```r
shiny::runApp("app.R")
```

O desde RStudio: abrir `app.R` y hacer clic en **Run App**.

---

## Flujo de trabajo

La app guía al analista por seis pantallas secuenciales con botones **Anterior / Siguiente**:

| Pantalla | Contenido |
|----------|-----------|
| **1 · ITBIS** | Tasas por producto (grupo, subclase o variedad). Filtro opcional para mostrar solo productos exentos. |
| **2 · Renta** | Escala del ISR personal (opcional; personalizable por tramos). |
| **3 · Subsidio** | Política de subsidio eléctrico (opcional personalización de bloques y tarifas por distribuidora). |
| **4 · Compensación** | Política de compensación post-reforma (opcional). |
| **5 · Revisar** | Nombre del escenario, guardar en una de las tres ranuras, exportar / importar JSON. |
| **6 · Simular** | Ejecutar todos los escenarios guardados y explorar resultados (pobreza, desigualdad, incidencia, recaudación). |

Se pueden definir y comparar **hasta tres escenarios** en una misma sesión.

---

## Nota: Preparación del catálogo ITBIS (una vez)

Si `data/itbis_base_catalog.csv` no existe en el repositorio:

```bash
cp data/pipeline/r_params/param_csv/itbis_base.csv data/itbis_base_catalog.csv
Rscript scripts/build_itbis_base_catalog.R
```

