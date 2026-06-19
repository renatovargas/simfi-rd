# Simulador de escenarios fiscales (simfi-rd)

Panel Shiny para construcción y comparación de escenarios de reforma fiscal para República Dominicana. Corre localmente; no requiere servidor Shiny.

---

## Instalación de paquetes R

Ejecutar **una sola vez** en la consola de R antes de lanzar la app:

```r
install.packages(c(
  # Shiny / UI
  "shiny", "bslib", "DT", "plotly", "RColorBrewer",
  # Tidyverse core
  "dplyr", "tidyr", "tibble", "readr", "stringr", "purrr",
  # Datos
  "readxl", "openxlsx", "data.table", "janitor", "labelled", "haven",
  # Estadística / econometría
  "scales", "KernSmooth", "collapse", "rlang", "statar", "Hmisc",
  # Misceláneos (requeridos por el pipeline)
  "sf", "ggplot2", "gt", "jsonlite"
), dependencies = TRUE)
```

---

## Lanzar la app

```r
shiny::runApp("app.R")
```

O desde RStudio / Positron: abrir `app.R` y hacer clic en **Run App**.

---

## Flujo de trabajo

La app guía al analista por seis pantallas secuenciales con botones **Anterior / Siguiente**:

| Pantalla | Contenido |
|----------|-----------|
| **1 · ITBIS** | Tasas por producto, modificables por grupo, subclase o variedad individual. Filtros de búsqueda por grupo, subclase y tasa. |
| **2 · Renta** | Escala del ISR personal: opcional, personalizable por tramos (límites y tasas). |
| **3 · Subsidio** | Política de subsidio eléctrico: opcional, personalizable por distribuidora (bloques y tarifas). |
| **4 · Compensación** | Política de compensación post-reforma: opcional, con grupos y métodos de focalización. |
| **5 · Revisar** | Biblioteca de componentes guardados por instrumento; compositor de escenarios; exportar / importar en Excel. |
| **6 · Simular** | Ejecutar los escenarios marcados y explorar resultados (pobreza, desigualdad, incidencia, efecto fiscal). |

Se pueden construir **escenarios ilimitados** y comparar **hasta cuatro** simultáneamente en la pantalla de resultados.

---

## Gestión de sesión

La app guarda el estado automáticamente (`app_state/autosave.rds`) y lo restaura al iniciar. Para empezar desde cero use el botón **Nueva sesión (borrar todo)** en **5 · Revisar**.
