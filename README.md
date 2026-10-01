# Simulador de escenarios fiscales (simfi-rd)

Panel Shiny para construcción y comparación de escenarios de reforma fiscal para República Dominicana. Corre localmente; no requiere servidor Shiny.

---

## Instalación de paquetes R

**Paso obligatorio antes de lanzar la app.** Ejecutar **una sola vez** en la consola de R:

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

No pulse **Run App** / Play hasta que esa instalación haya terminado sin errores.

---

## Lanzar la app

1. En Positron o RStudio, abra la carpeta del repositorio con **File → Open Folder…** (no solo el archivo `app.R`). Así el directorio de trabajo apunta al proyecto y la app encuentra datos y scripts.
2. Confirme que ya instaló los paquetes de la sección anterior.
3. Lance la app:

Abra el archivo `app.R` y haga clic en **Run App**.


Si la app no arranca o no funciona bien: detenga el servidor Shiny con **Ctrl+C** en la terminal o con el botón rojo de detener en Positron, verifique los pasos 1 y 2, y vuelva a lanzarla.

También es posible, teniendo la carpeta correcta abierta correr el app desde la consola de R con el siguiente comando, pero los pasos de arriba deberían funcionar bien antes de recurrir a esto.

```r
shiny::runApp("app.R")
```

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
