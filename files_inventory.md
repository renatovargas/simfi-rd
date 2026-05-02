# Inventario de archivos — dom_tax

Repositorio: herramienta de microsimulación fiscal para República Dominicana.
La app corre localmente (Ministerio de Hacienda); nunca se despliega en un servidor Shiny.

---

## 1. Punto de entrada

| Archivo | Descripción |
|---------|-------------|
| `app.R` | App Shiny principal. Importa librerías, hace `source` de `R/00_autoload.R`, carga el catálogo ITBIS, define UI y servidor. |

---

## 2. Funciones R auxiliares (`R/`)

Cargados en orden por `R/00_autoload.R → autoload_dom_r()`.

| Archivo | Rol |
|---------|-----|
| `R/00_autoload.R` | Carga todos los archivos `R/` en orden de dependencias. |
| `R/config.R` | Define `get_dom_paths()` (todas las rutas) y `assert_required_packages()`. |
| `R/param_csv.R` | `read_param_csv()`: lee un CSV de parámetros con `readr` + `janitor`. |
| `R/load_parameters.R` | `load_global_params()`, `scenario_inputs_to_escenarios()`, `build_sim_comp_override_row()`, `default_scenario_inputs()`. |
| `R/isr_bracket_helpers.R` | `pipeline_isr_from_brackets()`: convierte límites/tasas de la UI al formato `tramo`/`base`/`tasa` del pipeline. |
| `R/sim_sub_helpers.R` | `read_sim_sub_template_row()`, `patch_sim_sub_row_from_ui()`: manejo del subsidio eléctrico. |
| `R/pipeline_runner.R` | `run_pipeline_cached()`: ejecuta los scripts 01–05 en un entorno aislado con caché RDS en memoria (sin escritura de archivos intermedios a disco). |
| `R/insumos_dictionary.R` | `load_insumos_catalog()`, helpers de incidencia por instrumento. |
| `R/module_summary.R` | `prepare_dom_list()`, `get_escenario_summary()`. |
| `R/analyst_fixture_scenario.R` | `read_analyst_fixture_scenario_inputs()`: carga el escenario de prueba desde `tests/fixtures/`. |
| `R/dom_dashboard_charts.R` | Funciones de visualización Plotly: `comb01_plotly_native()`, `graficar_incidencia_plotly()`, `plotly_npov_horizontal()`, `fiscal_post_measures_mat()`, `kakwani_summary_matrices()`, `procesar_fila_dom()`, `incidencia_columna()`, `tabla_macro_region()`, `plotly_compensacion_bars()`, `labels_inc_dom()`, etc. |
| `R/itbis_catalog_builder.R` | `load_itbis_catalog()`, `tasa_efectiva_desde_listas()`, `build_detalle_itbis_override()`, `build_sim_renta_override()`, `itbis_row_key()`, `itbis_subclase_key()`. |
| `R/run_dom_scenario.R` | `run_dom_scenario()`: entrada principal para ejecutar un escenario completo (prepara extras, llama al pipeline, retorna resultados). |
| `R/module_itbis.R` | Stub (la lógica vive en el pipeline y en `itbis_catalog_builder.R`). |
| `R/module_isr.R` | Stub. |
| `R/module_subsidy.R` | Stub. |
| `R/module_compensation.R` | Stub. |

---

## 3. Scripts del pipeline (`scripts/pipeline/`)

Se ejecutan secuencialmente vía `run_pipeline_cached()`. No se corren directamente desde la app; se `sys.source()`-an en un entorno aislado.

| Archivo | Rol |
|---------|-----|
| `scripts/pipeline/00_DOM26_Master.R` | Orquestador para ejecución en lote (no lo usa la app). |
| `scripts/pipeline/01_DOM26_itbis.R` | Microsimulación del ITBIS: aplica tasas a gastos de hogares, calcula incidencia. Lee `data/presim/`: `DOM26_gtotals.RDS`, `DOM26_gtotal.RDS`, `DOMCEQ_DIncome.RDS`, `urban.RDS`, `DOM26_SDIncome.RDS`, `mip2013_rd.RDS`. |
| `scripts/pipeline/02_DOM26_irenta.R` | Microsimulación del ISR personal. Lee `data/presim/DOM26_IngImponible.RDS`. |
| `scripts/pipeline/03_DOM26_subs.R` | Microsimulación del subsidio eléctrico. Lee `data/presim/elec.RDS`, `DOM26_SDIncome.RDS`, etc. |
| `scripts/pipeline/04_DOM26_compensacion.R` | Política de compensación post-reforma. Lee resultados intermedios de pasos 01–03 (vía caché en memoria). |
| `scripts/pipeline/05_DOM26_ConsolidaEscenarios.R` | Consolida indicadores (Gini, FGT, Palma, Theil, Kakwani, incidencia) para todos los escenarios activos. Produce `resultados_escenarios` en la app. |
| `scripts/pipeline/06_DOM26_Reportes.R` | Generación de reportes (no lo usa la app en producción). |
| `scripts/pipeline/run_batch_local.R` | Runner local en lote (no lo usa la app). |

---

## 4. Parámetros en CSV (`data/params/`)

Reemplazan al Excel `DOM_parametros_simulaciones.xlsx`. Los exporta `scripts/export_param_xlsx_to_csv.R`.

| Archivo CSV | Descripción |
|-------------|-------------|
| `itbis_base.csv` | Catálogo completo de productos ITBIS (~6 600 variedades) con tasa base (`tasa`), clasificación (`grupo`: `exentos` / `gravados`), codigos COICOP, sector MIP. **También es la fuente de `data/itbis_base_catalog.csv`** (ver §5). |
| `itbis_sim.csv` | Tasas detalladas por variedad para cada escenario ITBIS (`itbis_alt1`, `itbis_alt2`, …). |
| `sim_itbis.csv` | Metadatos de escenarios ITBIS (tasa estándar, opciones). |
| `sim_renta.csv` | Escenarios de ISR: tramos, bases acumuladas y tasas marginales. |
| `sim_sub.csv` | Escenarios del subsidio eléctrico: bloques kWh, cargos fijos y tarifas por distribuidora. |
| `sim_comp.csv` | Metadatos de escenarios de compensación. |
| `esc_comp.csv` | Parámetros adicionales de compensación. |
| `escenarios.csv` | Índice de escenarios. |
| `global_params.csv` | Parámetros macroeconómicos (PIB, recaudación total ITBIS, ISR, subsidios). |
| `coicop.csv` | Clasificación COICOP (auxiliar). |
| `tipo_establecimiento.csv` | Tipos de establecimiento (ENGHI). |
| `resumen.csv` | Etiquetas y definiciones de indicadores resumen. |
| `sensibilidad.csv` | Parámetros de análisis de sensibilidad. |
| `dir_transfers.csv` | Parámetros de transferencias directas. |
| `dom_params.R` | Parámetros adicionales en formato R (complementa los CSV). |

---

## 5. Catálogo ITBIS (en git)

| Archivo | Descripción |
|---------|-------------|
| `data/itbis_base_catalog.csv` | Catálogo de productos ITBIS con códigos rellenados a la izquierda para orden estable. Generado una sola vez desde `data/params/itbis_base.csv` con `Rscript scripts/build_itbis_base_catalog.R` y **commiteado al repo**. No cambia salvo actualización legislativa. **La app no puede iniciar sin este archivo.** |

> Para regenerarlo (si se actualiza `itbis_base.csv`):
> ```bash
> cp data/params/itbis_base.csv data/itbis_base_catalog.csv
> Rscript scripts/build_itbis_base_catalog.R
> git add data/itbis_base_catalog.csv && git commit -m "update itbis_base_catalog"
> ```

---

## 6. Microdatos pre-simulación (`data/presim/`) — en git

Archivos binarios RDS producidos por la etapa de preprocesamiento Stata/R.
Son suficientemente compactos para vivir en el repositorio.
Nunca cambian entre escenarios de política; son la base de la microsimulación.

| Archivo | Encuesta / Fuente | Descripción |
|---------|-------------------|-------------|
| `DOM26_gtotals.RDS` | ENHOGAR 2018 (ENGHI) | Gastos de hogares por variedad de producto. |
| `DOM26_gtotal.RDS` | ENHOGAR 2018 (ENGHI) | Gasto total del hogar. |
| `DOMCEQ_DIncome.RDS` | ENHOGAR 2018 (ENGHI) | Ingreso disponible per cápita, ponderadores, líneas de pobreza. |
| `urban.RDS` | ENHOGAR 2018 (ENGHI) | Clasificación urbano/rural del hogar. |
| `DOM26_SDIncome.RDS` | ENCFT 2024 | Ingreso disponible, trimestre, factor de expansión anual, zona. |
| `DOM26_IngImponible.RDS` | ENCFT 2024 | Ingreso imponible para ISR (por hogar/persona). |
| `mip2013_rd.RDS` | Matriz Insumo-Producto 2013 | Coeficientes técnicos para traslado del ITBIS. |
| `elec.RDS` | ENCFT 2024 | Consumo y gasto eléctrico de los hogares para el subsidio. |

---

## 7. Otros datos (`data/`)

| Archivo | Descripción |
|---------|-------------|
| `data/glosario_indicadores.csv` | Glosario de indicadores (tabla en pestaña "Glosario" de la app). |
| `data/glosario_grupos.csv` | Glosario de grupos de análisis. |
| `data/glosario_variables.csv` | Glosario de variables del modelo. |
| `data/scenario_descriptions.csv` | Etiquetas de escenarios. |
| `data/geodata/macro_regiones_rd.dbf` | Geodata de macrorregiones (tabla de atributos). |
| `data/geodata/macro_regiones_rd.prj` | Proyección geográfica. |
| `data/geodata/macro_regiones_rd.qmd` | Metadatos geográficos. |
| `data/survey/Libro_de_código_ENHOGAR_2024_Personas.htm` | Libro de códigos ENHOGAR 2024 (personas). |
| `data/survey/Libro_de_código_ENHOGAR_2024_Hogares.htm` | Libro de códigos ENHOGAR 2024 (hogares). |

---

## 8. Resultados intermedios opcionales (`data/mod/`)

| Archivo | Descripción |
|---------|-------------|
| `DOM_insumos.rds` | Plantilla de insumos analíticos (opcional; si existe, enriquece las pestañas "Por instrumento"). |
| `diccionario.xlsx` | Diccionario de variables para los insumos (opcional). |

---

## 9. Fixtures de prueba

| Archivo | Descripción |
|---------|-------------|
| `tests/fixtures/scenario_analyst_escenario_2.json` | Escenario de referencia para pruebas (ITBIS `sim_itbis=2` + ISR + subsidio), usado por el botón "Ejecutar prueba". |

---

## 10. Scripts utilitarios

| Archivo | Descripción |
|---------|-------------|
| `scripts/export_param_xlsx_to_csv.R` | Convierte `DOM_parametros_simulaciones.xlsx` a CSVs bajo `data/params/`. Sólo se necesita al actualizar parámetros desde Excel. |
| `scripts/build_itbis_base_catalog.R` | Rellena códigos del catálogo ITBIS. Se corre una vez al instalar o al actualizar `itbis_base.csv`. |

---

## 11. Estructura del repo

```
dom_tax/
├── app.R
├── README.md
├── files_inventory.md
├── R/                              # 12 archivos + 4 stubs
├── scripts/
│   ├── pipeline/                   # 01–05_DOM26_*.R (+ 00, 06 opcionales)
│   ├── export_param_xlsx_to_csv.R
│   └── build_itbis_base_catalog.R
├── data/
│   ├── itbis_base_catalog.csv      ← catálogo ITBIS (commiteado)
│   ├── glosario_indicadores.csv
│   ├── glosario_grupos.csv
│   ├── glosario_variables.csv
│   ├── scenario_descriptions.csv
│   ├── params/                     ← parámetros CSV + dom_params.R
│   │   ├── itbis_base.csv
│   │   ├── itbis_sim.csv
│   │   ├── sim_itbis.csv
│   │   ├── sim_renta.csv
│   │   ├── sim_sub.csv
│   │   ├── sim_comp.csv
│   │   ├── esc_comp.csv
│   │   ├── escenarios.csv
│   │   ├── global_params.csv
│   │   ├── dom_params.R
│   │   └── ...
│   ├── presim/                     ← microdatos RDS (8 archivos)
│   ├── mod/                        ← salidas opcionales del pipeline
│   └── geodata/                    ← macro_regiones_rd.*
└── tests/
    └── fixtures/
        └── scenario_analyst_escenario_2.json
```
