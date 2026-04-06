# Taller 2 - Análisis Avanzado de Datos

Proyecto en **R + RStudio + Git** para resolver el Taller 2 de manera reproducible.

## Estructura

- `workshop2_solution.Rmd`: notebook principal listo para compilar a HTML/PDF.
- `R/utils.R`: funciones auxiliares para partición, validación cruzada, ajuste y evaluación.
- `scripts_run_analysis.R`: script para ejecutar el flujo completo sin abrir el notebook.
- `output/`: resultados exportados (tablas y figuras).
- `data/`: reservado si desea guardar datos intermedios.

## Paquetes requeridos

```r
install.packages(c(
  "tidyverse", "ISLR2", "splines", "broom", "glue", "patchwork", "knitr", "rmarkdown"
))
```

## Cómo usar en RStudio

1. Abra la carpeta del proyecto en RStudio.
2. Instale los paquetes requeridos.
3. Compile `workshop2_solution.Rmd`.
4. Si desea exportar resultados primero, corra:

```r
source("scripts_run_analysis.R")
```

## Recomendación Git

```bash
git init
git add .
git commit -m "Initial sophisticated solution for Taller 2"
```

## Nota

Este proyecto fue preparado para seguir fielmente el enunciado: separación 90/10, selección de knots por CV de 10 folds, comparación entre paradigmas y repetición de todo el proceso 10 veces, además de incluir la parte teórica de Nadaraya-Watson. El taller recomienda explícitamente un cuaderno `.Rmd` y también incentiva versionar en git. fileciteturn1file1
