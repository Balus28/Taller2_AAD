# Taller 2 - Análisis Avanzado de Datos

Proyecto en **R + RStudio + Git** para resolver el Taller 2 de manera reproducible.

## Estructura

- `Taller2.Rmd`: notebook principal listo para compilar a HTML/PDF.
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
3. Compile `Taller2.Rmd`.


## Recomendación Git

```bash
git init
git add .
git commit -m "Initial sophisticated solution for Taller 2"
```

## Nota

Este proyecto fue preparado para seguir fielmente el enunciado: separación 90/10, selección de knots por CV de 10 folds, comparación entre paradigmas y repetición de todo el proceso 10 veces, además de incluir la parte teórica de Nadaraya-Watson. El taller recomienda explícitamente un cuaderno `.Rmd` y también incentiva versionar en git.
