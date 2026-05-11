# Plan de trabajo CRAN para agroclimR

Fecha de reevaluacion: 2026-05-11

## 1) Estado actual

El paquete ya avanzo desde el diagnostico inicial: `DESCRIPTION` usa version
`0.1.0`, `Depends: R (>= 4.1.0)` y roxygen reciente. La prioridad inmediata ya
no es cambiar metadatos basicos, sino cerrar inconsistencias de documentacion,
ejemplos, writers y chequeos CRAN.

### Fortalezas
- Estructura estandar de paquete R: `DESCRIPTION`, `NAMESPACE`, `R/`, `man/`,
  `tests/` y `vignettes/`.
- API exportada para generar insumos de DSSAT, ORYZA y AquaCrop.
- Datos de ejemplo disponibles (`weather`, `soil`, observaciones agronomicas).
- Version y dependencia minima de R ya ajustadas para una primera release.

### Riesgos CRAN actuales
1. **Documentacion de writers desalineada con la API real.** Algunos archivos
   `write_exp_*` describen funciones o argumentos de otros modelos, y algunos
   `write_soil_*` documentan argumentos que no existen en la firma.
2. **Ejemplos que escriben en el directorio de trabajo.** Deben usar `tempdir()`
   para evitar efectos secundarios durante `R CMD check`.
3. **Writers con construccion manual de rutas.** Conviene usar `file.path()` para
   evitar nombres incorrectos y mejorar portabilidad.
4. **Cobertura de tests insuficiente para writers.** Se necesitan pruebas que
   validen archivos creados, extensiones esperadas, encabezados y limpieza.
5. **Funciones con binarios externos.** `run_drates_param()` y funciones afines
   deben mantenerse protegidas con validaciones, mensajes claros y ejemplos
   seguros.
6. **NOTEs por NSE/imports.** Algunas funciones usan columnas de `dplyr` y helpers
   que pueden generar notas de variables globales si no se declaran o importan
   bien.

## 2) Objetivo

Llegar a `R CMD check --as-cran` con 0 ERROR y 0 WARNING, dejando como maximo
NOTEs justificables antes del envio inicial a CRAN.

## 3) Trabajo inmediato

### P0 - Cierre tecnico
- Revisar y corregir roxygen de:
  - `write_wth_oryza()`, `write_wth_dssat()`, `write_wth_aquacrop()`.
  - `write_soil_oryza()`, `write_soil_dssat()`, `write_soil_aquacrop()`.
  - `write_exp_oryza()`, `write_exp_dssat()`, `write_exp_aquacrop()`.
- Regenerar `man/*.Rd` desde roxygen.
- Cambiar ejemplos de writers para escribir en `tempdir()`.
- Normalizar rutas de salida con `file.path()` donde aplique.
- Corregir mensajes que mencionan el modelo equivocado.

### P1 - Tests
- Crear fixtures pequenos y deterministas para writers.
- Probar que cada writer retorna rutas existentes.
- Probar encabezados o marcas minimas de formato por modelo.
- Probar rutas de error cuando faltan columnas obligatorias.
- Mantener los tests sin depender de binarios externos.

### P2 - Check CRAN
- Ejecutar `devtools::document()`.
- Ejecutar `devtools::check(cran = TRUE)` o
  `rcmdcheck::rcmdcheck(args = "--as-cran")`.
- Crear/actualizar `cran-comments.md` con plataformas y notas residuales.

## 4) Backlog priorizado

### Bloqueantes
- Documentacion roxygen completa y consistente con firmas reales.
- Ejemplos CRAN-safe.
- Tests funcionales de writers.
- `run_drates_param()` sin dependencias implicitas ni ejemplos inseguros.

### Muy recomendados
- Revisar imports/suggests y uso de `::`.
- Corregir textos editoriales en README/vignettes.
- Unificar estilo de mensajes, errores y rutas de salida.

### Optimizacion
- Medir rendimiento con archivos climaticos grandes.
- Mejorar validaciones de entrada con mensajes mas especificos.
- Considerar helpers compartidos para escritura de archivos si se consolida el
  nucleo `write_file_model`.

## 5) Indicadores de exito

- `R CMD check --as-cran`: 0 ERROR, 0 WARNING.
- Ejemplos reproducibles sin escribir fuera de temporales.
- Tests de writers cubriendo al menos clima, suelo y experimentos.
- Documentacion de funciones exportadas sin argumentos faltantes o sobrantes.
- Mensajes de salida coherentes con DSSAT, ORYZA y AquaCrop.
