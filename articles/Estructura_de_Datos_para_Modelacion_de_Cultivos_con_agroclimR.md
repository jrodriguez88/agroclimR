# Estructura de Datos para Modelación de Cultivos con agroclimR

    ## Warning: no DISPLAY variable so Tk is not available

## Introducción

La estructura adecuada y organización de datos son fundamentales para la
modelación y simulación de cultivos, permitiendo a investigadores y
analistas reproducir los ejemplos y aprovechar al máximo las capacidades
del paquete agroclimR en R. Este paquete facilita la automatización de
tareas de modelación agroclimática, mejorando la eficiencia y precisión
de los análisis. La organización del libro de trabajo recomendado, con
hojas que abarcan desde datos generales del ensayo experimental hasta
información detallada sobre fenología, crecimiento de plantas,
rendimiento, datos del suelo y climáticos, asegura que todos los
componentes necesarios para modelos basados en procesos estén accesibles
y bien documentados.

Conocimientos en tidyverse y el uso efectivo del operador pipe %\>% son
esenciales para manipular y preparar los datos, facilitando la creación
de flujos de trabajo de procesamiento de datos que son esenciales para
la obtención de resultados confiables y la toma de decisiones informadas
en el ámbito de la agroclimatología.

A continuación, se detalla la información de cada hoja y las variables
que contiene el libro de trabajo recomendado para trabajar con
agroclimR.

## Estructura del Libro de Excel y Descripción de Variables

El libro de trabajo contiene 7 tablas que representan los diferentes
componentes de un sistema de cultivo evaluado con el objetivo de modelar
y simular cultivos (modelos basados en procesos):

- *AGRO_man*: Datos generales del ensayo experimental. Ver
  [agro](https://jrodriguez88.github.io/agroclimR/reference/agro.html)
- *FERT_obs*: Plan de fertilización de cada localidad o ensayo
  experimental. Ver
  [fertil](https://jrodriguez88.github.io/agroclimR/reference/fertil.html)
- *PHEN_obs*: Datos de fenología de cada experimento monitoreado. Ver
  [phenol](https://jrodriguez88.github.io/agroclimR/reference/phenol.html)
- *PLANT_obs*: Crecimiento del cultivo (biomasa, índice de área foliar,
  número de órganos). Observaciones derivadas de muestreos destructivos.
  Ver
  [plant](https://jrodriguez88.github.io/agroclimR/reference/plant.html)
- *YIELD_obs*: Rendimiento en grano y componentes del rendimiento
  (fertilidad, número de granos, etc.). Ver
  [yield](https://jrodriguez88.github.io/agroclimR/reference/yield.html)
- *SOIL_obs*: Datos del suelo (químico-físico). Ver
  [soil](https://jrodriguez88.github.io/agroclimR/reference/soil.html)
- *WTH_obs*: Datos climáticos agregados a escala diaria. Ver
  [weather](https://jrodriguez88.github.io/agroclimR/reference/weather.html)
- *Metadata*: Tabla que contiene la descripción de las variables en cada
  tabla anterior.

Las celdas vacias se consideran datos faltantes o nulos. Veamos en mas
detalle cada uno de los conjuntos de datos.

### AGRO - Datos Generales del Ensayo Experimental

Un conjunto de datos para información agronómica detallada y general
sobre ensayos experimentales para la modelación de cultivos. Sirve como
la tabla maestra para el libro de trabajo propuesto en el paquete
agroclimR.

| VAR_NAME | DESCRIPTION | TYPE | UNITS | Calculation method | Agrovoc URL |
|----|----|----|----|----|----|
| ID | Trial ID | character | XXXX |  |  |
| LOC_ID | locality ID | character | name |  |  |
| PROJECT | Project ID | character | name |  |  |
| CULTIVAR | Cultivar name | character | name |  |  |
| TR_N | Treatment number | character | cn |  |  |
| LAT | Latitude | numeric | Decimal degrees | GPS, G-EARTH | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_4222) |
| LONG | Longitude | numeric | Decimal degrees | GPS, G-EARTH | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_52bebf31) |
| ALT | Elevation(m above sea level) | numeric | meters | GPS, G-EARTH | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_316) |
| PDAT | Planting date | date | MM/DD/YYYY | Field Manual | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_16208) |
| CROP_SYS | Crop system | character | IRRIGATED-RAINFED | Field Manual |  |
| ESTAB | Establishment | character | TRANSPLANT-DIRECT-SEED | Field Manual |  |
| NPLDS | Number of plants/m2 | numeric | number/m² | Mean of samples (25\<DDE\<80) | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_5975) |
| SBDUR | Seed-bed duration | numeric | days |  |  |
| TRDAT | Transplanting date | date | MM/DD/YYYY |  | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_330700) |

`ID`, `LOC_ID`, `PROJECT`, `CULTIVAR` y `TR_N` son factores que
identifican un experimento único y conectan las demás tablas, con
recomendaciones tales como:

- ID de Ensayo (`ID`): Un identificador único para cada ensayo. Ejemplo:
  “LOC1T1PROJ1”.
- ID de Localidad (`LOC_ID`): Indica la ubicación del ensayo. Ejemplo:
  “LOC1”.
- ID de Proyecto (`PROJECT`): Asocia el ensayo con un proyecto
  específico. Ejemplo: “PROJ1”.
- Nombre del Cultivar (`CULTIVAR`): Especifica el cultivar de planta
  utilizado en el ensayo. Ejemplo: “CULTIVAR1”.
- Número de Tratamiento (`TR_N`): Ejemplo: “T1”.

``` r

# Código de ejemplo para mostrar las primeras filas del conjunto de datos AGRO
# Asumiendo que 'agro' es el conjunto de datos cargado con agroclimR
head(agro)
#> # A tibble: 6 × 14
#>   ID         LOC_ID PROJECT CULTIVAR TR_N    LAT  LONG   ALT PDAT       CROP_SYS
#>   <chr>      <chr>  <chr>   <chr>    <chr> <dbl> <dbl> <dbl> <date>     <chr>   
#> 1 SDTOS1MAD… SDTO   MADRI   FED2000  S1     3.91 -75.0   415 2013-04-29 IRRIGAT…
#> 2 SDTOS1MAD… SDTO   MADRII  FED2000  S1     3.91 -75.0   415 2015-06-05 IRRIGAT…
#> 3 SDTOS2MAD… SDTO   MADRI   FED2000  S2     3.91 -75.0   415 2013-12-05 IRRIGAT…
#> 4 SDTOS2MAD… SDTO   MADRII  FED2000  S2     3.91 -75.0   415 2015-11-03 IRRIGAT…
#> # ℹ 2 more rows
#> # ℹ 4 more variables: ESTAB <chr>, NPLDS <dbl>, SBDUR <lgl>, TRDAT <lgl>
str(agro)
#> tibble [7 × 14] (S3: tbl_df/tbl/data.frame)
#>  $ ID      : chr [1:7] "SDTOS1MADRI" "SDTOS1MADRII" "SDTOS2MADRI" "SDTOS2MADRII" ...
#>  $ LOC_ID  : chr [1:7] "SDTO" "SDTO" "SDTO" "SDTO" ...
#>  $ PROJECT : chr [1:7] "MADRI" "MADRII" "MADRI" "MADRII" ...
#>  $ CULTIVAR: chr [1:7] "FED2000" "FED2000" "FED2000" "FED2000" ...
#>  $ TR_N    : chr [1:7] "S1" "S1" "S2" "S2" ...
#>  $ LAT     : num [1:7] 3.91 3.91 3.91 3.91 3.91 ...
#>  $ LONG    : num [1:7] -75 -75 -75 -75 -75 ...
#>  $ ALT     : num [1:7] 415 415 415 415 415 415 415
#>  $ PDAT    : Date[1:7], format: "2013-04-29" "2015-06-05" ...
#>  $ CROP_SYS: chr [1:7] "IRRIGATED" "IRRIGATED" "IRRIGATED" "IRRIGATED" ...
#>  $ ESTAB   : chr [1:7] "DIRECT-SEED" "DIRECT-SEED" "DIRECT-SEED" "DIRECT-SEED" ...
#>  $ NPLDS   : num [1:7] 131 84 152 84 160 122 160
#>  $ SBDUR   : logi [1:7] NA NA NA NA NA NA ...
#>  $ TRDAT   : logi [1:7] NA NA NA NA NA NA ...
```

### FERT: Datos Observados del Plan de Fertilización.

Un conjunto de datos que detalla la gestión o aplicación de
fertilización a través de varios ensayos experimentales o regiones.
Incluye información sobre la identificación del ensayo, localidad, IDs
del proyecto y detalles específicos sobre las aplicaciones de
fertilizantes.

| VAR_NAME | DESCRIPTION | TYPE | UNITS | Calculation method | Agrovoc URL |
|----|----|----|----|----|----|
| ID | Trial ID | character | XXXX |  |  |
| LOC_ID | locality ID | character | name |  |  |
| FERT_No | Fertilize app number | numeric | number | Field Manual | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_10795) |
| DDE | Days after Emergence | numeric | days |  |  |
| N | Nitrogen | numeric | kg/ha | % of fertilizer | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_5192) |
| P | Phosphate | numeric | kg/ha | % of fertilizer |  |
| K | Potassium | numeric | kg/ha | % of fertilizer |  |

``` r

# Código de ejemplo para mostrar las primeras filas del conjunto de datos FERT
# Asumiendo que 'fertil' es el conjunto de datos cargado con agroclimR
head(fertil)
#> # A tibble: 6 × 7
#>   ID          LOC_ID FERT_No   DDE     N     P     K
#>   <chr>       <chr>    <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 SDTOS1MADRI SDTO         1    10  42    21.5    33
#> 2 SDTOS1MADRI SDTO         2    23  58     1.5    33
#> 3 SDTOS1MADRI SDTO         3    38  56.5   0      30
#> 4 SDTOS1MADRI SDTO         4    52  51.2   0      15
#> # ℹ 2 more rows
str(fertil)
#> tibble [37 × 7] (S3: tbl_df/tbl/data.frame)
#>  $ ID     : chr [1:37] "SDTOS1MADRI" "SDTOS1MADRI" "SDTOS1MADRI" "SDTOS1MADRI" ...
#>  $ LOC_ID : chr [1:37] "SDTO" "SDTO" "SDTO" "SDTO" ...
#>  $ FERT_No: num [1:37] 1 2 3 4 5 1 2 3 4 5 ...
#>  $ DDE    : num [1:37] 10 23 38 52 64 9 23 37 50 62 ...
#>  $ N      : num [1:37] 42 58 56.5 51.2 18.4 ...
#>  $ P      : num [1:37] 21.5 1.5 0 0 0 21.5 31.5 0 0 0 ...
#>  $ K      : num [1:37] 33 33 30 15 0 48 33 30 15 0 ...
```

### PHEN: Datos Fenológicos observados en el cultivo

Un conjunto de datos que contiene fechas fenológicas clave para los
cultivares de plantas utilizados en ensayos de modelación de cultivos.
Cada registro detalla las etapas fenológicas del crecimiento de las
plantas.

| VAR_NAME | DESCRIPTION | TYPE | UNITS | Calculation method | Agrovoc URL |
|----|----|----|----|----|----|
| ID | Trial ID | character | XXXX |  |  |
| LOC_ID | locality ID | character | name |  |  |
| CULTIVAR | Cultivar name | character | name |  |  |
| EDAT | Emergence date | date | MM/DD/YYYY | Field observation. | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_3247) |
| IDAT | Panicle initiation date | date | MM/DD/YYYY | Field observation. | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_330848) |
| FDAT | Flowering date | date | MM/DD/YYYY | Field observation. | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_3510) |
| MDAT | Maturity date | date | MM/DD/YYYY | Harvest date - 7 days | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_13933) |

``` r

# Código de ejemplo para mostrar las primeras filas del conjunto de datos PHEN
# Asumiendo que 'phenol' es el conjunto de datos cargado con agroclimR
head(phenol)
#> # A tibble: 6 × 8
#>   ID      LOC_ID CULTIVAR PDAT       EDAT       IDAT       FDAT       MDAT      
#>   <chr>   <chr>  <chr>    <date>     <date>     <date>     <date>     <date>    
#> 1 SDTOS1… SDTO   FED2000  2013-04-29 2013-05-08 2013-06-28 2013-07-28 2013-08-27
#> 2 SDTOS1… SDTO   FED2000  2015-06-05 2015-06-14 2015-08-01 2015-09-07 2015-10-14
#> 3 SDTOS2… SDTO   FED2000  2013-12-05 2013-12-12 2014-01-30 2014-03-07 2014-04-03
#> 4 SDTOS2… SDTO   FED2000  2015-11-03 2015-11-12 2015-12-22 2016-01-23 2016-02-26
#> # ℹ 2 more rows
str(phenol)
#> tibble [7 × 8] (S3: tbl_df/tbl/data.frame)
#>  $ ID      : chr [1:7] "SDTOS1MADRI" "SDTOS1MADRII" "SDTOS2MADRI" "SDTOS2MADRII" ...
#>  $ LOC_ID  : chr [1:7] "SDTO" "SDTO" "SDTO" "SDTO" ...
#>  $ CULTIVAR: chr [1:7] "FED2000" "FED2000" "FED2000" "FED2000" ...
#>  $ PDAT    : Date[1:7], format: "2013-04-29" "2015-06-05" ...
#>  $ EDAT    : Date[1:7], format: "2013-05-08" "2015-06-14" ...
#>  $ IDAT    : Date[1:7], format: "2013-06-28" "2015-08-01" ...
#>  $ FDAT    : Date[1:7], format: "2013-07-28" "2015-09-07" ...
#>  $ MDAT    : Date[1:7], format: "2013-08-27" "2015-10-14" ...
```

### PLANT: Datos Observados de Crecimiento de Cultivos.

Un conjunto de datos completo de métricas de crecimiento de plantas
observadas, incluyendo número de órganos, muestreos de materia seca y
area foliar. Estas mediciones son integrales para estudios de modelación
de cultivos y fisiología, ofreciendo percepciones sobre el desarrollo y
salud de diferentes cultivares bajo condiciones variadas.

| VAR_NAME | DESCRIPTION | TYPE | UNITS | Calculation method | Agrovoc URL |
|----|----|----|----|----|----|
| ID | Trial ID | character | XXXX |  |  |
| LOC_ID | locality ID | character | name |  |  |
| CULTIVAR | Cultivar name | character | name |  |  |
| SAMPLING_DATE | Sampling date | date | MM/DD/YYYY |  |  |
| LAI_OBS | Leaf Area Index observed | numeric | m² leaf / m² ground | LICOR LI-3100 Leaf Scanner | [Link](http://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_35196) |
| LAI_SE | LAI standard deviation | numeric | m² leaf / m² ground |  |  |
| WLVG_OBS | Green leaf dry weight | numeric | kg/ha | Destructive sampling (MADR:1m, COL:0.5m) | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_2398) |
| WLVG_SE | WLVG Standard Error | numeric | kg/ha |  |  |
| WLVD_OBS | Dead leaf dry weight | numeric | kg/ha | Destructive sampling (MADR:1m, COL:0.5m) | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_2398) |
| WLVD_SE | WLVD Standard Error | numeric | kg/ha |  |  |
| WST_OBS | Stem dry weight | numeric | kg/ha | Destructive sampling (MADR:1m, COL:0.5m) |  |
| WST_SE | WST Standard Error | numeric | kg/ha |  |  |
| WSO_OBS | Panicle dry weight | numeric | kg/ha | Destructive sampling (MADR:1m, COL:0.5m) |  |
| WSO_SE | WSO Standard Error | numeric | kg/ha |  |  |
| WAGT_OBS | Total dry weight | numeric | kg/ha | Sum of the components |  |
| WAGT_SE | WAGT Standard Error | numeric | kg/ha |  |  |
| NLV_OBS | Number of green leaves | numeric | number/m² | Destructive sampling (MADR:1m, COL:0.5m) | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_4243) |
| NLV_SE | NLV Standard Error | numeric | number/m² |  |  |
| NST_OBS | Number of Stem | numeric | number/m² | Destructive sampling (MADR:1m, COL:0.5m) |  |
| NST_SE | NST Standard Error | numeric | number/m² |  |  |
| NP_OBS | Number of Panicles | numeric | number/m² | Destructive sampling (MADR:1m, COL:0.5m) | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24557) |
| NP_SE | NP Standard Error | numeric | number/m² |  |  |

``` r

# Código de ejemplo para mostrar las primeras filas del conjunto de datos PLANT
# Asumiendo que 'plant' es el conjunto de datos cargado con agroclimR
head(plant)
#> # A tibble: 6 × 22
#>   ID      LOC_ID CULTIVAR SAMPLING_DATE WLVG_OBS WLVG_SE LAI_OBS LAI_SE WLVD_OBS
#>   <chr>   <chr>  <chr>    <date>           <dbl>   <dbl>   <dbl>  <dbl>    <dbl>
#> 1 SDTOS1… SDTO   FED2000  2013-05-21        316.    20.4      NA     NA      0  
#> 2 SDTOS1… SDTO   FED2000  2013-05-28        544.   122.       NA     NA      0  
#> 3 SDTOS1… SDTO   FED2000  2013-06-07        752.   168.       NA     NA     38.1
#> 4 SDTOS1… SDTO   FED2000  2013-06-12        816.   169.       NA     NA     36.3
#> # ℹ 2 more rows
#> # ℹ 13 more variables: WLVD_SE <dbl>, WST_OBS <dbl>, WST_SE <dbl>,
#> #   WSO_OBS <dbl>, WSO_SE <dbl>, WAGT_OBS <dbl>, WAGT_SE <dbl>, NLV_OBS <lgl>,
#> #   NLV_SE <lgl>, NST_OBS <lgl>, NST_SE <lgl>, NP_OBS <lgl>, NP_SE <lgl>
str(plant)
#> tibble [77 × 22] (S3: tbl_df/tbl/data.frame)
#>  $ ID           : chr [1:77] "SDTOS1MADRI" "SDTOS1MADRI" "SDTOS1MADRI" "SDTOS1MADRI" ...
#>  $ LOC_ID       : chr [1:77] "SDTO" "SDTO" "SDTO" "SDTO" ...
#>  $ CULTIVAR     : chr [1:77] "FED2000" "FED2000" "FED2000" "FED2000" ...
#>  $ SAMPLING_DATE: Date[1:77], format: "2013-05-21" "2013-05-28" ...
#>  $ WLVG_OBS     : num [1:77] 316 544 752 816 1495 ...
#>  $ WLVG_SE      : num [1:77] 20.4 122.4 168.4 168.8 250.4 ...
#>  $ LAI_OBS      : num [1:77] NA NA NA NA NA NA NA NA NA NA ...
#>  $ LAI_SE       : num [1:77] NA NA NA NA NA NA NA NA NA NA ...
#>  $ WLVD_OBS     : num [1:77] 0 0 38.1 36.3 181.7 ...
#>  $ WLVD_SE      : num [1:77] 0 0 18.4 13.2 62 ...
#>  $ WST_OBS      : num [1:77] 294 644 913 1083 2106 ...
#>  $ WST_SE       : num [1:77] 20 132 186 134 339 ...
#>  $ WSO_OBS      : num [1:77] 0 0 0 0 0 ...
#>  $ WSO_SE       : num [1:77] 0 0 0 0 0 ...
#>  $ WAGT_OBS     : num [1:77] 610 1188 1704 1935 3783 ...
#>  $ WAGT_SE      : num [1:77] 37.2 254 350.8 288.4 628.8 ...
#>  $ NLV_OBS      : logi [1:77] NA NA NA NA NA NA ...
#>  $ NLV_SE       : logi [1:77] NA NA NA NA NA NA ...
#>  $ NST_OBS      : logi [1:77] NA NA NA NA NA NA ...
#>  $ NST_SE       : logi [1:77] NA NA NA NA NA NA ...
#>  $ NP_OBS       : logi [1:77] NA NA NA NA NA NA ...
#>  $ NP_SE        : logi [1:77] NA NA NA NA NA NA ...
```

### YIELD: Datos Observados de Rendimiento

Un conjunto de datos que captura el rendimiento de grano detallado y los
componentes del rendimiento de ensayos experimentales. Estas
observaciones son críticas para evaluar el rendimiento del modelo a
través de diferentes condiciones de cultivo.

| VAR_NAME | DESCRIPTION | TYPE | UNITS | Calculation method | Agrovoc URL |
|----|----|----|----|----|----|
| ID | Trial ID | character | XXXX |  |  |
| LOC_ID | locality ID | character | name |  |  |
| CULTIVAR | Cultivar name | character | name |  |  |
| YIELD_AVG | Yield average | numeric | kg/ha | Average of grain yield(3 methods: 3m², 0.5m², 1m | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_10176) |
| YIELD_MIN | Yield minimum | numeric | kg/ha | Bootstrap minimum - CI |  |
| YIELD_MAX | Yield maximum | numeric | kg/ha | Bootstrap maximum - CI |  |
| HIAM | Harvest index at maturity | numeric | ratio | WAGT_OBS/YIELD_AV | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24854) |
| HIAM_SE | HIAM Standard Error | numeric | ratio |  |  |
| PAN_fert | Panicle fertility | numeric | % | Destructive sampling |  |
| PAN_fert_SE | PAN_fert Standard Error | numeric | % |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24419) |
| GW1000 | 1000-Grain weight | numeric | g | Destructive sampling |  |
| GW1000_SE | GW1000 Standard Error | numeric | g |  |  |
| ST_M2 | Number of Stem | numeric | number/m² | Destructive sampling | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24419) |
| ST_M2_SE | ST_M2 Standard Error | numeric | number/m² |  |  |
| PAN_M2 | Number of Panicles | numeric | number/m² | Destructive sampling | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24419) |
| PAN_M2_SE | PAN_M2 Standard Error | numeric | number/m² |  |  |
| GT_PAN | Number of total grains per panicle | numeric | number/panicle | Destructive sampling | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24419) |
| GT_PAN_SE | GT_PAN Standard Error | numeric | number/panicle |  |  |
| GF_PAN | Number of fill grains per panicle | numeric | number/panicle | Destructive sampling | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24419) |
| GF_PAN_SE | GF_PAN Standard Error | numeric | number/panicle |  |  |

``` r

# Código de ejemplo para mostrar las primeras filas del conjunto de datos YIELD
# Asumiendo que 'yield' es el conjunto de datos cargado con agroclimR
head(yield)
#> # A tibble: 6 × 20
#>   ID        LOC_ID CULTIVAR YIELD_AVG YIELD_MIN YIELD_MAX  HIAD HIAD_SE PAN_fert
#>   <chr>     <chr>  <chr>        <dbl>     <dbl>     <dbl> <dbl>   <dbl>    <dbl>
#> 1 SDTOS1MA… SDTO   FED2000      8103.     7331.     8931. 0.335 8.90e-3     88.9
#> 2 SDTOS2MA… SDTO   FED2000      4474.     3795.     5244. 0.300 5.65e-4     95.5
#> 3 SDTOS3MA… SDTO   FED2000      5032.     4247.     5951. 0.305 2.95e-3     92.9
#> 4 SDTOS3COL SDTO   FED2000      4935.     4403.     5365. 0.415 1.19e-2     88.5
#> # ℹ 2 more rows
#> # ℹ 11 more variables: PAN_fert_SE <dbl>, GW1000 <dbl>, GW1000_SE <dbl>,
#> #   ST_M2 <dbl>, ST_M2_SE <dbl>, PAN_M2 <dbl>, PAN_M2_SE <dbl>, GT_PAN <dbl>,
#> #   GT_PAN_SE <dbl>, GF_PAN <dbl>, GF_PAN_SE <dbl>
str(yield)
#> tibble [7 × 20] (S3: tbl_df/tbl/data.frame)
#>  $ ID         : chr [1:7] "SDTOS1MADRI" "SDTOS2MADRI" "SDTOS3MADRI" "SDTOS3COL" ...
#>  $ LOC_ID     : chr [1:7] "SDTO" "SDTO" "SDTO" "SDTO" ...
#>  $ CULTIVAR   : chr [1:7] "FED2000" "FED2000" "FED2000" "FED2000" ...
#>  $ YIELD_AVG  : num [1:7] 8103 4474 5032 4935 5542 ...
#>  $ YIELD_MIN  : num [1:7] 7331 3795 4247 4403 4927 ...
#>  $ YIELD_MAX  : num [1:7] 8931 5244 5951 5365 6056 ...
#>  $ HIAD       : num [1:7] 0.335 0.3 0.305 0.415 0.39 ...
#>  $ HIAD_SE    : num [1:7] 0.008903 0.000565 0.002955 0.011861 0.029848 ...
#>  $ PAN_fert   : num [1:7] 88.9 95.5 92.9 88.5 81.3 ...
#>  $ PAN_fert_SE: num [1:7] 0.589 0.393 0.966 1.714 2.313 ...
#>  $ GW1000     : num [1:7] 24.7 23.5 20.8 25.4 24.4 ...
#>  $ GW1000_SE  : num [1:7] 0.0715 0.2074 0.2671 0.484 0.2067 ...
#>  $ ST_M2      : num [1:7] 554 308 372 409 342 ...
#>  $ ST_M2_SE   : num [1:7] 10 4 16 20.8 40.1 ...
#>  $ PAN_M2     : num [1:7] 532 192 316 373 288 ...
#>  $ PAN_M2_SE  : num [1:7] 20 38 20 6.01 29.72 ...
#>  $ GT_PAN     : num [1:7] 69.2 74.7 68.5 68 111.5 ...
#>  $ GT_PAN_SE  : num [1:7] 9.33 20.41 1.11 2.45 15.36 ...
#>  $ GF_PAN     : num [1:7] 62.7 71.5 63 60 90 ...
#>  $ GF_PAN_SE  : num [1:7] 8.85 19.85 1.68 1.37 10.03 ...
```

### SOIL: Datos de Suelo Observados

Un conjunto de datos de suelo observado que proporciona propiedades
químicas y físicas detalladas del suelo de ensayos experimentales. Es
esencial para entender las condiciones ambientales que afectan el
crecimiento y desarrollo del cultivo.

| VAR_NAME | DESCRIPTION | TYPE | UNITS | Calculation method | Agrovoc URL |
|----|----|----|----|----|----|
| ID | Trial ID | character | XXXX |  |  |
| LOC_ID | locality ID | character | name |  |  |
| NL | Number of soil layers (maximum is 10) | numeric | number |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_7799) |
| DEPTH | Thickness of each soil layer | numeric | cm |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_7726) |
| STC | Soil Texture Class (12-USDA) | character | USDA texture class |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_7199) |
| SAND | Soil sand content | numeric | % |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_6779) |
| SILT | Soil silt content | numeric | % |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_331558) |
| CLAY | Soil clay content | numeric | % |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_15619) |
| SBDM | Soil Bulk Density | numeric | g/cm³ |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_7167) |
| SOC | Soil organic carbon | numeric | g/kg | From OM - Walkley-Black | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24fb4269) |
| SLON | Soil Organic Nitrogen | numeric | mg/kg |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_5193) |
| SNH4 | Ammonium, KCl, g elemental N | numeric | mg/kg | (KCl 1M) Espectrom | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_37457) |
| SNO3 | Nitrate, KCl, g elemental N | numeric | mg/kg | (KCl 1M) Espectrom | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_0e5d9815) |
| PH | pH | numeric | number | pH Water 1:1 | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_34901) |
| SCEC | Cation exchange capacity | numeric | cmol/kg | (Amonio Acetato) Volumet | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_24981) |
| WCST | Saturated volumetric water content | numeric | % |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_7208) |
| WCFC | Volumetric water content at field capacity | numeric | % |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_32445) |
| WCWP | Volumetric water content at wilting point | numeric | % |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_32626) |
| WCAD | Volumetric water content at air dryness | numeric | % |  |  |
| SSKS | Saturated hydraulic conductivity | numeric | cm/h |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_25579) |

``` r

# Código de ejemplo para mostrar las primeras filas del conjunto de datos SOIL
# Asumiendo que 'soil' es el conjunto de datos cargado con agroclimR
head(soil)
#> # A tibble: 6 × 21
#>   ID        LOC_ID SAMPLING_DATE    NL DEPTH STC    SAND  SILT  CLAY  SBDM   SOC
#>   <chr>     <chr>  <date>        <dbl> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 SDTOS1MA… SDTO   2013-05-10        1    20 Lo     39.2  45.6  15.2  1.53  6.63
#> 2 SDTOS1MA… SDTO   2013-05-10        2    20 Lo     37.9  46.9  15.2  1.71 11.4 
#> 3 SDTOS1MA… SDTO   2013-05-10        3    20 Lo     40.3  44.5  15.2  1.66  6.52
#> 4 SDTOS1MA… SDTO   2015-08-10        1    20 SaLo   70.9  17.6  11.6  1.40  2.5 
#> # ℹ 2 more rows
#> # ℹ 10 more variables: SLON <dbl>, SNH4 <dbl>, SNO3 <dbl>, PH <dbl>,
#> #   SCEC <dbl>, WCST <dbl>, WCFC <dbl>, WCWP <dbl>, WCAD <dbl>, SSKS <dbl>
str(soil)
#> tibble [18 × 21] (S3: tbl_df/tbl/data.frame)
#>  $ ID           : chr [1:18] "SDTOS1MADRI" "SDTOS1MADRI" "SDTOS1MADRI" "SDTOS1MADRII" ...
#>  $ LOC_ID       : chr [1:18] "SDTO" "SDTO" "SDTO" "SDTO" ...
#>  $ SAMPLING_DATE: Date[1:18], format: "2013-05-10" "2013-05-10" ...
#>  $ NL           : num [1:18] 1 2 3 1 2 3 1 2 3 1 ...
#>  $ DEPTH        : num [1:18] 20 20 20 20 20 20 20 20 20 20 ...
#>  $ STC          : chr [1:18] "Lo" "Lo" "Lo" "SaLo" ...
#>  $ SAND         : num [1:18] 39.2 37.9 40.3 70.9 79.7 ...
#>  $ SILT         : num [1:18] 45.63 46.9 44.5 17.56 8.77 ...
#>  $ CLAY         : num [1:18] 15.2 15.2 15.2 11.6 11.6 ...
#>  $ SBDM         : num [1:18] 1.53 1.71 1.66 1.4 1.49 ...
#>  $ SOC          : num [1:18] 6.63 11.37 6.52 2.5 1.5 ...
#>  $ SLON         : num [1:18] 407 914 382 388 300 ...
#>  $ SNH4         : num [1:18] 5.19 7.95 4.81 6.11 6.11 ...
#>  $ SNO3         : num [1:18] 4.078 10.373 3.605 12.126 0.028 ...
#>  $ PH           : num [1:18] 6.28 5.84 6.5 5.78 6.48 6.51 5.55 6.38 6.34 6.28 ...
#>  $ SCEC         : num [1:18] 12.1 14 12.7 7.95 7.95 ...
#>  $ WCST         : num [1:18] 68.1 60 69.6 45.3 44.6 ...
#>  $ WCFC         : num [1:18] 40.8 35.1 36.4 17.5 16.9 ...
#>  $ WCWP         : num [1:18] 21.75 16.11 17.15 7.36 12.29 ...
#>  $ WCAD         : num [1:18] 8.28 6.16 7.05 8.63 9.28 ...
#>  $ SSKS         : num [1:18] 40.1 32.5 27.9 94.2 98 ...
```

### WTH: Datos Meteorológicos Observados

Un conjunto de datos que captura observaciones meteorológicas diarias.
Incluye datos sobre temperatura, precipitación, radiación solar, humedad
relativa y velocidad del viento, recopilados de estaciones
meteorológicas. Estos datos son vitales para que los modelos de
simulación de cultivos reflejen con precisión el impacto de las
condiciones meteorológicas en el crecimiento y rendimiento de los
cultivos.

| VAR_NAME | DESCRIPTION | TYPE | UNITS | Calculation method | Agrovoc URL |
|----|----|----|----|----|----|
| ID | Trial ID | character | XXXX |  |  |
| LOC_ID | locality ID | character | name |  |  |
| WS_ID | Weather Station ID | numeric | number |  |  |
| DATE | Date | date | MM/DD/YYYY |  |  |
| TMAX | Maximum temperature | numeric | celsius degrees | Davis Vantage Pro2 | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_230) |
| TMIN | Minimum temperature | numeric | celsius degrees | Davis Vantage Pro2 | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_230) |
| RAIN | Rain | numeric | millimeters | Davis Vantage Pro2 | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_6161) |
| SRAD | Solar Radiation | numeric | MJ/m²\*dia | Davis Vantage Pro2 | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_14415) |
| RHUM | Relative humidity | numeric | % | Davis Vantage Pro2 | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_6496) |
| WSPD | Wind Speed | numeric | m/s |  | [Link](https://agrovoc.uniroma2.it/agrovoc/agrovoc/en/page/c_29582) |

``` r

# Código de ejemplo para mostrar las primeras filas del conjunto de datos WTH
# Asumiendo que 'weather' es el conjunto de datos cargado con agroclimR
head(weather)
#> # A tibble: 6 × 9
#>   LOC_ID WS_ID DATE        TMAX  TMIN  RAIN  SRAD  RHUM WSPD 
#>   <chr>  <dbl> <date>     <dbl> <dbl> <dbl> <dbl> <dbl> <lgl>
#> 1 SDTO       1 2013-01-01  36.0  25.7   0    20.9  66.3 NA   
#> 2 SDTO       1 2013-01-02  35.7  25.1   0.2  21.6  73.3 NA   
#> 3 SDTO       1 2013-01-03  38.5  24.0   0    23.1  61.4 NA   
#> 4 SDTO       1 2013-01-04  35.7  22.9   0    21.4  68.9 NA   
#> # ℹ 2 more rows
str(weather)
#> tibble [1,461 × 9] (S3: tbl_df/tbl/data.frame)
#>  $ LOC_ID: chr [1:1461] "SDTO" "SDTO" "SDTO" "SDTO" ...
#>  $ WS_ID : num [1:1461] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ DATE  : Date[1:1461], format: "2013-01-01" "2013-01-02" ...
#>  $ TMAX  : num [1:1461] 36 35.7 38.5 35.7 38.8 ...
#>  $ TMIN  : num [1:1461] 25.7 25.1 24 22.9 23.8 ...
#>  $ RAIN  : num [1:1461] 0 0.2 0 0 0 1.2 0 0 0 0 ...
#>  $ SRAD  : num [1:1461] 20.9 21.6 23.1 21.4 22.9 ...
#>  $ RHUM  : num [1:1461] 66.3 73.3 61.4 68.9 69.5 ...
#>  $ WSPD  : logi [1:1461] NA NA NA NA NA NA ...
```
