# Import Experimental Data from agroclimR Data Workbook

`import_exp_data()` performs extraction of experimental data from the
agroclimR data workbook, including location, soil, climate data, and
observed data for phenology, leaf area index, dry matter, and yield.

Reads data from agroclimR workbook sheets.

## Usage

``` r
import_exp_data(files, model = "oryza")

read_agroclimr_data(workbook_name)
```

## Arguments

- files:

  Vector of strings with workbook names (.xls extension file included).

- model:

  String indicating the model name, default "oryza",options: ("dssat",
  "aquacrop").

- workbook_name:

  String with the workbook name (full name).

## Value

List containing raw and observed data by component ("data", "soil",
"wth", "phen", "lai", "dry_matter", "yield").

agroclimR list. A List of data frames for each sheet in the workbook.

## Examples

``` r
#' # File names vector, extension include
name_file = c("agroclimR_workbook.xlsx")

# Files directory
test_file = system.file("extdata", name_file, package = "agroclimR")

# Import data to R lists and tibble formats
obs_data = import_exp_data(test_file, model = "oryza")

head(obs_data)
#> $data
#> # A tibble: 1 × 2
#>   file                                                              input_data  
#>   <chr>                                                             <list>      
#> 1 /home/runner/work/_temp/Library/agroclimR/extdata/agroclimR_work… <named list>
#> 
#> $soil
#> # A tibble: 1 × 5
#>   site  soil                lat   lon  elev
#>   <chr> <list>            <dbl> <dbl> <dbl>
#> 1 SDTO  <tibble [3 × 20]>  3.91 -75.0   415
#> 
#> $wth
#> # A tibble: 1 × 6
#>   site  ws_id wth                    lat   lon  elev
#>   <chr> <dbl> <list>               <dbl> <dbl> <dbl>
#> 1 SDTO      1 <tibble [1,461 × 7]>  3.91 -75.0   415
#> 
#> $phen
#> # A tibble: 21 × 4
#>    exp_file               data             var   value
#>    <chr>                  <list>           <chr> <dbl>
#>  1 SDTO_FED2000_MADRI_S1  <tibble [5 × 2]> IDAT     51
#>  2 SDTO_FED2000_MADRI_S1  <tibble [5 × 2]> FDAT     81
#>  3 SDTO_FED2000_MADRI_S1  <tibble [5 × 2]> MDAT    104
#>  4 SDTO_FED2000_MADRII_S1 <tibble [5 × 2]> IDAT     48
#>  5 SDTO_FED2000_MADRII_S1 <tibble [5 × 2]> FDAT     85
#>  6 SDTO_FED2000_MADRII_S1 <tibble [5 × 2]> MDAT    115
#>  7 SDTO_FED2000_MADRI_S2  <tibble [5 × 2]> IDAT     49
#>  8 SDTO_FED2000_MADRI_S2  <tibble [5 × 2]> FDAT     85
#>  9 SDTO_FED2000_MADRI_S2  <tibble [5 × 2]> MDAT    105
#> 10 SDTO_FED2000_MADRII_S2 <tibble [5 × 2]> IDAT     40
#> # ℹ 11 more rows
#> 
#> $lai
#> # A tibble: 77 × 5
#>    exp_file              date       var   value    se
#>    <chr>                 <date>     <chr> <dbl> <dbl>
#>  1 SDTO_FED2000_MADRI_S1 2013-05-21 LAI      NA    NA
#>  2 SDTO_FED2000_MADRI_S1 2013-05-28 LAI      NA    NA
#>  3 SDTO_FED2000_MADRI_S1 2013-06-07 LAI      NA    NA
#>  4 SDTO_FED2000_MADRI_S1 2013-06-12 LAI      NA    NA
#>  5 SDTO_FED2000_MADRI_S1 2013-06-18 LAI      NA    NA
#>  6 SDTO_FED2000_MADRI_S1 2013-06-25 LAI      NA    NA
#>  7 SDTO_FED2000_MADRI_S1 2013-07-07 LAI      NA    NA
#>  8 SDTO_FED2000_MADRI_S1 2013-07-10 LAI      NA    NA
#>  9 SDTO_FED2000_MADRI_S1 2013-07-17 LAI      NA    NA
#> 10 SDTO_FED2000_MADRI_S1 2013-07-22 LAI      NA    NA
#> # ℹ 67 more rows
#> 
#> $dry_matter
#> # A tibble: 385 × 5
#>    exp_file              date       var   value    se
#>    <chr>                 <date>     <chr> <dbl> <dbl>
#>  1 SDTO_FED2000_MADRI_S1 2013-05-21 WAGT   610.  37.2
#>  2 SDTO_FED2000_MADRI_S1 2013-05-21 WLVD     0    0  
#>  3 SDTO_FED2000_MADRI_S1 2013-05-21 WLVG   316.  20.4
#>  4 SDTO_FED2000_MADRI_S1 2013-05-21 WSO      0    0  
#>  5 SDTO_FED2000_MADRI_S1 2013-05-21 WST    294.  20  
#>  6 SDTO_FED2000_MADRI_S1 2013-05-28 WAGT  1188  254  
#>  7 SDTO_FED2000_MADRI_S1 2013-05-28 WLVD     0    0  
#>  8 SDTO_FED2000_MADRI_S1 2013-05-28 WLVG   544. 122. 
#>  9 SDTO_FED2000_MADRI_S1 2013-05-28 WSO      0    0  
#> 10 SDTO_FED2000_MADRI_S1 2013-05-28 WST    644. 132. 
#> # ℹ 375 more rows
#> 
#' # File names vector, extension include
name_file = c("agroclimR_workbook.xlsx")

# Files directory
test_file = system.file("extdata", name_file, package = "agroclimR")

# Import data to R lists and tibble formats
agroclimr_list = read_agroclimr_data(test_file)
agroclimr_list
#> $AGRO_man
#> # A tibble: 7 × 14
#>   ID        LOC_ID PROJECT CULTIVAR TR_N    LAT  LONG   ALT PDAT  CROP_SYS ESTAB
#>   <chr>     <chr>  <chr>   <chr>    <chr> <dbl> <dbl> <dbl> <chr> <chr>    <chr>
#> 1 SDTOS1MA… SDTO   MADRI   FED2000  S1     3.91 -75.0   415 2013… IRRIGAT… DIRE…
#> 2 SDTOS1MA… SDTO   MADRII  FED2000  S1     3.91 -75.0   415 2015… IRRIGAT… DIRE…
#> 3 SDTOS2MA… SDTO   MADRI   FED2000  S2     3.91 -75.0   415 2013… IRRIGAT… DIRE…
#> 4 SDTOS2MA… SDTO   MADRII  FED2000  S2     3.91 -75.0   415 2015… IRRIGAT… DIRE…
#> 5 SDTOS3COL SDTO   COL     FED2000  S3     3.91 -75.0   415 2014… IRRIGAT… DIRE…
#> 6 SDTOS3MA… SDTO   MADRI   FED2000  S3     3.91 -75.0   415 2014… IRRIGAT… DIRE…
#> 7 SDTOS4COL SDTO   COL     FED2000  S4     3.91 -75.0   415 2015… IRRIGAT… DIRE…
#> # ℹ 3 more variables: NPLDS <dbl>, SBDUR <lgl>, TRDAT <lgl>
#> 
#> $PLANT_obs
#> # A tibble: 77 × 22
#>    ID        LOC_ID CULTIVAR SAMPLING_DATE       WLVG_OBS WLVG_SE LAI_OBS LAI_SE
#>    <chr>     <chr>  <chr>    <dttm>                 <dbl>   <dbl>   <dbl>  <dbl>
#>  1 SDTOS1MA… SDTO   FED2000  2013-05-21 00:00:00     316.    20.4      NA     NA
#>  2 SDTOS1MA… SDTO   FED2000  2013-05-28 00:00:00     544.   122.       NA     NA
#>  3 SDTOS1MA… SDTO   FED2000  2013-06-07 00:00:00     752.   168.       NA     NA
#>  4 SDTOS1MA… SDTO   FED2000  2013-06-12 00:00:00     816.   169.       NA     NA
#>  5 SDTOS1MA… SDTO   FED2000  2013-06-18 00:00:00    1495.   250.       NA     NA
#>  6 SDTOS1MA… SDTO   FED2000  2013-06-25 00:00:00    1665.   387.       NA     NA
#>  7 SDTOS1MA… SDTO   FED2000  2013-07-07 00:00:00    2740.   400.       NA     NA
#>  8 SDTOS1MA… SDTO   FED2000  2013-07-10 00:00:00    3476.   270        NA     NA
#>  9 SDTOS1MA… SDTO   FED2000  2013-07-17 00:00:00    3985.   129.       NA     NA
#> 10 SDTOS1MA… SDTO   FED2000  2013-07-22 00:00:00    3419.    48.8      NA     NA
#> # ℹ 67 more rows
#> # ℹ 14 more variables: WLVD_OBS <dbl>, WLVD_SE <dbl>, WST_OBS <dbl>,
#> #   WST_SE <dbl>, WSO_OBS <dbl>, WSO_SE <dbl>, WAGT_OBS <dbl>, WAGT_SE <dbl>,
#> #   NLV_OBS <lgl>, NLV_SE <lgl>, NST_OBS <lgl>, NST_SE <lgl>, NP_OBS <lgl>,
#> #   NP_SE <lgl>
#> 
#> $FERT_obs
#> # A tibble: 37 × 7
#>    ID           LOC_ID FERT_No   DDE     N     P     K
#>    <chr>        <chr>    <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 SDTOS1MADRI  SDTO         1    10  42    21.5    33
#>  2 SDTOS1MADRI  SDTO         2    23  58     1.5    33
#>  3 SDTOS1MADRI  SDTO         3    38  56.5   0      30
#>  4 SDTOS1MADRI  SDTO         4    52  51.2   0      15
#>  5 SDTOS1MADRI  SDTO         5    64  18.4   0       0
#>  6 SDTOS1MADRII SDTO         1     9  42    21.5    48
#>  7 SDTOS1MADRII SDTO         2    23  58    31.5    33
#>  8 SDTOS1MADRII SDTO         3    37  56.5   0      30
#>  9 SDTOS1MADRII SDTO         4    50  51.2   0      15
#> 10 SDTOS1MADRII SDTO         5    62  36.8   0       0
#> # ℹ 27 more rows
#> 
#> $PHEN_obs
#> # A tibble: 7 × 8
#>   ID           LOC_ID CULTIVAR PDAT                EDAT               
#>   <chr>        <chr>  <chr>    <dttm>              <dttm>             
#> 1 SDTOS1MADRI  SDTO   FED2000  2013-04-29 00:00:00 2013-05-08 00:00:00
#> 2 SDTOS1MADRII SDTO   FED2000  2015-06-05 00:00:00 2015-06-14 00:00:00
#> 3 SDTOS2MADRI  SDTO   FED2000  2013-12-05 00:00:00 2013-12-12 00:00:00
#> 4 SDTOS2MADRII SDTO   FED2000  2015-11-03 00:00:00 2015-11-12 00:00:00
#> 5 SDTOS3COL    SDTO   FED2000  2014-10-07 00:00:00 2014-10-16 00:00:00
#> 6 SDTOS3MADRI  SDTO   FED2000  2014-02-05 00:00:00 2014-02-11 00:00:00
#> 7 SDTOS4COL    SDTO   FED2000  2015-02-20 00:00:00 2015-03-03 00:00:00
#> # ℹ 3 more variables: IDAT <dttm>, FDAT <dttm>, MDAT <dttm>
#> 
#> $YIELD_obs
#> # A tibble: 7 × 20
#>   ID        LOC_ID CULTIVAR YIELD_AVG YIELD_MIN YIELD_MAX  HIAD HIAD_SE PAN_fert
#>   <chr>     <chr>  <chr>        <dbl>     <dbl>     <dbl> <dbl>   <dbl>    <dbl>
#> 1 SDTOS1MA… SDTO   FED2000      8103.     7331.     8931. 0.335 8.90e-3     88.9
#> 2 SDTOS2MA… SDTO   FED2000      4474.     3795.     5244. 0.300 5.65e-4     95.5
#> 3 SDTOS3MA… SDTO   FED2000      5032.     4247.     5951. 0.305 2.95e-3     92.9
#> 4 SDTOS3COL SDTO   FED2000      4935.     4403.     5365. 0.415 1.19e-2     88.5
#> 5 SDTOS4COL SDTO   FED2000      5542.     4927.     6056. 0.390 2.98e-2     81.3
#> 6 SDTOS1MA… SDTO   FED2000      5430.     5120.     5719. 0.405 7.22e-3     86.1
#> 7 SDTOS2MA… SDTO   FED2000      5114.     4676.     5550. 0.356 2.01e-2     88.4
#> # ℹ 11 more variables: PAN_fert_SE <dbl>, GW1000 <dbl>, GW1000_SE <dbl>,
#> #   ST_M2 <dbl>, ST_M2_SE <dbl>, PAN_M2 <dbl>, PAN_M2_SE <dbl>, GT_PAN <dbl>,
#> #   GT_PAN_SE <dbl>, GF_PAN <dbl>, GF_PAN_SE <dbl>
#> 
#> $WTH_obs
#> # A tibble: 1,461 × 9
#>    LOC_ID WS_ID DATE                 TMAX  TMIN  RAIN  SRAD  RHUM WSPD 
#>    <chr>  <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <lgl>
#>  1 SDTO       1 2013-01-01 00:00:00  36.0  25.7   0    20.9  66.3 NA   
#>  2 SDTO       1 2013-01-02 00:00:00  35.7  25.1   0.2  21.6  73.3 NA   
#>  3 SDTO       1 2013-01-03 00:00:00  38.5  24.0   0    23.1  61.4 NA   
#>  4 SDTO       1 2013-01-04 00:00:00  35.7  22.9   0    21.4  68.9 NA   
#>  5 SDTO       1 2013-01-05 00:00:00  38.8  23.8   0    22.9  69.5 NA   
#>  6 SDTO       1 2013-01-06 00:00:00  37.2  22.7   1.2  24.2  65.9 NA   
#>  7 SDTO       1 2013-01-07 00:00:00  38.4  23.7   0    22.9  60.3 NA   
#>  8 SDTO       1 2013-01-08 00:00:00  34.7  22.5   0    22.1  74.3 NA   
#>  9 SDTO       1 2013-01-09 00:00:00  38.9  23.9   0    22.7  72.1 NA   
#> 10 SDTO       1 2013-01-10 00:00:00  38.9  24.2   0    22.8  72.2 NA   
#> # ℹ 1,451 more rows
#> 
#> $SOIL_obs
#> # A tibble: 18 × 21
#>    ID       LOC_ID SAMPLING_DATE          NL DEPTH STC    SAND  SILT  CLAY  SBDM
#>    <chr>    <chr>  <dttm>              <dbl> <dbl> <chr> <dbl> <dbl> <dbl> <dbl>
#>  1 SDTOS1M… SDTO   2013-05-10 00:00:00     1    20 Lo     39.2 45.6  15.2   1.53
#>  2 SDTOS1M… SDTO   2013-05-10 00:00:00     2    20 Lo     37.9 46.9  15.2   1.71
#>  3 SDTOS1M… SDTO   2013-05-10 00:00:00     3    20 Lo     40.3 44.5  15.2   1.66
#>  4 SDTOS1M… SDTO   2015-08-10 00:00:00     1    20 SaLo   70.9 17.6  11.6   1.40
#>  5 SDTOS1M… SDTO   2015-08-10 00:00:00     2    20 SaLo   79.7  8.77 11.6   1.49
#>  6 SDTOS1M… SDTO   2015-08-10 00:00:00     3    20 SaLo   77.7  7.83 14.4   1.46
#>  7 SDTOS2M… SDTO   2014-04-10 00:00:00     1    20 Lo     41.3 45.3  13.4   1.79
#>  8 SDTOS2M… SDTO   2014-04-10 00:00:00     2    20 Lo     47.6 39.0  13.4   1.76
#>  9 SDTOS2M… SDTO   2014-04-10 00:00:00     3    20 SaLo   77.9  8.78 13.3   1.59
#> 10 SDTOS2M… SDTO   2015-12-23 00:00:00     1    20 Lo     42.6 39.1  18.3   1.56
#> 11 SDTOS2M… SDTO   2015-12-23 00:00:00     2    20 Lo     47.7 37.9  14.5   1.69
#> 12 SDTOS2M… SDTO   2015-12-23 00:00:00     3    20 Lo     43.9 40.4  15.7   1.70
#> 13 SDTOS3M… SDTO   2014-02-26 00:00:00     1    20 SiLo   28.3 56.0  15.7   1.59
#> 14 SDTOS3M… SDTO   2014-02-26 00:00:00     2    20 Lo     45.0 36.9  18.1   1.80
#> 15 SDTOS3M… SDTO   2014-02-26 00:00:00     3    20 SaLo   81.4  2.96 15.6   1.54
#> 16 SDTOS4M… SDTO   2014-07-24 00:00:00     1    20 Lo     50.3 40.7   8.96  1.45
#> 17 SDTOS4M… SDTO   2014-07-24 00:00:00     2    20 SaLo   81.9  5.43 12.7   1.53
#> 18 SDTOS4M… SDTO   2014-07-24 00:00:00     3    20 SaLo   83.1  2.93 13.9   1.47
#> # ℹ 11 more variables: SOC <dbl>, SLON <dbl>, SNH4 <dbl>, SNO3 <dbl>, PH <dbl>,
#> #   SCEC <dbl>, WCST <dbl>, WCFC <dbl>, WCWP <dbl>, WCAD <dbl>, SSKS <dbl>
#> 
#> $Metadata
#> # A tibble: 85 × 7
#>    COMPONENT VAR_NAME DESCRIPTION     TYPE  UNITS Calculation method /…¹ Agrovoc
#>    <chr>     <chr>    <chr>           <chr> <chr> <chr>                  <chr>  
#>  1 AGRO      ID       Trial ID        char… XXXX  NA                     NA     
#>  2 AGRO      LOC_ID   locality ID     char… name  NA                     NA     
#>  3 AGRO      PROJECT  Project ID      char… name  NA                     NA     
#>  4 AGRO      CULTIVAR Cultivar name   char… name  NA                     NA     
#>  5 AGRO      TR_N     Treatment numb… char… cn    NA                     NA     
#>  6 AGRO      LAT      Latitude        nume… Deci… GPS, G-EARTH           http:/…
#>  7 AGRO      LONG     Longitude       nume… Deci… GPS, G-EARTH           http:/…
#>  8 AGRO      ALT      Elevation(m ab… nume… mete… GPS, G-EARTH           http:/…
#>  9 AGRO      PDAT     Planting date   date  MM/D… Field Manual           http:/…
#> 10 AGRO      CROP_SYS Crop system     char… IRRI… Field Manual           NA     
#> # ℹ 75 more rows
#> # ℹ abbreviated name: ¹​`Calculation method / Observation`
#> 
```
