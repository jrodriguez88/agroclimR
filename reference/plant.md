# Observed Plant Growth Data

A comprehensive dataset of observed plant growth metrics, including
organ biomass and dry matter samplings. These measurements are integral
for crop modeling and physiology studies, offering insights into the
development and health of different cultivars under varying conditions.

## Usage

``` r
plant
```

## Format

A `data.frame` with 77 rows and 22 columns:

- ID:

  Trial ID - (`character`). A unique identifier for each trial. Example:
  `"LOC1T1PROJ1"`.

- LOC_ID:

  Locality ID - (`character`). Indicates the location of the trial.
  Example: `"LOC1"`.

- CULTIVAR:

  Cultivar name - (`character`). Specifies the plant cultivar used in
  the trial. Example: `"CULTIVAR1"`.

- SAMPLING_DATE:

  Sampling date - (`date`). The date when the plant sampling was
  conducted, in MM/DD/YYYY format.

- LAI_OBS:

  Leaf Area Index observed - (`numeric`). Measures the area of leaves
  per unit area of ground, in m² leaf/m² ground.

- LAI_SE:

  Leaf Area Index standard deviation - (`numeric`). The standard
  deviation of the Leaf Area Index, in m² leaf/m² ground.

- WLVG_OBS:

  Green leaf dry weight - (`numeric`). The dry weight of green leaves,
  in kg/ha.

- WLVG_SE:

  Green leaf dry weight standard error - (`numeric`). The standard error
  of green leaf dry weight, in kg/ha.

- WLVD_OBS:

  Dead leaf dry weight - (`numeric`). The dry weight of dead leaves, in
  kg/ha.

- WLVD_SE:

  Dead leaf dry weight standard error - (`numeric`). The standard error
  of dead leaf dry weight, in kg/ha.

- WST_OBS:

  Stem dry weight - (`numeric`). The dry weight of stems, in kg/ha.

- WST_SE:

  Stem dry weight standard error - (`numeric`). The standard error of
  stem dry weight, in kg/ha.

- WSO_OBS:

  Panicle dry weight - (`numeric`). The dry weight of panicles, in
  kg/ha.

- WSO_SE:

  Panicle dry weight standard error - (`numeric`). The standard error of
  panicle dry weight, in kg/ha.

- WAGT_OBS:

  Total dry weight - (`numeric`). The total plant dry weight, including
  all sampled organs, in kg/ha.

- WAGT_SE:

  Total dry weight standard error - (`numeric`). The standard error of
  total dry weight, in kg/ha.

- NLV_OBS:

  Number of green leaves - (`numeric`). The number of green leaves per
  square meter, in number/m².

- NLV_SE:

  Number of green leaves standard error - (`numeric`). The standard
  error of the number of green leaves, in number/m².

- NST_OBS:

  Number of stems - (`numeric`). The number of stems per square meter,
  in number/m².

- NST_SE:

  Number of stems standard error - (`numeric`). The standard error of
  the number of stems, in number/m².

- NP_OBS:

  Number of panicles - (`numeric`). The number of panicles per square
  meter, in number/m².

- NP_SE:

  Number of panicles standard error - (`numeric`). The standard error of
  the number of panicles, in number/m².

## Source

`Rodriguez-Espinoza J, (2024)` and `CIAT-MADR-FEDEARROZ, (2016)`

## Details

Observed Plant Growth Data

This dataset contains observed plant growth data, including organ
biomass or dry matter samplings, crucial for modeling and crop
physiology studies. It provides a detailed view of plant growth dynamics
across various trials, facilitating the analysis of crop development,
yield potential, and the effects of agronomic treatments on plant
growth.

## Examples

``` r
# Assuming `plant` is your dataset name
summary(plant)
#>       ID               LOC_ID            CULTIVAR         SAMPLING_DATE       
#>  Length:77          Length:77          Length:77          Min.   :2013-05-21  
#>  Class :character   Class :character   Class :character   1st Qu.:2014-02-13  
#>  Mode  :character   Mode  :character   Mode  :character   Median :2014-05-29  
#>                                                           Mean   :2014-09-27  
#>                                                           3rd Qu.:2015-07-23  
#>                                                           Max.   :2016-02-26  
#>                                                                               
#>     WLVG_OBS         WLVG_SE         LAI_OBS          LAI_SE      
#>  Min.   :  68.0   Min.   :  4.0   Min.   :0.200   Min.   :0.0000  
#>  1st Qu.: 683.6   1st Qu.: 79.2   1st Qu.:1.175   1st Qu.:0.1000  
#>  Median :1665.3   Median :122.4   Median :2.200   Median :0.2000  
#>  Mean   :1713.3   Mean   :152.6   Mean   :2.506   Mean   :0.2365  
#>  3rd Qu.:2512.0   3rd Qu.:219.0   3rd Qu.:3.500   3rd Qu.:0.3250  
#>  Max.   :4261.5   Max.   :523.0   Max.   :6.200   Max.   :0.8000  
#>                                   NA's   :25      NA's   :25      
#>     WLVD_OBS         WLVD_SE          WST_OBS            WST_SE      
#>  Min.   :   0.0   Min.   :  0.00   Min.   :   80.5   Min.   :   3.6  
#>  1st Qu.:  71.3   1st Qu.: 16.40   1st Qu.: 1554.2   1st Qu.: 122.4  
#>  Median : 268.0   Median : 58.00   Median : 4400.5   Median : 236.8  
#>  Mean   : 630.8   Mean   : 86.47   Mean   : 3894.7   Mean   : 365.9  
#>  3rd Qu.:1059.2   3rd Qu.:113.60   3rd Qu.: 5421.8   3rd Qu.: 430.4  
#>  Max.   :2761.2   Max.   :468.00   Max.   :10214.0   Max.   :2927.2  
#>                                                                      
#>     WSO_OBS         WSO_SE          WAGT_OBS          WAGT_SE      
#>  Min.   :   0   Min.   :   0.0   Min.   :  148.5   Min.   :  11.2  
#>  1st Qu.:   0   1st Qu.:   0.0   1st Qu.: 2909.0   1st Qu.: 171.2  
#>  Median :   0   Median :   0.0   Median : 9106.9   Median : 478.0  
#>  Mean   :2139   Mean   : 190.1   Mean   : 8377.4   Mean   : 597.1  
#>  3rd Qu.:4507   3rd Qu.: 295.2   3rd Qu.:13291.2   3rd Qu.: 875.2  
#>  Max.   :8663   Max.   :1361.0   Max.   :24570.7   Max.   :3142.8  
#>                                                                    
#>  NLV_OBS         NLV_SE        NST_OBS         NST_SE         NP_OBS       
#>  Mode:logical   Mode:logical   Mode:logical   Mode:logical   Mode:logical  
#>  NA's:77        NA's:77        NA's:77        NA's:77        NA's:77       
#>                                                                            
#>                                                                            
#>                                                                            
#>                                                                            
#>                                                                            
#>   NP_SE        
#>  Mode:logical  
#>  NA's:77       
#>                
#>                
#>                
#>                
#>                
```
