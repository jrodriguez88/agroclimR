# Analisis Exploratorio de Datos con agroclimR

``` r

library(agroclimR)
#> Warning: no DISPLAY variable so Tk is not available
```

``` r

# Código de ejemplo para crear lista de conjunto de datos Localidad-Cultivar
# Asumiendo que agroclimR esta cargado
obs_data  = list(
  list(AGRO_man = agro, 
       FERT_obs = fertil,
       PHEN_obs = phenol,
       PLANT_gro = plant,
       YIELD_obs = yield,
       SOIL_obs = soil,
       WTH_obs = weather))

# con la lista de trabajo es posible acceder a un conjunto de herramientas para manipular y analizar datos de modelacion
yield_data <- extract_obs_var(obs_data, "yield")
#head(yield_data)

# Cargamos ggplot y visualizamos
library(ggplot2)
plot <- agroclimR:::plot_yield_obs(yield_data) 
#> Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
#> ℹ Please use the `linewidth` argument instead.
#> ℹ The deprecated feature was likely used in the agroclimR package.
#>   Please report the issue to the authors.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.

plotly::ggplotly(plot)
```

Podemos obtener el mismo resultado si exportamos los datos desde el
libro de trabajo propuesto en este articulo.

``` r


# File name
file_name = c("agroclimR_workbook.xlsx")

# Files directory
path_file = system.file("extdata", file_name, package = "agroclimR")

# A Partir de un libro de datos 
obs_data2 = list(agroclimR::read_agroclimr_data(path_file))


yield_data2 <- extract_obs_var(obs_data2, "yield")

plot2 <- agroclimR:::plot_yield_obs(yield_data2) + coord_flip()

plotly::ggplotly(plot2)
```
