cal_canopycover_params <-
function(wth_data, phen_data, lai_data, tbase = 8, CCo = 3) {
    # Ajustar curva polinómica de segundo grado


  PDAT <- phen_data %>% filter(var == "PDAT") %>% pull(value)
  EDAT <- phen_data %>% filter(var == "EDAT") %>% pull(value)
  IDAT <- phen_data %>% filter(var == "IDAT") %>% pull(value)
  FDAT <- phen_data %>% filter(var == "FDAT") %>% pull(value)
  MDAT <- phen_data %>% filter(var == "MDAT") %>% pull(value)


  test <- wth_data %>%
    dplyr::filter(date >= PDAT,
                  date <= MDAT ) %>% mutate(HUH = ((tmax + tmin)/2) - tbase) %>%
    dplyr::select(-c(tmax:rhum)) %>%
    mutate(gdd = cumsum(HUH)) %>% left_join(lai_data, by = join_by(date)) %>%
    dplyr::select(date, HUH, gdd, lai = value, se) %>% na.omit()
    #mutate(lai = case_when(date == PDAT ~ 0,
    #                       TRUE ~ lai ),
    #       se = case_when(date == PDAT ~ 0,
    #                      TRUE ~ se ))

  # canopy cover data.frame
  # Convert CC = 1 - exp(-k*LAI))

  if(mean(test$lai)>20){

    df <- test %>% mutate(canopy = lai/100)

  } else {

  df <- test %>%
    mutate(
      k = case_when(
        date <= IDAT ~ 0.4,
        date >= FDAT ~ 0.6,
        TRUE ~ 0.5),
      canopy = (1 - exp(-k*lai)))}



    max_obs <- df$gdd[which.max(df$canopy)]

    modelo <- lm(canopy ~ poly(gdd, 2), data = df)

    # Extraer valor máximo y punto de inflexión del modelo ajustado
    x_vals <- seq(min(df$gdd), max(df$gdd), length.out = 1000)
    y_vals <- predict(modelo, newdata = data.frame(gdd = x_vals))
    maximo <- max(y_vals)
    inflexion <- x_vals[which.max(y_vals)]

    # Calcular pendiente de la fase de crecimiento
    p1 <- coef(lm(canopy ~ gdd, data = subset(df, gdd <= inflexion)))
    growing_slope <- p1[2]

    # Calcular pendiente de la fase de senescencia
    p2 <- coef(lm(canopy ~ gdd, data = subset(df, gdd >= max_obs)))
    senescence_slope <- p2[2]

    # Crear gráfico
    plot(df$gdd, df$canopy, pch = 16, xlab = "Grados dias de crecimiento", ylab = "Porcentaje Cobertura de canopy")
    lines(x_vals, y_vals)
    points(inflexion, maximo, col = "green")
    abline(p1, col = "blue", lty = 2)
    abline(p2, col = "red", lty = 2)

    # Retornar resultados
    return(data.frame(CCx = maximo, GDD_senescence = inflexion, CGC = growing_slope*10,
                CDC = -senescence_slope*10))
  }
