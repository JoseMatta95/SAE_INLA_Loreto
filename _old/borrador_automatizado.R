library(purrr)

ys <- c("hv109NE_cor_logit", "hv109PE_cor_logit", "hv109SE_cor_logit", "hv109SuE_cor_logit")
ses <- c("se_hv109NE_cor", "se_hv109PE_cor", "se_hv109SE_cor", "se_hv109SuE_cor")

modelos_fh <- map2(ys, ses, ~{
  inla(
    formula = as.formula(paste(.x, "~ 1 + pc1 + pc2 + pc3 + pc4 + pc5 +",
                               "f(id.sp, model = 'bym', graph = w.loreto) +",
                               "f(year, model = 'ar1')")),
    data = edu_censo_final,
    family = "gaussian",
    control.family = list(hyper = list(prec = list(fixed = TRUE))),
    scale = edu_censo_final[[.y]],
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, cpo = TRUE)
  )
})

# nombrando cada objeto del map
names(modelos_fh) <- ys

#
walk2(modelos_fh, names(modelos_fh), ~{
  col_name <- paste0("fit_", .y)
  edu_censo_final[[col_name]] <<- .x$summary.fitted.values$mean
})

edu_censo_final %>% 
  select(year,ubigeo,fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit) %>% 
  mutate(
    across(.cols = fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit, .f = ~plogis(.x)),
    
    total = rowSums(across(fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit)),
    
    across(
      fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit,
      ~ .x / total
    )
  ) %>% 
  
  mutate(
    total2 = rowSums(across(fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit))
  )

######

library(purrr)
library(dplyr)

# Definir las variables de interés
ys <- c("hv109NE_cor_logit", "hv109PE_cor_logit", "hv109SE_cor_logit", "hv109SuE_cor_logit")
ses <- c("se_hv109NE_cor", "se_hv109PE_cor", "se_hv109SE_cor", "se_hv109SuE_cor")

# Ajustar los modelos con map2
modelos_fh <- map2(ys, ses, ~{
  inla(
    formula = as.formula(paste(.x, "~ 1 + pc1 + pc2 + pc3 + pc4 + pc5 +",
                               "f(id.sp, model = 'bym', graph = w.loreto) +",
                               "f(year, model = 'ar1')")),
    data = edu_censo_final,
    family = "gaussian",
    control.family = list(hyper = list(prec = list(fixed = TRUE))),
    scale = edu_censo_final[[.y]], # Usando la SE correspondiente como peso
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, cpo = TRUE)
  )
})

# Nombrar cada objeto del map
names(modelos_fh) <- ys

# Extraer las predicciones, errores estándar y calcular intervalos de credibilidad
walk2(modelos_fh, names(modelos_fh), ~{
  col_name <- paste0("fit_", .y)
  edu_censo_final[[col_name]] <<- .x$summary.fitted.values$mean  # Predicciones (fit)
  
  # Intervalos de credibilidad: 2.5% y 97.5% percentiles en escala logit
  lower_col_name <- paste0("lower_", .y)
  upper_col_name <- paste0("upper_", .y)
  
  edu_censo_final[[lower_col_name]] <<- .x$summary.fitted.values$`0.025quant`  # Límite inferior del intervalo de credibilidad
  edu_censo_final[[upper_col_name]] <<- .x$summary.fitted.values$`0.975quant`  # Límite superior del intervalo de credibilidad
  
  # Transformación logit -> probabilidad para los intervalos de credibilidad
  edu_censo_final[[lower_col_name]] <<- plogis(edu_censo_final[[lower_col_name]])  # Límite inferior transformado a probabilidad
  edu_censo_final[[upper_col_name]] <<- plogis(edu_censo_final[[upper_col_name]])  # Límite superior transformado a probabilidad
  
  # Calcular el SE transformado (logit -> probabilidad)
  se_logit <- .x$summary.fitted.values$sd  # Desviación estándar en la escala logit
  mean_logit <- .x$summary.fitted.values$mean  # Predicción en la escala logit
  
  # Transformación delta para obtener el SE en la escala de proporciones (probabilidad)
  mean_prob <- plogis(mean_logit)  # Transformar la media logit a probabilidad
  se_prob <- sqrt((mean_prob * (1 - mean_prob))^2 * se_logit^2)  # Propagación del error
  
  # Nombrar y agregar el SE transformado al dataframe
  col_name_se <- paste0("se_", .y)
  edu_censo_final[[col_name_se]] <<- se_prob  # Agregar SE en la escala de probabilidad
})

# Realizar la transformación y cálculos adicionales, incluyendo los SE y los intervalos de credibilidad
edu_censo_final %>% 
  select(year, ubigeo, 
         fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit,
         lower_hv109NE_cor_logit:upper_hv109SuE_cor_logit,  # Incluir intervalos de credibilidad en probabilidad
         se_hv109NE_cor_logit:se_hv109SuE_cor_logit) %>%  # Incluir las columnas de SE
  mutate(
    # Transformar las estimaciones logit a probabilidad
    across(.cols = fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit, .f = ~plogis(.x)),
    
    # Sumar las probabilidades para calcular el total
    total = rowSums(across(fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit)),
    
    # Normalizar las probabilidades para que sumen a 1
    across(
      fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit,
      ~ .x / total
    )
  ) %>% 
  mutate(
    total2 = rowSums(across(fit_hv109NE_cor_logit:fit_hv109SuE_cor_logit))) %>% view()

  ggplot(aes(x = fit_hv109PE_cor_logit, y = ubigeo)) +
  geom_point(color = "blue") +  # Puntos para la predicción
  geom_segment(aes(x = lower_hv109PE_cor_logit, xend = upper_hv109PE_cor_logit, y = ubigeo, yend = ubigeo),
               color = "red", size = 1) +  # Línea para el IC (intervalo de credibilidad)
  labs(x = "Predicción HV109NE", y = "Ubigeo", title = "Predicciones con IC por Ubigeo") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) +
  facet_wrap(~year)

