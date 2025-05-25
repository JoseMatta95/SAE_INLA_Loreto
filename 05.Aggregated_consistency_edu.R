library(tidyverse)
library(survey)
library(janitor)
library(sf)
library(viridis)

# data

wi_edu_2010_2020<- read.csv("./data/wi_edu_2010_2020.csv")

resultados_SAE_loreto_edu <- read.csv("./data/final_data/resultados_SAE_loreto_edu.csv") %>% 
  mutate(ubigeo = as.character(ubigeo),
         ubigeo_prov = substr(ubigeo, 1, 4))

pop_dist <- read.csv("./data/aux_data/malaria_dist_total.csv") %>% 
  select(year,iddist,pop_landscan) %>% 
  rename(ubigeo = iddist) %>% 
  mutate(
    ubigeo = as.character(ubigeo)
  )

# Aggregated consistency check - EDU ----

# 1. Estimates will be calculated for each year for the whole Loreto department and provinces, 
# 2. The district-level estimates by year will be aggregated to the department-year and province-year levels,
# 3. It will be verified if the direct estimate is on the confidence interval (CI) of the SAE-INLA model estimates.

# The logic is that the true value is expected to lie within the CI of the model/design-based estimate.
# ENDES assumes that the true value lies within its own CI. For the SAE-INLA models, we would expect 
# ENDES estimates to fall within the model-generated CIs.

## Departament: Loreto----

# 1. Estimates will be calculated for each year for the whole Loreto department and provinces

data_final_nest<-
  wi_edu_2010_2020 %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    svydesign_endes= map(.x=data,
                         .f = ~svydesign(ids = ~hv001, strata = ~hv022, 
                                         weights = ~hv005, data = .x, nest = T)),
    
    wi_dep = map(.x = svydesign_endes,
                 .f = ~svyby(~hv270_recat, ~hv023, design = .x, svymean, keep.var = T)),
    
    edu_dep = map(.x = svydesign_endes,
                  .f = ~svyby(~hv109_recat, ~hv023, design = .x, svymean, keep.var = T)),
    
    
    wi_prov = map(.x = svydesign_endes,
                  .f = ~svyby(~hv270_recat, ~ubigeo_prov, design = .x, svymean, keep.var = T)),
    
    edu_prov = map(.x = svydesign_endes,
                  .f = ~svyby(~hv109_recat, ~ubigeo_prov, design = .x, svymean, keep.var = T))
    )


# 2. The district-level estimates by year will be aggregated to the department-year level

predict_sae_loreto<-
  resultados_SAE_loreto_edu %>% 
  full_join(pop_dist) %>% # total population for calculate proportions
  group_by(year,modelo) %>% 
  summarise(
    total_loreto = sum(pop_landscan),
    mean_loreto = sum(mean),
    lower_loreto = sum(lower),
    upper_loreto = sum(upper)
  ) %>% 
  mutate(
    across(.cols = c(mean_loreto:upper_loreto), .f = ~.x/total_loreto)
  )


#3. It will be verified if the direct estimate is on the confidence interval (CI) of 
# the SAE-INLA model estimates

data_final_nest %>% 
  select(year,edu_dep) %>% 
  unnest(edu_dep) %>% 
  ungroup() %>%
  select(year,hv109_recat) %>% 
  
  full_join(predict_sae_loreto) %>% 
  
  mutate(
    check = ifelse(hv109_recat > lower_loreto & hv109_recat<upper_loreto, "cumple", "no cumple")
  ) %>% 
  
  filter(
    modelo %in% c('modelo_fh10_spatial',
                  'modelo_fh8_spatial',
                  'modelo_fh5_spatial',
                  'modelo_fh2_spatial')
  ) %>% 
  
  ggplot()+
  geom_pointrange(aes(x = modelo, y =mean_loreto,
                      col = modelo, ymin = lower_loreto, ymax = upper_loreto))+
  geom_point(aes(x = modelo, y = hv109_recat, shape = check), col = 'black', show.legend = F)+
  
  scale_shape_manual(
    values = c(
      "cumple" = 23, 
      "no cumple" = 15)
  ) +
  
  coord_flip()+
  facet_grid(rows = vars(year), switch = "y") +
  
  theme_minimal() +
  
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.text.y=element_blank(),
    axis.line=element_line(),
    strip.placement = "outside",
    axis.text.x=element_text(face="bold"),
    strip.text.y = element_text(vjust = 1,size = 10,face="bold"))


## Provinces ----

predict_sae_loreto_prov<-
  resultados_SAE_loreto_edu %>% 
  full_join(pop_dist) %>% # total population for calculate proportions
  group_by(year,ubigeo_prov,modelo) %>% 
  summarise(
    total_loreto = sum(pop_landscan),
    mean_loreto = sum(mean),
    lower_loreto = sum(lower),
    upper_loreto = sum(upper)
  ) %>% 
  mutate(
    across(.cols = c(mean_loreto:upper_loreto), .f = ~.x/total_loreto)
  )




data_final_nest %>% 
  select(year,edu_prov) %>% 
  unnest(edu_prov) %>% 
  ungroup() %>%
  select(year,ubigeo_prov,hv109_recat) %>% 
  mutate(
    ubigeo_prov = as.character(ubigeo_prov)
  ) %>% 
  
  full_join(predict_sae_loreto_prov) %>% 
  
  mutate(
    check = ifelse(hv109_recat > lower_loreto & hv109_recat<upper_loreto, "cumple", "no cumple")
  ) %>% 
  
  
  ggplot()+
  geom_pointrange(aes(x = modelo, y =mean_loreto,
                      col = modelo, ymin = lower_loreto, ymax = upper_loreto))+
  geom_point(aes(x = modelo, y = hv109_recat, shape = check), col = 'black', show.legend = F)+
  
  scale_shape_manual(
    values = c(
      "cumple" = 23, 
      "no cumple" = 15)
  ) +
  
  coord_flip()+
  facet_grid(ubigeo_prov~year, switch = "y") +
  
  theme_minimal()


# Aggregated consistency check - WI ----



predict_sae_loreto<-
  resultados_SAE_loreto_wi %>% 
  full_join(pop_dist) %>% # total population for calculate proportions
  group_by(year,modelo) %>% 
  summarise(
    total_loreto = round(sum(pop_landscan)),
    mean_loreto = sum(mean),
    lower_loreto = sum(lower),
    upper_loreto = sum(upper)
  ) %>% 
  mutate(
    across(.cols = c(mean_loreto:upper_loreto), .f = ~.x/total_loreto)
  )

data_final_nest %>% 
  select(year,wi_dep) %>% 
  unnest(wi_dep) %>% 
  ungroup() %>%
  select(year,hv270_recat,se) %>% 
  
  full_join(predict_sae_loreto) %>% 
  
  mutate(
    check = ifelse(hv270_recat > lower_loreto & hv270_recat<upper_loreto, "cumple", "no cumple"),
    across(.cols = c(hv270_recat,mean_loreto:upper_loreto), .f = ~round(.x,2)),
    
    dif_percet = hv270_recat - mean_loreto,
    
    se_modelo = (upper_loreto - lower_loreto) / (2 * 1.96),
    
    z = (hv270_recat - mean_loreto) / sqrt(se^2 + se_modelo^2),
    p_value = 2 * pnorm(-abs(z)),
    diferencia_significativa = ifelse(p_value < 0.05,"S","NS")
  ) %>% 
  
  filter(
    modelo %in% c('modelo_fh10_spatial',
                  'modelo_fh8_spatial',
                  'modelo_fh5_spatial',
                  'modelo_fh2_spatial',
                  'modelo_fh2_new')
  ) %>% 
  
  ggplot()+
  geom_pointrange(aes(x = modelo, y =mean_loreto,
                      col = modelo, ymin = lower_loreto, ymax = upper_loreto))+
  geom_pointrange(aes(x = modelo, y = hv270_recat, 
                      ymin = hv270_recat-(1.96*se), 
                      ymax = hv270_recat+(1.96*se),
                      shape = diferencia_significativa), 
                  
                  col = 'black', alpha = .5)+
  
  scale_shape_manual(
    values = c(
      "S" = 23, 
      "NS" = 15)
  ) +
  
  coord_flip()+
  facet_grid(rows = vars(year), switch = "y") +
  
  theme_minimal() +
  
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.text.y=element_blank(),
    axis.line=element_line(),
    strip.placement = "outside",
    axis.text.x=element_text(face="bold"),
    strip.text.y = element_text(vjust = 1,size = 10,face="bold"))
