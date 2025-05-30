library(tidyverse)
library(janitor)
library(sf)
library(innovar)
library(factoextra)
library(spdep)
library(INLA)
library(car)
library(yardstick)
library(gt)
library(viridis)

data("Peru")
distritos<- Peru %>% filter(dep=="LORETO") %>% select(ubigeo,geometry)
aux_data<- read.csv("./data/aux_data/aux_data.csv") %>% mutate(ubigeo = as.character(ubigeo))
# data de trabajo
edu_censo_final<- read.csv("./data/edu_censo_final_recat.csv") %>% as_tibble() %>% mutate(ubigeo = as.character(ubigeo))

wi_censo_final<- read.csv("./data/wi_censo_final_recat.csv") %>% as_tibble() %>% mutate(ubigeo = as.character(ubigeo))

# to this point: we have the direct estimates and standard errors at the district level for the wealth index (hv270) 
# and education level (hv109). Additionally, due to the complex sampling design of the ENDES, data is available for some
# of the 53 districts. On the other hand, variables of interest from the 2017 Census were used as covariates. A total of 92 variables were obtained. 
# A PCA was performed, and 20 principal components were selected. 
# INLA-based models will be fitted to produce SAE and to predict values for the missing districts.

# Fit INLA ----

# INLA - Education ----

## adding auxiliary data

edu_censo_final<-
  edu_censo_final %>% 
  left_join(aux_data)

## HV109 count

## Adding total population to the dataset (using malaria incidence data processed in the main repo)

pop_dist <- read.csv("./data/aux_data/malaria_dist_total.csv") %>% 
  select(year,iddist,pop_landscan) %>% 
  rename(ubigeo = iddist) %>% 
  mutate(
    ubigeo = as.character(ubigeo)
  )


edu_censo_final2<-
  edu_censo_final %>% 
  #select(year,ubigeo,hv270_recat,hv270_recat_cor) %>% 
  inner_join(pop_dist) %>% 
  mutate(
    pop_landscan = round(pop_landscan),
    cases_number = round(hv109_recat_cor*pop_landscan)
  )


# Spatial component
loreto.dist <- poly2nb(distritos)
w.loreto <- nb2mat(loreto.dist, style = "W")

# fit models ----

## comparing family: binomial vs poisson vs negative binomial

model_binom <- inla(
  formula = cases_number ~ 1 + 
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = edu_censo_final2,
  family = "binomial",
  Ntrials = pop_landscan,
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_binom)


model_poisson <- inla(
  formula = cases_number ~ 1 + 
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = edu_censo_final2,
  family = "poisson",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_poisson)

model_nb <- inla(
  formula = cases_number ~ 1 + 
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = edu_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_nb) # The negative binomial family was chosen.

# fit models (2) ----

modelo_fh8_spatial <- inla(
  
  formula =  cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+ 
    pc6 + pc7 + pc8 + total_millones +
    f(id.sp, model = "bym2", graph = w.loreto)+
    f(year, model = "ar1"),
  
  data = edu_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)

summary(modelo_fh8_spatial)

# modelo_fh8 <- inla(
#   
#   formula =  cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+
#     pc6 + pc7 + pc8 + total_millones +
#     f(year, model = "ar1"),
#   
#   data = edu_censo_final2,
#   family = "nbinomial",
#   offset = log(pop_landscan),  
#   
#   control.predictor = list(compute = TRUE, link = 1),
#   control.compute = list(dic = TRUE, cpo = TRUE)
# )

# summary(modelo_fh8)

modelo_fh10_spatial <- inla(
  
  formula =  cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+
    pc6 + pc7 + pc8 + pc9 + pc10 + total_millones +
    f(id.sp, model = "bym2", graph = w.loreto)+
    f(year, model = "ar1"),
  
  data = edu_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)

summary(modelo_fh10_spatial)

# modelo_fh10 <- inla(
#   
#   formula =  cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+
#     pc6 + pc7 + pc8 + pc9 + pc10+ total_millones +
#     f(year, model = "ar1"),
#   
#   data = edu_censo_final2,
#   family = "nbinomial",
#   offset = log(pop_landscan),  
#   
#   control.predictor = list(compute = TRUE, link = 1),
#   control.compute = list(dic = TRUE, cpo = TRUE)
# )
# 
# summary(modelo_fh10)


modelo_fh5_spatial <- inla(
  
  formula =  cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5 + total_millones +
    f(id.sp, model = "bym2", graph = w.loreto)+ 
    f(year, model = "ar1"),
  
  data = edu_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)

summary(modelo_fh5_spatial)

# modelo_fh5 <- inla(
#   
#   formula =  cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+ total_millones + 
#     f(year, model = "ar1"),
#   
#   data = edu_censo_final2,
#   family = "nbinomial",
#   offset = log(pop_landscan),  
#   
#   control.predictor = list(compute = TRUE, link = 1),
#   control.compute = list(dic = TRUE, cpo = TRUE)
# )
# 
# summary(modelo_fh5)

# modelo_fh2 <- inla(
#   
#   formula =  cases_number ~ 1 + pc1 + pc2 + total_millones +
#     f(year, model = "ar1"),
#   
#   data = edu_censo_final2,
#   family = "nbinomial",
#   offset = log(pop_landscan),  
#   
#   control.predictor = list(compute = TRUE, link = 1),
#   control.compute = list(dic = TRUE, cpo = TRUE)
# )
# 
# summary(modelo_fh2)


modelo_fh2_spatial <- inla(
  
  formula =  cases_number ~ 1 + pc1 + pc2 +
    f(year, model = "ar1") +
    f(id.sp, model = "bym2", graph = w.loreto),
  
  data = edu_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)

summary(modelo_fh2_spatial)

modelo_fh2_spatial_new <- inla(
  
  formula =  cases_number ~ 1 + pc1 + pc2 + total_millones +
    f(year, model = "ar1") +
    f(id.sp, model = "bym2", graph = w.loreto),
  
  data = edu_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE)
)

summary(modelo_fh2_spatial_new)



# Summary ----

model_metrics <- tibble(
  Modelo = c("Modelo 10", "Modelo 8", "Modelo 5", "Modelo 2","modelo 2 new"),
  DIC     = c(modelo_fh10_spatial$dic$dic, modelo_fh8_spatial$dic$dic, 
              modelo_fh5_spatial$dic$dic, modelo_fh2_spatial$dic$dic,modelo_fh2_spatial_new$dic$dic),
  pD      = c(modelo_fh10_spatial$dic$p.eff, modelo_fh8_spatial$dic$p.eff, 
              modelo_fh5_spatial$dic$p.eff, modelo_fh2_spatial$dic$p.eff,modelo_fh2_spatial_new$dic$p.eff),
  WAIC    = c(modelo_fh10_spatial$waic$waic, modelo_fh8_spatial$waic$waic, 
              modelo_fh5_spatial$waic$waic, modelo_fh2_spatial$waic$waic,modelo_fh2_spatial_new$waic$waic),
  pWAIC   = c(modelo_fh10_spatial$waic$p.eff, modelo_fh8_spatial$waic$p.eff, 
              modelo_fh5_spatial$waic$p.eff, modelo_fh2_spatial$waic$p.eff,modelo_fh2_spatial_new$waic$p.eff),
  logML   = c(modelo_fh10_spatial$mlik[1,1], modelo_fh8_spatial$mlik[1,1], 
              modelo_fh5_spatial$mlik[1,1], modelo_fh2_spatial$mlik[1,1],modelo_fh2_spatial_new$mlik[1,1])
)


model_metrics %>%
  gt() %>%
  tab_header(title = md("**Comparación de modelos INLA**")) %>%
  fmt_number(columns = c(DIC, pD, WAIC, pWAIC, logML), decimals = 2)


# Model accuracy ----

distritos_sf_predict<-
  edu_censo_final2 %>% 
  mutate(
    fit8 = (modelo_fh8$summary.fitted.values$mean),
    fit8_spat = (modelo_fh8_spatial$summary.fitted.values$mean),
    fit10 = (modelo_fh10$summary.fitted.values$mean),
    fit10_spat = (modelo_fh10_spatial$summary.fitted.values$mean),
    fit5 = (modelo_fh5$summary.fitted.values$mean),
    fit5_spat = (modelo_fh5_spatial$summary.fitted.values$mean),
    fit2_spatial = (modelo_fh2_spatial$summary.fitted.values$mean),
    fit2 = (modelo_fh2$summary.fitted.values$mean),
    fit2_new = (modelo_fh2_spatial_new$summary.fitted.values$mean),
    real_cases = (edu_censo_final2$cases_number)
  ) %>% 
  left_join(distritos) %>% 
  st_as_sf()

map.direct_est <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=real_cases))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )

# map.real_censo<-
#   distritos %>% 
#   inner_join(censo_final_loreto %>% 
#                mutate(ubigeo = as.character(ubigeo)) %>% 
#                select(ubigeo,c5_p13_niv_recat_1:c5_p13_niv_recat_6)) %>% 
#   ggplot(
#     
#   ) +
#   geom_sf(aes(fill = c5_p13_niv_recat_1)) +
#   theme(
#     legend.position = "bottom"
#   )


map.15 <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit8))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )

map.15spat <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit8_spat))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )

map.10 <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit10))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )


map.10spat <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit10_spat))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )


map.5 <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit5))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )

map.5spat<-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit5_spat))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )


map.2 <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit2))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )

map.2spat<-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit2_spatial))+
  scale_fill_viridis(direction = -1,option = "rocket")+
  theme(
    legend.position = "bottom"
  )


cowplot::plot_grid(map.direct_est,map.5,map.5spat
                   ,map.10, map.10spat,
                   map.15,map.15spat,map.2, map.2spat, nrow = 2)


## Comparison DE vs Pred. SAE ----
distritos_sf_predict %>% 
  select(year,cases_number,fit2,fit2_spatial,fit10,fit10_spat,fit5,fit5_spat) %>% 
  st_drop_geometry() %>%
  filter(!is.na(cases_number)) %>%
  rename(directo = cases_number) %>%
  pivot_longer(cols = starts_with("fit"),
               names_to = "modelo",
               values_to = "predicho") %>%
  ggplot(aes(x = directo, y = predicho, color = modelo)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  facet_grid(year~modelo) +
  labs(
    x = "Estimación directa",
    y = "Predicción del modelo",
    title = "Comparación de estimaciones directas vs modelos SAE",
    subtitle = "Línea diagonal = ajuste perfecto"
  ) +
  theme_minimal()


## Performance metrics ----

fit8 <- (modelo_fh8$summary.fitted.values$mean)
fit8_spat <- (modelo_fh8_spatial$summary.fitted.values$mean)
fit10 <- (modelo_fh10$summary.fitted.values$mean)
fit10_spat <- (modelo_fh10_spatial$summary.fitted.values$mean)
fit5 <- (modelo_fh5$summary.fitted.values$mean)
fit5_spat <- (modelo_fh5_spatial$summary.fitted.values$mean)
fit2 <- (modelo_fh2$summary.fitted.values$mean)
fit2_spat <- (modelo_fh2_spatial$summary.fitted.values$mean)
fit2_new <- (modelo_fh2_spatial_new$summary.fitted.values$mean)
estimacion_real <- (edu_censo_final2$cases_number)


fitted_vals  <-  list("pc10" = fit10,
                      "pc10sp" = fit10_spat,
                      "pc8" = fit8,
                      "pc8sp" = fit8_spat,
                      "pc5" = fit5,
                      "pc5sp" = fit5_spat,
                      "pc2" = fit2,
                      "pc2sp" = fit2_spat,
                      "pc2new" = fit2_new) %>% 
  as.data.frame() %>% 
  gather(key = "modelo", value = "fit") %>% 
  group_by(modelo) %>% 
  mutate(
    actual = estimacion_real,
    ubigeo = as.character(edu_censo_final$ubigeo)
  )

#### Metrics ----
perform.metrics <- yardstick::metric_set(mae,mase,smape,rmse)
perform.metrics.dist <- yardstick::metric_set(mae,smape,rmse)

##### total metrics----
tbl.yrd.full <-  fitted_vals %>% 
  group_by(modelo) %>%
  perform.metrics(truth = actual, estimate = fit)

##### metrics by districts----
tbl.yrd.dist <-  fitted_vals %>% 
  group_by(modelo,ubigeo) %>%
  perform.metrics.dist(truth = actual, estimate = fit)

## Table in-sample accuracy metrics ----
tbl.yrd.full %>% 
  pivot_wider(id_cols = modelo,
              names_from = .metric,
              values_from = .estimate) %>%         
  gt() %>%
  tab_header(title = md("in-sample accuracy metrics")) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#FFF3B0"),  # amarillo claro
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = modelo %in% c("pc5sp", "pc2sp","pc2new")
    )
  )

### mapping the performance metrics
distritos %>% 
  inner_join(tbl.yrd.dist) %>% 
  filter(.metric=="smape" & modelo %in% c("pc5","pc5sp",
                                          "pc10","pc10sp",
                                          "pc8","pc8sp",
                                          "pc2","pc2sp","pc2new")) %>% 
  ggplot() +
  geom_sf(aes(fill=.estimate),lwd=0.1) +
  scale_fill_distiller(palette="Reds",direction=1,name="smape") +
  facet_wrap(vars(modelo), nrow = 2) +
  theme_linedraw(base_size = 23) +
  theme(strip.text = element_text(face = "bold",size = 30)) 


# cross validation ----

cpo.5cp <--2*sum(log(modelo_fh5$cpo$cpo), na.rm = T)
cpo.5.cp.sp <--2*sum(log(modelo_fh5_spatial$cpo$cpo),na.rm = T)
cpo.10 <--2*sum(log(modelo_fh10$cpo$cpo),na.rm = T)
cpo.10cp.sp <--2*sum(log(modelo_fh10_spatial$cpo$cpo),na.rm = T)
cpo.2cp <--2*sum(log(modelo_fh2$cpo$cpo),na.rm = T)
cpo.2cp.sp <--2*sum(log(modelo_fh2_spatial$cpo$cpo),na.rm = T)
cpo.2cp.sp.new <- -2*sum(log(modelo_fh2_spatial_new$cpo$cpo),na.rm = T)

data.cpo  <-  list("modelo5" = cpo.5cp,
                   "modelo5spat" = cpo.5.cp.sp,
                   "modelo10" = cpo.10,
                   "modelo10spat" = cpo.10cp.sp,
                   "modelo2" = cpo.2cp,
                   "modelo2spat" = cpo.2cp.sp,
                   "modelo2_new" = cpo.2cp.sp.new) %>% 
  as.data.frame()

data.cpo %>% 
  pivot_longer(cols=colnames(data.cpo),
               names_to = "model",
               values_to = "CPO")  %>% 
  gt() %>%
  tab_header(title = md("LOO-CV")) 

## Visualization ---- 

# real values of edu level
graficos<-
  distritos_sf_predict %>% 
  st_drop_geometry() %>% 
  select(year,ubigeo,real_cases,fit8:fit2_new) %>% 
  pivot_longer(cols = fit8:fit2_new) %>% 
  
  group_by(name) %>% 
  nest() %>% 
  
  mutate(
    data_graph = map(.x = data,
                     .f = ~.x %>% pivot_longer(cols = c(real_cases,value))),
    
    graph = map2(.x = data_graph, .y = name,
                .f = ~.x %>% ggplot(aes(x = value, y=ubigeo, col = name))+
                  geom_point(alpha = 0.5)+
                  geom_line(aes(group = ubigeo))+
                  facet_wrap(~year, scales = "free")+
                  ggtitle(name))
  )

graficos$graph[[4]]
  
  
  
# calculation proportions with cases predicted
  
prop_sae_predict_edu<-
  distritos_sf_predict %>% 
  select(year,ubigeo,pop_landscan,real_cases,fit10_spat,
         fit8_spat,fit5_spat,fit2_spatial,fit2_new,geometry) %>% 
  mutate(
    across(.cols = c(real_cases:fit2_new), .f=~.x/pop_landscan)
  )


prop_sae_predict_edu %>% 
  pivot_longer(cols = real_cases:fit2_new) %>% 
  mutate(
    name = factor(name, levels = c("real_cases", "fit2_new", "fit2_spatial", "fit5_spat", 
                                   "fit8_spat", "fit10_spat"))
  ) %>% 
  
  ggplot()+
  geom_sf(aes(fill = value)) +
  scale_fill_viridis(direction = -1,option = "rocket") +
  facet_grid(name~year)


# Point and interval estimates of proportions (INLA)

modelos <- list( # lista para trabajar en bloque
  modelo_fh8 = modelo_fh8,
  modelo_fh8_spatial = modelo_fh8_spatial,
  modelo_fh10 = modelo_fh10,
  modelo_fh10_spatial = modelo_fh10_spatial,
  modelo_fh5 = modelo_fh5,
  modelo_fh5_spatial = modelo_fh5_spatial,
  modelo_fh2 = modelo_fh2,
  modelo_fh2_spatial = modelo_fh2_spatial,
  modelo_fh2_new = modelo_fh2_spatial_new
)


resultados_SAE_loreto_edu <- 
  imap_dfr(modelos, ~{ # formato long de las estimaciones y los IC de cada modelo distrito-año
  data.frame(
    year = edu_censo_final$year,
    ubigeo = edu_censo_final$ubigeo,
    mean = .x$summary.fitted.values$mean,
    lower = .x$summary.fitted.values$`0.025quant`,
    upper = .x$summary.fitted.values$`0.975quant`,
    modelo = .y
  )
}) 

write.csv(resultados_SAE_loreto_edu, "./data/final_data/resultados_SAE_loreto_edu.csv", row.names = F )

resultados_SAE_loreto_edu %>% 
  ggplot(aes(x = mean, y = ubigeo, col = modelo))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper))+
  facet_grid(modelo~year)


# accuracy metris vs population

tbl.yrd.dist %>% 
  full_join(
pop_dist %>% 
  group_by(ubigeo) %>% 
  summarise(pop_model = sum(pop_landscan))
) %>% 
  filter(
    modelo %in% c("pc2new","pc5sp","pc8sp","pc10sp")
  ) %>% 
  
  ggplot(aes(x = .estimate, y = (pop_model), col = modelo))+
  geom_point(aes(shape = modelo), alpha = .5, size = 3)+
  geom_line(aes(group = ubigeo)) +
  
  coord_flip() +
  
  facet_wrap(~.metric, scales = "free", ncol = 1)


