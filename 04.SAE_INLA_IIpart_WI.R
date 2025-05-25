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

#shapefiles
data("Peru")
distritos<- Peru %>% filter(dep=="LORETO") %>% select(ubigeo,geometry)
aux_data<- read.csv("./data/aux_data/aux_data.csv") %>% mutate(ubigeo = as.character(ubigeo))

# working data
wi_censo_final<- read.csv("./data/wi_censo_final_recat.csv") %>% 
  as_tibble() %>% 
  mutate(ubigeo = as.character(ubigeo)) %>% 
  left_join(aux_data)

# Work begins on the variable wealth index (hv270), recategorized as:belongs to 
# quintile 1 or 2 (1) and is in the 3rd or higher (0).

hist(wi_censo_final$hv270_recat) # no normal distribution
hist(wi_censo_final$hv270_recat_cor)  # no normal distribution

hist(wi_edu_2010_2020$hv271) # wi in continuous form. no normal distribution

# because neither has a normal distribution, even after transforming to the logit scale,
# we explore other approaches (binomial, poisson, negative binomial).

# for any of three options, we need:
  # numerator: cases
  # denominator: total population
# we have the proportion of cases (direct estimation from ENDES) and total population
# from LandScan database.

# To obtain the numerator: 
 ##proportion of cases * total population = estimated (hypothetical) number of cases

# offset: log(total population)

# Fit INLA ----

## INLA - wealth index (hv207_recat) ----

# Spatial component
loreto.dist <- poly2nb(distritos)
w.loreto <- nb2mat(loreto.dist, style = "W")

# fit models ----
## Adding total population to the dataset (using malaria incidence data processed in the main repo)

pop_dist <- read.csv("./data/aux_data/malaria_dist_total.csv") %>% 
  select(year,iddist,pop_landscan) %>% 
  rename(ubigeo = iddist) %>% 
  mutate(
    ubigeo = as.character(ubigeo)
  )

wi_censo_final2<-
  wi_censo_final %>% 
  #select(year,ubigeo,hv270_recat,hv270_recat_cor) %>% 
  inner_join(pop_dist) %>% 
  mutate(
    pop_landscan = round(pop_landscan),
    cases_number = round(hv270_recat*pop_landscan)
  )

## comparing family: binomial vs poisson vs negative binomial

model_binom <- inla(
  formula = cases_number ~ 1 + 
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = wi_censo_final2,
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
  
  data = wi_censo_final2,
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
  
  data = wi_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_nb) # The negative binomial family was chosen.


# fit models (2)
model_fit10spat <- inla(
  formula = cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5 + total_millones + nls_year_norm +
    pc6 + pc7 + pc8 + pc9 + pc10 +
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = wi_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_fit10spat)

model_fit8spat <- inla(
  formula = cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5 + total_millones + nls_year_norm +
    pc6 + pc7 + pc8 + total_millones + 
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = wi_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_fit8spat)

model_fit5spat <- inla(
  formula = cases_number ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5 + total_millones + nls_year_norm +
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = wi_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_fit5spat)

model_fit2spat <- inla(
  formula = cases_number ~ 1 + pc1 + pc2 + 
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = wi_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_fit2spat)


model_fit2spat_new <- inla(
  formula = cases_number ~ 1 + pc1 + pc2 + total_millones + nls_year_norm + 
    f(id.sp, model = "bym", graph = w.loreto) +   # efecto espacial
    f(year, model = "ar1"),                       # efecto temporal
  
  data = wi_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
)

summary(model_fit2spat_new)

# Model accuracy ----

distritos_sf_predict<-
  wi_censo_final2 %>% 
  mutate(
    fit8_spat = (model_fit8spat$summary.fitted.values$mean),
    fit10_spat = (model_fit10spat$summary.fitted.values$mean),
    fit5_spat = (model_fit5spat$summary.fitted.values$mean),
    fit2_spat = (model_fit2spat$summary.fitted.values$mean),
    fit2_new = model_fit2spat_new$summary.fitted.values$mean,
    real_cases = (wi_censo_final2$cases_number)
  ) %>% 
  left_join(distritos) %>% 
  st_as_sf()


map.direct_est <-
  distritos_sf_predict %>% 
  filter(year == 2010) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=cases_number))+
  scale_fill_viridis(direction = -1,option = "rocket") +
  theme(
    #legend.position = "bottom"
  )

map2sp <-
  distritos_sf_predict %>% 
  filter(year == 2010) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit2_spat))+
  scale_fill_viridis(direction = -1,option = "rocket") +
  theme(
    #legend.position = "bottom"
  )

map5sp <-
  distritos_sf_predict %>% 
  filter(year == 2010) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit5_spat))+
  scale_fill_viridis(direction = -1,option = "rocket") +
  theme(
    #legend.position = "bottom"
  )

map8sp <-
  distritos_sf_predict %>% 
  filter(year == 2010) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit8_spat))+
  scale_fill_viridis(direction = -1,option = "rocket") +
  theme(
    #legend.position = "bottom"
  )

map10sp <-
  distritos_sf_predict %>% 
  filter(year == 2010) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit10_spat))+
  scale_fill_viridis(direction = -1,option = "rocket") +
  theme(
    #legend.position = "bottom"
  )

cowplot::plot_grid(map.direct_est,map2sp,map5sp,map8sp,map10sp)


## Comparison DE vs Pred. SAE ----
distritos_sf_predict %>% 
  select(year,cases_number,fit2_new,fit2_spat,fit5_spat,fit8_spat,fit10_spat) %>% 
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

fit8_spat = (model_fit8spat$summary.fitted.values$mean)
fit10_spat = (model_fit10spat$summary.fitted.values$mean)
fit5_spat = (model_fit5spat$summary.fitted.values$mean)
fit2_spat = (model_fit2spat$summary.fitted.values$mean)
fit2_spat_new = model_fit2spat_new$summary.fitted.values$mean
real = wi_censo_final2$cases_number


fitted_vals  <-  list("pc10sp" = fit10_spat,
                      "pc8sp" = fit8_spat,
                      "pc5sp" = fit5_spat,
                      "pc2sp" = fit2_spat,
                      "pc2new" = fit2_spat_new) %>% 
  as.data.frame() %>% 
  gather(key = "modelo", value = "fit") %>% 
  group_by(modelo) %>% 
  mutate(
    actual = real,
    ubigeo = as.character(wi_censo_final$ubigeo)
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
      rows = modelo %in% c("pc2sp")
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
  theme_linedraw(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold",size = 30),
    legend.position = "bottom") 

# cross validation ----

cpo.10cp.sp <- -2*sum(log(model_fit10spat$cpo$cpo),na.rm = T)
cpo.8cp.sp <- -2*sum(log(model_fit8spat$cpo$cpo),na.rm = T)
cpo.5cp.sp <- -2*sum(log(model_fit5spat$cpo$cpo),na.rm = T)
cpo.2cp.sp <- -2*sum(log(model_fit2spat$cpo$cpo),na.rm = T)

data.cpo  <-  list("modelo10" = cpo.10cp.sp,
                   "modelo8" = cpo.8cp.sp,
                   "modelo5" = cpo.5cp.sp,
                   "modelo2" = cpo.2cp.sp) %>% 
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
  select(year,ubigeo,real_cases,fit2_new,fit2_spat,
         fit5_spat,fit8_spat,fit10_spat) %>% 
  pivot_longer(cols = fit2_new:fit10_spat) %>% 
  mutate(
    #ubigeo = as.numeric(ubigeo)
  ) %>% 
  
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

graficos$graph[[1]]


# calculation proportions with cases predicted

prop_sae_predict_wi<-
  distritos_sf_predict %>% 
  select(year,ubigeo,pop_landscan,real_cases,fit2_new,fit2_spat,
         fit5_spat,fit8_spat,fit10_spat,geometry) %>% 
  mutate(
    across(.cols = c(real_cases:fit10_spat), .f=~.x/pop_landscan)
  )


# prop_sae_predict_wi %>% 
#   pivot_longer(cols = real_cases:fit10_spat) %>% 
#   mutate(
#     name = factor(name, levels = c("real_cases", "fit2_new", 
#                                    "fit2_spat", "fit5_spat", 
#                                    "fit8_spat", "fit10_spat"))
#   ) %>% 
#   
#   ggplot()+
#   geom_sf(aes(fill = value)) +
#   scale_fill_viridis(direction = -1,option = "rocket") +
#   facet_grid(name~year)


# Point and interval estimates of proportions (INLA)

modelos <- list( # lista para trabajar en bloque
 
  modelo_fh8_spatial = model_fit10spat,
  modelo_fh10_spatial = model_fit8spat,
  modelo_fh5_spatial = model_fit5spat,
  modelo_fh2_spatial = model_fit2spat,
  modelo_fh2_new = model_fit2spat_new
)


resultados_SAE_loreto_wi <- 
  imap_dfr(modelos, ~{ # formato long de las estimaciones y los IC de cada modelo distrito-año
    data.frame(
      year = wi_censo_final2$year,
      ubigeo = wi_censo_final2$ubigeo,
      mean = .x$summary.fitted.values$mean,
      lower = .x$summary.fitted.values$`0.025quant`,
      upper = .x$summary.fitted.values$`0.975quant`,
      modelo = .y
    )
  }) 

#write.csv(resultados_SAE_loreto_edu, "./data/final_data/resultados_SAE_loreto_edu.csv", row.names = F )

resultados_SAE_loreto_wi %>% 
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

