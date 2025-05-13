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

data("Peru")
distritos<- Peru %>% filter(dep=="LORETO") %>% select(ubigeo,geometry)
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

## HV109_recat (transformed variable)

# Spatial component
loreto.dist <- poly2nb(distritos)
w.loreto <- nb2mat(loreto.dist, style = "W")

# fit models ----

modelo_fh8_spatial <- inla(
  
  formula =  hv109_recat_cor_logit ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+
    pc6 + pc7 + pc8 +
    f(id.sp, model = "bym", graph = w.loreto)+
    f(year, model = "ar1"),
  
  data = edu_censo_final,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109_recat_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)
summary(modelo_fh8_spatial)

modelo_fh8 <- inla(
  
  formula =  hv109_recat_cor_logit ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+
    pc6 + pc7 + pc8 +
    f(year, model = "ar1"),
  
  data = edu_censo_final,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109_recat_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)

summary(modelo_fh8)

modelo_fh10_spatial <- inla(
  
  formula =  hv109_recat_cor_logit ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+
    pc6 + pc7 + pc8 + pc9 + pc10 + f(id.sp, model = "bym", graph = w.loreto)+
    f(year, model = "ar1"),
  
  data = edu_censo_final,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109_recat_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)

summary(modelo_fh10_spatial)

modelo_fh10 <- inla(
  
  formula =  hv109_recat_cor_logit ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+
    pc6 + pc7 + pc8 + pc9 + pc10+
    f(year, model = "ar1")
    ,
  
  data = edu_censo_final,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109_recat_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)

summary(modelo_fh10)


modelo_fh5_spatial <- inla(
  
  formula =  hv109_recat_cor_logit ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5 +
    f(id.sp, model = "bym", graph = w.loreto)+
    f(year, model = "ar1"),
  
  data = edu_censo_final,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109_recat_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)

summary(modelo_fh5_spatial)

modelo_fh5 <- inla(
  
  formula =  hv109_recat_cor_logit ~ 1 + pc1 + pc2 + pc3 + pc4 + pc5+
    f(year, model = "ar1"),
  
  data = edu_censo_final,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109_recat_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)

summary(modelo_fh5)

modelo_fh2 <- inla(
  
  formula =  hv109_recat_cor_logit ~ 1 + pc1 + pc2 +
    f(year, model = "ar1"),
  
  data = edu_censo_final,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109_recat_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)

summary(modelo_fh2)


modelo_fh2_spatial <- inla(
  
  formula =  hv109_recat_cor_logit ~ 1 + pc1 + pc2 +
    f(year, model = "ar1") +
    f(id.sp, model = "bym", graph = w.loreto),
  
  data = edu_censo_final,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109_recat_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)

summary(modelo_fh2_spatial)

# Model accuracy ----

distritos_sf_predict<-
  edu_censo_final %>% 
  mutate(
    fit8 = plogis(modelo_fh8$summary.fitted.values$mean),
    fit8_spat = plogis(modelo_fh8_spatial$summary.fitted.values$mean),
    fit10 = plogis(modelo_fh10$summary.fitted.values$mean),
    fit10_spat = plogis(modelo_fh10_spatial$summary.fitted.values$mean),
    fit5 = plogis(modelo_fh5$summary.fitted.values$mean),
    fit5_spat = plogis(modelo_fh5_spatial$summary.fitted.values$mean),
    fit2_spatial = plogis(modelo_fh2_spatial$summary.fitted.values$mean),
    fit2 = plogis(modelo_fh2$summary.fitted.values$mean),
    real_hv109_recat = plogis(edu_censo_final$hv109_recat_cor_logit)
  ) %>% 
  left_join(distritos) %>% 
  st_as_sf()

map.direct_est <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=real_hv109_recat))+
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
  theme(
    legend.position = "bottom"
  )

map.15spat <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit8_spat))+
  theme(
    legend.position = "bottom"
  )

map.10 <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit10))+
  theme(
    legend.position = "bottom"
  )


map.10spat <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit10_spat))+
  theme(
    legend.position = "bottom"
  )


map.5 <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit5))+
  theme(
    legend.position = "bottom"
  )

map.5spat<-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit5_spat))+
  theme(
    legend.position = "bottom"
  )


map.2 <-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit2))+
  theme(
    legend.position = "bottom"
  )

map.2spat<-
  distritos_sf_predict %>% 
  filter(year == 2020) %>% 
  ggplot()+
  geom_sf(data = distritos)+
  geom_sf(aes(fill=fit2_spatial))+
  theme(
    legend.position = "bottom"
  )


cowplot::plot_grid(map.direct_est,map.5,map.5spat
                   ,map.10, map.10spat,
                   map.15,map.15spat,map.2, map.2spat, nrow = 2)


## Comparison DE vs Pred. SAE ----
distritos_sf_predict %>% 
  select(year,real_hv109_recat,fit2,fit2_spatial,fit10,fit10_spat,fit5,fit5_spat) %>% 
  st_drop_geometry() %>%
  filter(!is.na(real_hv109_recat)) %>%
  rename(directo = real_hv109_recat) %>%
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

fit8 <- plogis(modelo_fh8$summary.fitted.values$mean)
fit8_spat <- plogis(modelo_fh8_spatial$summary.fitted.values$mean)
fit10 <- plogis(modelo_fh10$summary.fitted.values$mean)
fit10_spat <- plogis(modelo_fh10_spatial$summary.fitted.values$mean)
fit5 <- plogis(modelo_fh5$summary.fitted.values$mean)
fit5_spat <- plogis(modelo_fh5_spatial$summary.fitted.values$mean)
fit2 <- plogis(modelo_fh2$summary.fitted.values$mean)
fit2_spat <- plogis(modelo_fh2_spatial$summary.fitted.values$mean)
estimacion_real <- plogis(edu_censo_final$hv109_recat_cor_logit)


fitted_vals  <-  list("pc10" = fit10,
                      "pc10sp" = fit10_spat,
                      "pc8" = fit8,
                      "pc8sp" = fit8_spat,
                      "pc5" = fit5,
                      "pc5sp" = fit5_spat,
                      "pc2" = fit2,
                      "pc2sp" = fit2_spat) %>% 
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
      rows = modelo %in% c("pc2", "pc2sp")
    )
  )

### mapping the performance metrics
distritos %>% 
  inner_join(tbl.yrd.dist) %>% 
  filter(.metric=="smape" & modelo %in% c("pc5","pc5sp",
                                          "pc10","pc10sp",
                                          "pc8","pc8sp",
                                          "pc2","pc2sp")) %>% 
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

data.cpo  <-  list("modelo5" = cpo.5cp,
                   "modelo5spat" = cpo.5.cp.sp,
                   "modelo10" = cpo.10,
                   "modelo10spat" = cpo.10cp.sp,
                   "modelo2" = cpo.2cp,
                   "modelo2spat" = cpo.2cp.sp) %>% 
  as.data.frame()

data.cpo %>% 
  pivot_longer(cols=colnames(data.cpo),
               names_to = "model",
               values_to = "CPO")  %>% 
  gt() %>%
  tab_header(title = md("LOO-CV")) 

## Visualization ---- 

# real values of edu level
distritos_sf_predict %>% 
  select(year,ubigeo,se_hv109_recat_cor,real_hv109_recat,fit8:fit2) %>% 
  mutate(
    ubigeo2 = ifelse(is.na(real_hv109_recat),paste0(ubigeo,"*"),ubigeo),
    real_sehv109 = plogis(se_hv109_recat_cor)
  ) %>% 
  
  ggplot(aes(x = real_hv109_recat, y = ubigeo2))+
  
  geom_point()+
  geom_errorbar(aes(xmin = real_hv109_recat - (1.96*real_sehv109), xmax = real_hv109_recat + (1.96*real_sehv109)),alpha = 0.5)+
  facet_wrap(~year)


# fitted values of edu level

modelos <- list(
  # modelo_fh8 = modelo_fh8,
  # modelo_fh8_spatial = modelo_fh8_spatial,
  # modelo_fh10 = modelo_fh10,
  # modelo_fh10_spatial = modelo_fh10_spatial,
  modelo_fh5 = modelo_fh5,
  modelo_fh5_spatial = modelo_fh5_spatial,
  modelo_fh2 = modelo_fh2,
  modelo_fh2_spatial = modelo_fh2_spatial
)

resultados <- imap_dfr(modelos, ~{
  data.frame(
    year = edu_censo_final$year,
    ubigeo = edu_censo_final$ubigeo,
    mean = .x$summary.fitted.values$mean,
    lower = .x$summary.fitted.values$`0.025quant`,
    upper = .x$summary.fitted.values$`0.975quant`,
    modelo = .y
  )
}) %>% 
  mutate(across(.cols = c(mean,lower,upper), .f = ~plogis(.x)))

resultados %>% 
  ggplot(aes(x = mean, y = ubigeo, col = modelo))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper))+
  facet_grid(modelo~year)

