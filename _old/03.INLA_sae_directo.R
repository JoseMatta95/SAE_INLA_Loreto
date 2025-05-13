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

# 1. data ----

data("Peru")
distritos<- Peru %>% filter(dep=="LORETO") %>% select(ubigeo,geometry)
edu_censo_total<- read.csv("./data processing/INLA_SAE/data/edu_censo_total.csv") %>% as_tibble()

edu_censo_total<-
  edu_censo_total %>% 
  group_by(
    year
  ) %>% 
  mutate(
    colegio = prueba2$n
  )
# Ajustando INLA ----

# Componente espacial
loreto.dist <- poly2nb(distritos)
w.loreto <- nb2mat(loreto.dist, style = "W")

# Componente temporal: deberia considerarlo y trabajar con varios años a la vez?

# ajuste de modelos ----

modelo_1 <- inla(
  
  formula =  hv109PE_cor_logit ~ 1 + c5_p13_niv_recat_1+c5_p13_niv_recat_6+colegio+
    f(id.sp, model = "bym", graph = w.loreto)+
    f(year, model = "ar1"),
  
  data = edu_censo_total,
  family = "gaussian",
  control.family = list(
    hyper = list(prec=list(fixed=T))
  ),
  scale = edu_censo_final$se_hv109PE_cor,  # varianza logit como pesos
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, cpo = TRUE)
)

summary(modelo_1)

edu_censo_total$fit1 <- modelo_1$summary.fitted.values$mean


edu_censo_total %>% 
  ggplot()+
  geom_point(aes(x = fit1, y = plogis(hv109NE_cor_logit)))+
  facet_wrap(~year)
