library(tidyverse)
library(survey)
library(janitor)
library(sf)
library(innovar)
library(fastDummies)
library(ggcorrplot)
library(corrplot)
library(factoextra)
library(spdep)
library(INLA)
library(car)
library(data.table)

wi_edu_2009_2021 <-read.csv("./data/aux_data/_other/wi_edu_2009_2021.csv") %>% mutate(ubigeo = as.character(ubigeo))
censo_pca_20 <- read.csv("./data/censo_2017/censo_2017_pca_20.csv") %>% mutate(ubigeo = as.character(ubigeo))
data("Peru")
distritos<- Peru %>% mutate(ubigeo = as.character(ubigeo)) %>% filter(dep == "LORETO")
aux_data<- read.csv("./data/aux_data/aux_data.csv") %>% mutate(ubigeo = as.character(ubigeo))
pop_dist <- read.csv("./data/aux_data/_other/pop_landscan_dist.csv") %>% 
  select(year,iddist,pop_landscan) %>% 
  mutate(
    ubigeo = as.character(iddist)
  )


options(survey.lonely.psu = "adjust")
data_final_nest<-
  wi_edu_2009_2021 %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    svydesign_endes= map(.x=data,
                         .f = ~svydesign(ids = ~hv001, strata = ~hv022, 
                                         weights = ~hv005, data = .x, nest = T)),
    
    
    wi_directa_recat = map(.x = svydesign_endes,
                           .f = ~svyby(~hv270_recat, ~ubigeo, design = .x, svymean, keep.var = T)),
    
    
    edu_directa_recat = map(.x = svydesign_endes,
                            .f = ~svyby(~hv109_recat, ~ubigeo, design = .x, svymean, keep.var = T))
  )


epsilon <- 0.005
direct_estimation_censo_total <- 
  data_final_nest %>%
  select(year,edu_directa_recat,wi_directa_recat) %>% 
  mutate(
    edu_censo_cpa_recat = map(.x = edu_directa_recat,
                        .f = ~distritos %>% 
                          full_join(
                            censo_pca_20 %>% 
                              left_join(.x) %>% 
                              
                              mutate(
                                across(.cols = hv109_recat, .f = ~.x * (1 - 2 * epsilon) + epsilon, .names = '{.col}_cor'),
                                
                                across(.cols = hv109_recat_cor, .f =~logit(.x), .names = '{.col}_logit'),
                                
                                se_hv109_recat_cor = (se^2)/(hv109_recat_cor^2 * (1-hv109_recat_cor)^2)
                              ) %>% 
                              
                              mutate(
                                across(se_hv109_recat_cor, .f=~ifelse(.x <1e-6,1e-6,.x))
                              )
                            
                          ) %>% 
                          group_by(ubigeo) %>% 
                          mutate(
                            id.sp = cur_group_id()
                          ) %>% 
                          ungroup()),
    
    
    wi_censo_cpa_recat= map(.x = wi_directa_recat,
                      .f = ~distritos %>% 
                        full_join(
                          censo_pca_20 %>% 
                            left_join(.x) %>% 
                            
                            mutate(
                              across(.cols = hv270_recat, .f = ~.x * (1 - 2 * epsilon) + epsilon, .names = '{.col}_cor'),
                              
                              across(.cols = hv270_recat_cor, .f =~logit(.x), .names = '{.col}_logit'),
                              
                              se_hv270_recat_cor = (se^2)/(hv270_recat_cor^2 * (1-hv270_recat_cor)^2)
                            ) %>% 
                            
                            mutate(
                              across(se_hv270_recat_cor, .f=~ifelse(.x <1e-6,1e-6,.x))
                            )
                          
                        ) %>% 
                        group_by(ubigeo) %>% 
                        mutate(
                          id.sp = cur_group_id()
                        ) %>% 
                        ungroup())
  )



####################

edu_censo_final2<-
  direct_estimation_censo_total %>% 
  select(year,edu_censo_cpa_recat) %>%
  unnest(edu_censo_cpa_recat) %>% 
  ungroup() %>% 
  select(-geometry) %>% 
  left_join(aux_data) %>% ## adding auxiliary data
  inner_join(pop_dist) %>% ## Adding total population to the dataset (using malaria incidence data processed in the main repo)
  mutate(
    pop_landscan = round(pop_landscan),
    cases_number = round(hv109_recat_cor*pop_landscan),
    year_re = as.integer(factor(year)), # for interaction spatial - time
    id.sp2 = id.sp # for interaction spatial - time
  )

wi_censo_final2<-
  direct_estimation_censo_total %>% 
  select(year,wi_censo_cpa_recat) %>%
  unnest(wi_censo_cpa_recat) %>% 
  ungroup() %>% 
  select(-geometry) %>% 
  left_join(aux_data) %>% ## adding auxiliary data
  inner_join(pop_dist) %>% ## Adding total population to the dataset (using malaria incidence data processed in the main repo)
  mutate(
    pop_landscan = round(pop_landscan),
    cases_number = round(hv270_recat_cor*pop_landscan),
    year_re = as.integer(factor(year)), # for interaction spatial - time
    id.sp2 = id.sp # for interaction spatial - time
  )

######################
loreto.dist <- poly2nb(distritos)
w.loreto <- nb2mat(loreto.dist, style = "W")
######################

modelo_fh2_spatial_edu_2 <- inla(
  
  formula =  cases_number ~ 1 + pc1 + pc2 +
    f(id.sp, model = "bym2", graph = w.loreto) +
    f(year_re, model = "ar1") +
    f(id.sp2, model = "bym2", graph = w.loreto, group = year_re,
      control.group = list(model = "ar1")),
  
  data = edu_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE)
)

modelo_fh2_spatial_wi_2 <- inla(
  formula = cases_number ~ 1 + pc1 + pc2 + 
    f(id.sp, model = "bym2", graph = w.loreto) +
    f(year_re, model = "ar1") +
    f(id.sp2, model = "bym2", graph = w.loreto, group = year_re,
      control.group = list(model = "ar1")),
  
  data = wi_censo_final2,
  family = "nbinomial",
  offset = log(pop_landscan),  
  
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE)
)

#############################

edu_prop_dist<-
  edu_censo_final2 %>% 
  mutate(
    edu_prop = (modelo_fh2_spatial_edu_2$summary.fitted.values$mean)/pop_landscan
  ) %>% 
  select(
    year,ubigeo,edu_prop
  )

write.csv(edu_prop_dist,"./data/aux_data/_other/edu_prop_dist.csv",row.names = F)

wi_prop_dist<-
  wi_censo_final2 %>% 
  mutate(
    wi_prop = (modelo_fh2_spatial_wi_2$summary.fitted.values$mean)/pop_landscan
  ) %>% 
  select(
    year,ubigeo,wi_prop
  )

write.csv(wi_prop_dist,"./data/aux_data/_other/wi_prop_dist.csv",row.names = F)
