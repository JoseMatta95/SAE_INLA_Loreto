
# RAW DATA PROCESSING OF ENDES AND CENSUS DATABASES (2010–2020)----

# 1. ENDES----
# endes data from 2010-2015: number of districts in Loreto before 2014 was 51. 
# Since 2015, Loreto have 53 districts (Creation of Putumayo province with 4 districts).

#devtools::install_github("horaciochacon/ENDES.PE")

library(ENDES.PE)
library(tidyverse)
library(haven)
library(janitor)
library(data.table)
library(fastDummies)
library(factoextra)

source("./_functions/functions.R") # error in consulta_endes function has been fixed

wi_edu_2016_2019<-
  map_df(.x = c(2009:2019),
         .f = ~consulta_endes2(periodo = .x,
                               codigo_modulo = 64,
                               base = 'RECH1',
                               guardar = F) %>% 
           left_join(consulta_endes2(periodo = .x,
                                     codigo_modulo = 65,
                                     base = 'RECH23', guardar = F) %>% 
                       mutate(
                         year = .x
                       ), by = c("HHID")) %>% 
           
           left_join(consulta_endes2(periodo = .x, 
                                     codigo_modulo = 64, 
                                     base = 'RECH0', guardar = F ), by = "HHID")%>% 
           select(year,everything())) %>%
  
  
  clean_names() %>% 
  select(shdistri,
         shprovin,
         year,
         hv001, #conglomerado
         hv002, #vivienda
         hv004, #unidad de muestreo
         hv023, #dominio - region
         ubigeo,
         hv005, # factor de ponderacion hogar
         hv022, #estrato
         hv025,
         #longitudx,
         #latitudy,
         
         hv109, # nivel educativo alcanzado
         hv106,
         hv270, # indice de riqueza
         hv271
  ) %>% 
  
  filter(hv023 == 16) %>% 
  
  mutate(
    cod_dist = ifelse(shdistri<10,paste0("0",shdistri),shdistri),
    cod_prov = ifelse(shprovin<10,paste0("0",shprovin),shprovin),
    ubigeo = paste0(hv023,cod_prov,cod_dist),
    hv270 = as.character(hv270),
    hv109 = as.character(hv109),
    
    hv109= case_when(hv109 == 0 | hv109 == 8 ~ "NE", # EDUCATION LEVEL
                     hv109 == 2 | hv109 == 1 ~ "PE",
                     hv109 == 3 | hv109 == 4 ~ "SE",
                     hv109 == 5 ~ "SuE",
                     TRUE ~ NA),
    
    hv270 = case_when(hv270 == 1 ~ "1st", # WEALTH INDEX
                      hv270 == 2 ~ "2nd",
                      hv270 == 3 ~ "3rd",
                      hv270 == 4 ~ "4th",
                      hv270 == 5 ~ "5th",
                      T ~ NA),
    
    hv109_recat = ifelse(hv109 == "NE",0,1), # at least primary education
    
    hv270_recat = ifelse(hv270 == "1st" | hv270 == "2nd",1,0), # at least 3rd WI
    
    hv025 = ifelse(hv025 == 1,0,1)
    
  ) %>% 
  zap_labels()


# since 2020, module codification number has been modified in INEI web. 
# for example, housing module in 2019 was 64, but now it is 1629

wi_edu_2020<-
  map_df(.x = c(2020:2021),
         .f = ~consulta_endes2(periodo = .x,
                               codigo_modulo = 1629,
                               base = 'RECH1',
                               guardar = F) %>% 
           left_join(consulta_endes2(periodo = .x,
                                     codigo_modulo = 1630,
                                     base = 'RECH23', guardar = F) %>% 
                       mutate(
                         year = .x
                       ), by = c("HHID")) %>% 
           
           left_join(consulta_endes2(periodo = .x, 
                                     codigo_modulo = 1629, 
                                     base = 'RECH0', guardar = F ), by = "HHID")%>% 
           select(year,everything()) %>% 
           clean_names() ) %>% 
  select(
    year,
    hv001, #conglomerado
    hv002, #vivienda
    hv004, #unidad de muestreo
    hv023, #dominio - region
    ubigeo,
    hv005, # factor de ponderacion hogar
    hv022, #estrato
    hv025,
    #longitudx,
    #latitudy,
    
    hv109, # nivel educativo alcanzado
    hv106,
    hv270,
    hv271# indice de riqueza)
  ) %>% 
  
  filter(hv023 == 16) %>% 
  
  mutate(
    hv270 = as.character(hv270),
    hv109 = as.character(hv109),
    
    hv109= case_when(hv109 == 0 | hv109 == 8 ~ "NE",
                     hv109 == 2 | hv109 == 1 ~ "PE",
                     hv109 == 3 | hv109 == 4 ~ "SE",
                     hv109 == 5 ~ "SuE",
                     TRUE ~ NA),
    
    
    hv270 = case_when(hv270 == 1 ~ "1st",
                      hv270 == 2 ~ "2nd",
                      hv270 == 3 ~ "3rd",
                      hv270 == 4 ~ "4th",
                      hv270 == 5 ~ "5th",
                      T ~ NA),
    
    hv109_recat = ifelse(hv109 == "NE",0,1), # at least primary education
    
    hv270_recat = ifelse(hv270 == "1st" | hv270 == "2nd",1,0), # at least 3rd WI
    
    hv025 = ifelse(hv025 == 1,0,1)
  ) %>% 
  zap_labels()

# merge datasets 

wi_edu_2010_2020<-
  bind_rows(wi_edu_2016_2019,wi_edu_2020) %>% 
  mutate(
    ubigeo = case_when(ubigeo == "160204" ~ "160702", # ubigeos that changed after 2015
                       ubigeo == "160203" ~ "160701",
                       ubigeo == "160207" ~ "160703",
                       ubigeo == "160208" ~ "160704",
                       ubigeo == "160209" ~ "160705",
                       ubigeo == "160114" ~ "160803",
                       
                       T ~ ubigeo),
    
    ubigeo_prov = substr(ubigeo, 1, 4),
    
    hv005 = hv005/100000
  ) %>% 
  filter(
    ubigeo != "160109" #drop ubigeo 160109, becausa it changed to 1608xx in 2016 
  ) %>% 
  mutate(
  ) %>% 
  as_tibble()

write.csv(wi_edu_2010_2020 %>% filter(year%in%c(2010:2020)),"./data/wi_edu_2010_2020.csv", row.names = F)
write.csv(wi_edu_2010_2020,"./data/aux_data/_other/wi_edu_2009_2021.csv", row.names = F)

# 2. CENSOS----

## CENSO 2007

## CENSO 2017

## census data 2017 ----
censo_hogares <- read_sav("../Google Drive/Mi unidad/censo2017_data/CPV2017_HOG.sav", 
                          col_select = c("ubigeo","area",'departamento','distrito',
                                         starts_with('c3_p1'),starts_with("c3_p2"),'c4_p1')) %>% 
  filter(departamento == "LORETO") %>% 
  zap_labels()

censo_hogar_dist<-
  censo_hogares %>% 
  group_by(ubigeo) %>% 
  mutate(
    across(.cols = c3_p2_1:c3_p2_16, .f = ~ifelse(.x == 1,0,1)),
    ubigeo = as.character(ubigeo)
  ) %>% 
  summarise(
    across(.cols = c3_p1_1:c4_p1, .f = ~mean(.x,na.rm = T))
  )

#write.csv(censo_hogares, "./data processing/INLA_SAE/data/censo_2017/censo_hogar_2017.csv", row.names = F)

censo_vivienda <- read_sav("../Google Drive/Mi unidad/censo2017_data/CPV2017_VIV.sav", 
                           col_select = c("ubigeo","area",'departamento','distrito',
                                          starts_with('c2_p'),'t_c4_p1')) %>% 
  select(-c2_p2,-c2_p7,-c2_p7a,-c2_p7b,-c2_p7c,-c2_p8,-c2_p9) %>% 
  filter(departamento == "LORETO") %>% 
  zap_labels()

censo_vivienda_dist<-
  censo_vivienda %>% 
  mutate(
    c2_p11 = ifelse(c2_p11==1,0,1),
    area = ifelse(area==1,0,1)
  ) %>% 
  filter(!is.na(c2_p3)) %>% 
  dummy_cols(select_columns = c("c2_p1","c2_p3","c2_p4","c2_p5","c2_p6",
                                "c2_p10")) %>% 
  group_by(ubigeo) %>%
  
  summarise(
    across(.cols = c(c2_p1:c2_p10_8), .f = ~mean(.x, na.rm=T))
  ) %>% 
  select(-(c2_p1:c2_p10))

#write.csv(censo_vivienda, "./data processing/INLA_SAE/data/censo_2017/censo_vivienda_2017.csv", row.names = F)

censo_poblacion <- read_sav("../Google Drive/Mi unidad/censo2017_data/CPV2017_POB.sav", 
                            col_select = c("ubigeo","area",'departamento','distrito',
                                           starts_with('c5_p8'),'c5_p11','c5_p12',
                                           'c5_p13_niv','c5_p15','c5_p16','c5_p23')) %>% 
  filter(departamento == "LORETO") %>% 
  zap_labels()

censo_poblacion_dist<-
  censo_poblacion %>% 
  mutate(
    c5_p11_recat = ifelse(c5_p11 ==10,1,
                          ifelse(c5_p11 == 1,2,
                                 ifelse(c5_p11==2,3,4))),
    
    c5_p13_niv_recat = ifelse(c5_p13_niv ==2,1,
                              ifelse(c5_p13_niv == 5,3,
                                     ifelse(c5_p13_niv>5,6,c5_p13_niv))),

    
    across(.cols = c(c5_p12,c5_p15,c5_p16,c5_p23), .f = ~ifelse(.x ==1,0,1))
    
  ) %>% 
  select(-c5_p11,-c5_p13_niv) %>% 
  filter(!is.na(c5_p13_niv_recat)) %>% 
  
  dummy_cols(select_columns = c("c5_p11_recat","c5_p13_niv_recat")) %>% 
  
  group_by(ubigeo) %>% 
  summarise(
    across(.cols = c5_p8_1:c5_p13_niv_recat_6, .f = ~mean(.x, na.rm=T))
  )

#write.csv(censo_poblacion, "./data processing/INLA_SAE/data/censo_2017/censo_poblacion_2017.csv", row.names = F)


### final dataset CENSO-distrito 2017 ----

censo_final_loreto_2017<-
  censo_hogar_dist %>% 
  full_join(censo_vivienda_dist) %>% 
  full_join(censo_poblacion_dist) %>% 
  mutate(
    ubigeo = as.character(ubigeo)
  )
#write.csv(censo_final_loreto_2017, "./data/censo_2017/01.censo_final_loreto_2017.csv", row.names = F)


## PCA
# censo_final_loreto_2017 has 95 variables. we used PCA  to address the problem

censo_loreto_sinid<- 
  censo_final_loreto_2017 %>% 
  select(-ubigeo,-c3_p1_3,-c3_p1_6) # matrix sin id

#scaled_censo_loreto_sinid <- scale(censo_loreto_sinid) # escalar valores

pca <- prcomp(censo_loreto_sinid, center = TRUE, scale. = TRUE) 
summary(pca) # varianza explicada
plot(pca, type = "l", main = "Scree Plot")
pca1<-fviz_pca_var(pca, col.var = "contrib", repel = TRUE)


library(ggplot2)

# varianza explicada
var_exp <- pca$sdev^2 / sum(pca$sdev^2)

df_scree <- data.frame(
  PC = factor(1:length(var_exp)),
  VarExp = var_exp
)

pca2<-ggplot(df_scree %>% filter(PC %in% c(1:15)), aes(x = PC, y = VarExp)) +
  geom_line(group = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Scree Plot",
    x = "Principal Component",
    y = "Explained Variance (%)"
  ) +
  theme_bw()

cowplot::plot_grid(pca1,pca2, labels = c("A","B"))
ggsave("./figures/sm_fig3.png", dpi = 800, bg = "white", width = 18, height = 9)
## devolviendo id y agregando outcome

censo_pca_20<-
  bind_cols(censo_final_loreto_2017$ubigeo,pca$x[,1:20]) %>% 
  clean_names() %>% 
  rename(ubigeo = x1) %>% 
  mutate(ubigeo = as.character(ubigeo))

write.csv(censo_pca_20, "./data/censo_2017/censo_2017_pca_20.csv", row.names = F)

## census data 2007 ----

