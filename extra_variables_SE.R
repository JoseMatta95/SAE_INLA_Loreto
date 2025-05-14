library(foreign)
library(tidyverse)
library(janitor)
library(innovar)

# 1. CENSO EDUCATIVO - MINEDU ----

data("Peru")
distritos<- Peru %>% filter(dep == "LORETO") %>% select(ubigeo)

distritos_years<-
  crossing(
  ubigeo = distritos$ubigeo,
  year = 2010:2020
)

leer_dbf_desde_zips <- function(urls) {
  purrr::map(urls, function(url) {
    temp_zip <- tempfile(fileext = ".zip")
    dir_temp <- tempdir()
    
    # Descargar el ZIP
    download.file(url, destfile = temp_zip, mode = "wb", quiet = TRUE)
    
    # Extraer el archivo .dbf
    archivo_extraido <- unzip(temp_zip, exdir = dir_temp)
    
    # Leer 
    foreign::read.dbf(archivo_extraido, as.is = TRUE)
  })
}


direccion<-c(
  "https://escale.minedu.gob.pe/documents/10156/c94c8471-173f-449e-baca-b0b226acd61d", # 2010,
"https://escale.minedu.gob.pe/documents/10156/0dca4201-c2e6-4fbe-aca0-775956638540", # 2011
"https://escale.minedu.gob.pe/documents/10156/b12f6293-3029-4cc3-9504-eff8ca224485", # 2012
"https://escale.minedu.gob.pe/documents/10156/04be227c-ca6d-4a8b-80d0-6114080f1889", # 2013
"https://escale.minedu.gob.pe/documents/10156/409deb8d-c51d-4dc0-9990-ba2e41ba0f98", # 2014
"https://escale.minedu.gob.pe/documents/10156/26b7428c-0935-440b-9345-7f68c62eb6e5", # 2015
"https://escale.minedu.gob.pe/documents/10156/984aad53-4a63-4483-b74f-3f2c8a5da1f2", # 2016 revisar
"https://escale.minedu.gob.pe/documents/10156/ef04e0d2-4873-4ae6-9a68-bde22b7aad8d", # 2017
"https://escale.minedu.gob.pe/documents/10156/47e28853-9f24-46c2-91fc-a39c7479442f", # 2018
"https://escale.minedu.gob.pe/documents/10156/b2f1dea2-24b4-46f3-836f-a096fb74f7ea", # 2019
"https://escale.minedu.gob.pe/documents/10156/0a5ffa9c-20d3-4611-9faf-d1ca768a4d91") # 2020


  

edu_minedu<-
  map(.x = leer_dbf_desde_zips(urls =direccion),
    .f = ~ .x %>%
      clean_names() %>% 
      mutate(
        ubigeo = as.character(codgeo),
        dep = substr(ubigeo,1,2)
      ) %>% 
      filter(dep == 16) %>% 
      as_tibble() %>% 
      group_by(ubigeo,nroced) %>% 
      nest() %>% 
      mutate(
        suma_total = map_dbl(
          .x = data,
          .f = ~.x %>% 
            select(starts_with('d')&where(is.numeric)) %>% 
            summarise(suma = sum(across(everything(), 
                                        ~as.numeric(.), 
                                        .names = "suma"), na.rm = T)) %>% 
            pull(suma)
        )
      ) %>% 
      ungroup() %>% 
      
      group_by(ubigeo) %>% 
      summarise(total_dist_mat = sum(suma_total)))




year <- c(2010:2020)

edu_minedu_year<-
  map2_df(.x = edu_minedu,
     .y = year,
     .f = ~ .x %>% mutate(year = .y))


edu_minedu_year<-
  edu_minedu_year %>% 
  mutate(
    ubigeo = case_when(ubigeo == "160204" ~ "160702", # ubigeos that changed after 2015
                       ubigeo == "160203" ~ "160701",
                       ubigeo == "160207" ~ "160703",
                       ubigeo == "160208" ~ "160704",
                       ubigeo == "160209" ~ "160705",
                       ubigeo == "160114" ~ "160803",
                       
                       T ~ ubigeo)
  ) %>% 
  full_join(distritos_years) %>% 
  
  group_by(year) %>% 
  mutate(
    total_dist_mat = round(ifelse(is.na(total_dist_mat), 
                                  total_dist_mat[ubigeo=="160109"]/3,total_dist_mat))
    # supuesto: los distritros 1608XX creados a partir de 2014, asigarle el valor de 160109 entre 3
  ) %>% 
  ungroup() %>% 
  filter(ubigeo!=160109)

