# LCLU FUNCTION -------
lclu_mapbioma<-
  function(lctype,geometry,id_column = "iddist"){
  lctype <- as.numeric(lctype)
  lctypename <- case_when(lctype == 9 ~ "forestplant",
                          lctype == 15 ~ "livestock",
                          lctype == 18 ~ "agriculture",
                          lctype == 21 ~ "agromosaic",
                          lctype == 24 ~ "urban",
                          lctype == 35 ~ "oilpalm",
                          lctype == 30 ~ "mining",
                          T ~ NA)
  scale <- 30
  lcluname <- paste0("lclu","_",lctypename)
  
  mapbioma<-
    ee$Image("projects/mapbiomas-public/assets/peru/collection2/mapbiomas_peru_collection2_integration_v1")
  
  img_mapbioma<-
    mapbioma$select('classification_2010','classification_2011','classification_2012',
                    'classification_2013','classification_2014','classification_2015',
                    'classification_2016','classification_2017','classification_2018',
                    'classification_2019','classification_2020')$clip(geometry)$eq(lctype)
  
  img_mapbioma_areakm2<-
    img_mapbioma$multiply(ee$Image$pixelArea())$divide(1000000)
  
  
  mapbioma_loreto<-
    ee_extract(
      x=img_mapbioma_areakm2,
      y = geometry,
      fun = ee$Reducer$sum(),
      sf = T,
      scale = scale,
      quiet = TRUE
    ) %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    
    
    # long format
    pivot_longer(cols = -all_of((id_column))) %>%
    mutate(
      year = str_extract(name, "\\d{4}")
    ) %>% 
    rename_with(~ lcluname, .cols = "value") %>%
    select(-name) %>%
    select(year, all_of(id_column), everything()) 
  
  
  return(mapbioma_loreto)
  
  }

# DEFORESTATION FUNCTION ----


get_deforestation_geobosques<- function(periodo,geometry){ 
  
  if(length(periodo) ==1){
    
    if(periodo <= 2009){
      year_number = as.numeric(str_sub(as.character(periodo), -1))
    }else{
      year_number = as.numeric(str_sub(as.character(periodo), -2))
    }
    
    ee_deforestation<-ee$Image("projects/ee-josematta2/assets/Perdida_2001_2023")$select('b1')$eq(year_number)$
      multiply(ee$Image$pixelArea())$divide(1000000)
    
    
    deforestation<-
      ee_extract(
        ee_deforestation,
        geometry,
        fun = ee$Reducer$sum(),
        scale = 30
      ) %>% 
      as_tibble()
    
  }else if(length(periodo)>1){
    
    periodo = periodo %>% as.list()
    
    periodo2 = map(.x = periodo,
                   .f = ~if(.x <= 2009){
                     year_number = as.numeric(str_sub(as.character(.x), -1))
                   }else{
                     year_number = as.numeric(str_sub(as.character(.x), -2))
                   })
    
    img_list<-
      map(.x = periodo2,
          .f = ~ee$Image("projects/ee-josematta2/assets/Perdida_2001_2023")$select('b1')$eq(.x)$
            multiply(ee$Image$pixelArea())$divide(1000000))
    
    
    deforestation<-
      map_df(.x = img_list,
             .f = ~ee_extract(
               .x,
               geometry,
               fun = ee$Reducer$sum(),
               scale = 30
             )) %>% 
      as_tibble() 
    
    
    deforestation <- deforestation %>%
      mutate(year = as.numeric(rep(periodo, each = nrow(deforestation) / length(periodo))),
             
             deforest_km2 = b1) %>% 
      select(-b1,year,everything())
    
  }
  
  return(deforestation)
}

# ENDES.PE MOD ----

consulta_endes2 <- function(periodo, codigo_modulo, base, guardar = FALSE, ruta = "", codificacion=NULL) {
  # Generamos dos objetos temporales: un archivo y una carpeta 
  temp <- tempfile() ; tempdir <- tempdir()
  
  # Genera una matriz con el número identificador de versiones por cada año
  versiones <- matrix(c(2020, 739, 2019, 691, 
                        2018, 638, 2017,605,2016,548,2015,504,2014,441,
                        2013,407,2012,323,2011,290,2010,260,
                        2009,238,2008,209,2007,194,2006,183,
                        2005,150,2004,120),byrow = T,ncol = 2)
  
  # Extrae el código de la encuesta con la matriz versiones
  codigo_encuesta <- versiones[versiones[,1] == periodo,2]
  ruta_base <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/" # La ruta de microdatos INEI
  modulo <- paste("-modulo",codigo_modulo,".zip",sep = "")
  url <- paste(ruta_base,codigo_encuesta,modulo,sep = "")
  
  # Descargamos el archivo
  utils::download.file(url,temp)
  
  # Listamos los archivos descargados y seleccionamos la base elegida
  archivos <- utils::unzip(temp,list = T)
  archivos <- archivos[stringr::str_detect(archivos$Name, paste0(base,"\\.")) == TRUE,]
  
  # Elegimos entre guardar los archivos o pasarlos directamente a un objeto
  if(guardar == TRUE) {
    utils::unzip(temp, files = archivos$Name, exdir = paste(getwd(), "/", ruta, sep = ""))
    print(paste("Archivos descargados en: ", getwd(), "/", ruta, sep = ""))
  } 
  else {
    endes <- haven::read_sav(
      utils::unzip(
        temp, 
        files = archivos$Name[grepl(".sav|.SAV",archivos$Name)], 
        exdir = tempdir
      ), 
      encoding = codificacion
    )
    nombres <- toupper(colnames(endes))
    colnames(endes) <- nombres
    endes
  }
}

# AGGEGATED MEANS PREDICT----

resumen_anual_samples <- function(modelo_inla, data) {
  # 1. Extraer muestras posteriores del predictor lineal (η)
  samples <- INLA::inla.posterior.sample(n = 1000, result = modelo_inla)
  idx_distritos <- which(grepl("^Predictor", rownames(samples[[1]]$latent)))
  eta_matrix <- sapply(samples, function(x) x$latent[idx_distritos])  # matriz: n_obs x n_draws
  
  # 2. Convertir η a μ (escala original)
  mu_matrix <- exp(eta_matrix)
  
  # 3. Crear df auxiliar con año y población
  df_info <- data.frame(
    year = data$year,
    poblacion = data$pop_landscan
  )
  
  # 4. Sumar μ por año
  proporcion_anual <- function(mu_sample) {
    df_info$mu <- mu_sample
    df_info |>
      dplyr::group_by(year) |>
      dplyr::summarise(p_agregado = sum(mu), .groups = "drop") |>
      dplyr::arrange(year) |>
      dplyr::pull(p_agregado)
  }
  
  # 5. Matriz de μ agregados por año
  mu_anio_matrix <- apply(mu_matrix, 2, proporcion_anual)
  
  # 6. Población total por año
  pob_total_anio <- df_info |>
    dplyr::group_by(year) |>
    dplyr::summarise(pob = sum(poblacion), .groups = "drop") |>
    dplyr::arrange(year) |>
    dplyr::pull(pob)
  
  # 7. Calcular proporciones (μ / población)
  prop_anio_matrix <- sweep(mu_anio_matrix, 1, pob_total_anio, FUN = "/")
  
  # 8. Estadísticos resumen (media e IC 95%)
  p_anio_mean <- rowMeans(prop_anio_matrix)
  p_anio_li <- apply(prop_anio_matrix, 1, quantile, probs = 0.025)
  p_anio_ls <- apply(prop_anio_matrix, 1, quantile, probs = 0.975)
  
  # 9. Consolidar en un data frame
  df_anual <- data.frame(
    year = sort(unique(df_info$year)),
    media = p_anio_mean,
    li_95 = p_anio_li,
    ls_95 = p_anio_ls
  ) %>% 
    as_tibble()
  
  return(df_anual)
}



# get_legend2 FUNCTION ----


get_legend2 <- function(plot, legend = NULL) {
  gt <- if (inherits(plot, "ggplot")) ggplotGrob(plot)
  else if (inherits(plot, "grob")) plot
  else stop("No es ggplot ni grob")
  pattern <- "guide-box"
  if (!is.null(legend)) pattern <- paste0(pattern, "-", legend)
  idx <- grep(pattern, gt$layout$name)
  nonz <- which(!vapply(gt$grobs[idx], inherits, logical(1), "zeroGrob"))
  if (length(nonz)) return(gt$grobs[[idx[nonz[1]]]])
  return(NULL)
}

