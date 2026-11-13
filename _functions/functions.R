# ENDES.PE MOD ----
# Updated version of https://github.com/horaciochacon/ENDES.PE

consulta_endes2<- function(periodo, codigo_modulo, base, codificacion = NULL) {
  temp<- tempfile() ; tempdir<- tempdir()
  versiones<- matrix(c(2020,739,2019,691,2018,638,2017,605,2016,548,2015,504,
                       2014,441,2013,407,2012,323,2011,290,2010,260),
                     byrow = T, ncol = 2)
  codigo_encuesta<- versiones[versiones[,1] == periodo,2]
  ruta_base<- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/"
  modulo<- paste("-modulo",codigo_modulo,".zip",sep = "")
  url<- paste(ruta_base,codigo_encuesta,modulo,sep = "")
  utils::download.file(url,temp)
  archivos<- utils::unzip(temp,list = T)

  archivos<- archivos[stringr::str_detect(archivos$Name, paste0(base,"\\.")) == TRUE,]
  endes<- haven::read_sav(
    utils::unzip(
      temp,
      files = archivos$Name[grepl(".sav|.SAV",archivos$Name)],
      exdir = tempdir
    ),
    encoding = codificacion
  )
  nombres<- toupper(colnames(endes))
  colnames(endes)<- nombres
  endes
}

# AGGREGATED MEANS PREDICT ----

resumen_anual_samples<- function(modelo_inla, pred_data, pred_idx, n_sim = 1000) {

  samples<- INLA::inla.posterior.sample(n = n_sim, result = modelo_inla)
  years<- sort(unique(pred_data$year))
  w_all<- pred_data$pop

  prev_matrix<- sapply(samples, function(s) {

    nms<- rownames(s$latent)

    pred_rows<- grep("^APredictor", nms)
    if (length(pred_rows) == 0) pred_rows<- grep("^Predictor", nms)
    p<- plogis(as.numeric(s$latent[pred_rows])[pred_idx])

    vapply(years, function(yr) {
      sel<- pred_data$year == yr
      sum(p[sel] * w_all[sel]) / sum(w_all[sel])
    }, numeric(1))
  })

  tibble::tibble(
    year = years,
    media = rowMeans(prev_matrix),
    li_95 = apply(prev_matrix, 1, quantile, probs = 0.025),
    ls_95 = apply(prev_matrix, 1, quantile, probs = 0.975)
  )
}

# DISTRICT PREDICTIONS FROM STRATA ----

district_samples<- function(modelo_inla, pred_data, pred_idx, label, n_sim = 5000) {

  w<- pred_data$w_stratum

  samples<- INLA::inla.posterior.sample(n = n_sim, result = modelo_inla)

  p_matrix<- sapply(samples, function(s) {
    nms<- rownames(s$latent)
    pred_rows<- grep("^APredictor", nms)
    if (length(pred_rows) == 0) pred_rows<- grep("^Predictor", nms)
    plogis(as.numeric(s$latent[pred_rows])[pred_idx])
  })

  district<- rowsum(p_matrix * w,
                    group = paste(pred_data$ubigeo, pred_data$year),
                    reorder = FALSE)

  pred_data %>%
    dplyr::distinct(ubigeo, year, id.sp, id.time) %>%
    dplyr::mutate(
      model = label,
      mean = rowMeans(district),
      median = apply(district, 1, median),
      lower = apply(district, 1, quantile, probs = 0.025),
      upper = apply(district, 1, quantile, probs = 0.975),
      sd = apply(district, 1, sd),
      ci_width = upper - lower
    )
}

# LEAVE-ONE-DISTRICT-OUT CV ----

loo_district_cv<- function(formula, label, data, obs_idx, pred_idx,
                           obs_ubigeo, pred_grid, family, A = NULL,
                           control.family = list(),
                           control.inla = list(int.strategy = "ccd")) {

  ctrl_pred<- list(compute = TRUE, link = 1)
  if (!is.null(A)) ctrl_pred$A<- A

  purrr::map_dfr(sort(unique(obs_ubigeo)), function(d) {

    dat<- data
    hold<- obs_idx[obs_ubigeo == d]
    dat$y[hold]<- NA
    dat$n[hold]<- NA

    fit<- INLA::inla(
      formula,
      family = family,
      Ntrials = n,
      data = dat,
      control.family = control.family,
      control.predictor = ctrl_pred,
      control.inla = control.inla,
      verbose = FALSE
    )

    keep<- pred_grid$ubigeo == d
    fv<- fit$summary.fitted.values[pred_idx[keep], ]
    tibble::tibble(ubigeo = d, year = pred_grid$year[keep], model = label,
                   mean = fv$mean,
                   lower = fv$`0.025quant`,
                   upper = fv$`0.975quant`,
                   w_stratum = pred_grid$w_stratum[keep])
  })
}

# get_legend2 FUNCTION ----
# Taken from teunbrand's solution in https://github.com/wilkelab/cowplot/issues/202

get_legend2<- function(plot, legend = NULL) {
  gt<- ggplotGrob(plot)
  pattern<- "guide-box"
  if (!is.null(legend)) pattern<- paste0(pattern, "-", legend)
  idx<- grep(pattern, gt$layout$name)
  nonz<- which(!vapply(gt$grobs[idx], inherits, logical(1), "zeroGrob"))
  if (length(nonz)) return(gt$grobs[[idx[nonz[1]]]])
  return(NULL)
}
