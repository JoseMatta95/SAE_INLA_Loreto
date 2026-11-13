# ENDES.PE MOD ----
# Updated version of https://github.com/horaciochacon/ENDES.PE

consulta_endes2 <- function(periodo, codigo_modulo, base, guardar = FALSE, ruta = "", codificacion=NULL) {
  temp <- tempfile() ; tempdir <- tempdir()
  versiones <- matrix(c(2025,1038,2024,968,2023,910,2022,786,2021,760,2020, 739,
                        2019, 691,
                        2018, 638, 2017,605,2016,548,2015,504,2014,441,
                        2013,407,2012,323,2011,290,2010,260,
                        2009,238,2008,209,2007,194,2006,183,
                        2005,150,2004,120),byrow = T,ncol = 2)
  codigo_encuesta <- versiones[versiones[,1] == periodo,2]
  ruta_base <- "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/"
  modulo <- paste("-modulo",codigo_modulo,".zip",sep = "")
  url <- paste(ruta_base,codigo_encuesta,modulo,sep = "")
  utils::download.file(url,temp)
  archivos <- utils::unzip(temp,list = T)
  
  base_buscar <- if (periodo >= 2023) paste0(base, "_", periodo) else base
  
  archivos <- archivos[stringr::str_detect(archivos$Name, paste0(base_buscar,"\\.")) == TRUE,]
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

# AGGEGATED MEANS PREDICT ----

# Aggregates district-year SAE predictions (pred rows of the fit) to department
# level, weighting each district by population, within each posterior sample.
# pred_data: one row per pred_idx (same order), with columns year and `peso`.

resumen_anual_samples <- function(modelo_inla, pred_data, pred_idx,
                                   peso = "pop", n_sim = 1000) {

  stopifnot(nrow(pred_data) == length(pred_idx),
            !anyNA(pred_data[[peso]]))

  samples <- INLA::inla.posterior.sample(n = n_sim, result = modelo_inla)
  years   <- sort(unique(pred_data$year))
  w_all   <- pred_data[[peso]]

  prev_matrix <- sapply(samples, function(s) {

    nms <- rownames(s$latent)

    pred_rows <- grep("^APredictor", nms)
    if (length(pred_rows) == 0) pred_rows <- grep("^Predictor", nms)
    p <- plogis(as.numeric(s$latent[pred_rows])[pred_idx])

    vapply(years, function(yr) {
      sel <- pred_data$year == yr
      sum(p[sel] * w_all[sel]) / sum(w_all[sel])
    }, numeric(1))
  })

  tibble::tibble(
    year  = years,
    media = rowMeans(prev_matrix, na.rm = TRUE),
    li_95 = apply(prev_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
    ls_95 = apply(prev_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
  )
}

# DISTRICT PREDICTIONS FROM STRATA ----

# Models with an urban/rural effect predict each district-year twice. Both are
# combined with the population shares (w_stratum) within each posterior sample,
# so the summaries are for the district (Wakefield et al. 2020, eq. 12).
# pred_data: one row per pred_idx (same order), with ubigeo, year, id.sp, id.time
# and w_stratum.

district_samples <- function(modelo_inla, pred_data, pred_idx, label, n_sim = 5000) {

  w <- if ("w_stratum" %in% names(pred_data)) pred_data$w_stratum else 1

  stopifnot(nrow(pred_data) == length(pred_idx), !anyNA(w))

  samples <- INLA::inla.posterior.sample(n = n_sim, result = modelo_inla)

  p_matrix <- sapply(samples, function(s) {
    nms <- rownames(s$latent)
    pred_rows <- grep("^APredictor", nms)
    if (length(pred_rows) == 0) pred_rows <- grep("^Predictor", nms)
    plogis(as.numeric(s$latent[pred_rows])[pred_idx])
  })

  district <- rowsum(p_matrix * w,
                     group = paste(pred_data$ubigeo, pred_data$year),
                     reorder = FALSE)

  pred_data %>%
    dplyr::distinct(ubigeo, year, id.sp, id.time) %>%
    dplyr::mutate(
      model    = label,
      mean     = rowMeans(district),
      median   = apply(district, 1, median),
      lower    = apply(district, 1, quantile, probs = 0.025),
      upper    = apply(district, 1, quantile, probs = 0.975),
      sd       = apply(district, 1, sd),
      ci_width = upper - lower
    )
}

# Same combination for point predictions (output of loo_district_cv)

district_mean <- function(df) {
  df %>%
    dplyr::group_by(ubigeo, year, model) %>%
    dplyr::summarise(mean = sum(w_stratum * mean), .groups = "drop")
}

# LEAVE-ONE-DISTRICT-OUT CV ----

# For each sampled district, its observations (all years) are set to NA, the
# model is refitted and the district's prediction rows are returned. Mesh, A
# matrix and graph stay fixed: only the response changes.
# data: INLA data (stack data list or data.frame) with columns y and n.
# obs_idx / pred_idx: rows of `data` for the observations / prediction grid.
# obs_ubigeo: district of each observation (same order as obs_idx).
# pred_grid: ubigeo and year of each prediction row (same order as pred_idx).
# If it has w_stratum, the column is returned for district_mean().

loo_district_cv <- function(formula, label, data, obs_idx, pred_idx,
                            obs_ubigeo, pred_grid, family, A = NULL,
                            control.family = list(),
                            control.inla = list(int.strategy = "ccd"),
                            districts = sort(unique(obs_ubigeo))) {

  stopifnot(length(obs_idx)  == length(obs_ubigeo),
            length(pred_idx) == nrow(pred_grid))

  ctrl_pred <- list(compute = TRUE, link = 1)
  if (!is.null(A)) ctrl_pred$A <- A

  purrr::map_dfr(districts, function(d) {
    cat(label, "- district", d, "(", match(d, districts), "/", length(districts), ")\n")

    dat  <- data
    hold <- obs_idx[obs_ubigeo == d]
    dat$y[hold] <- NA
    dat$n[hold] <- NA

    fit <- INLA::inla(
      formula,
      family            = family,
      Ntrials           = n,
      data              = dat,
      control.family    = control.family,
      control.predictor = ctrl_pred,
      control.inla      = control.inla,
      verbose           = FALSE
    )

    keep <- pred_grid$ubigeo == d
    fv   <- fit$summary.fitted.values[pred_idx[keep], ]
    tibble::tibble(ubigeo = d, year = pred_grid$year[keep], model = label,
                   mean  = fv$mean,
                   lower = fv$`0.025quant`,
                   upper = fv$`0.975quant`,
                   w_stratum = pred_grid$w_stratum[keep])
  })
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
