# ENDES.PE MOD ----
# Updated version of https://github.com/horaciochacon/ENDES.PE

consulta_endes2 <- function(periodo, codigo_modulo, base, guardar = FALSE, ruta = "", codificacion=NULL) {
  temp <- tempfile() ; tempdir <- tempdir()

  versiones <- matrix(c(2024,968,2023,910,2022,786,2021,760,2020, 739,
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
  archivos <- archivos[stringr::str_detect(archivos$Name, paste0(base,"\\.")) == TRUE,]

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

# RECONSTRUCT ETA SPDE ----

reconstruir_eta_spde <- function(sample, A_obs, data) {

  latent <- sample$latent
  nms    <- rownames(latent)

  u_idx <- grep("^i\\.field:", nms)
  eta   <- as.numeric(A_obs %*% latent[u_idx])

  int_idx <- grep("^intercept:", nms)
  eta     <- eta + as.numeric(latent[int_idx])

  rw2_idx  <- grep("^id\\.time:", nms)
  rw2_vals <- as.numeric(latent[rw2_idx])
  eta      <- eta + rw2_vals[data$id.time]

  prov_idx <- grep("^id\\.prov:", nms)
  if (length(prov_idx) > 0) {
    prov_vals <- as.numeric(latent[prov_idx])
    eta       <- eta + prov_vals[data$id.prov]
  }

  pc1_idx <- grep("^pc1:", nms)
  if (length(pc1_idx) > 0)
    eta <- eta + as.numeric(latent[pc1_idx]) * data$pc1

  pc2_idx <- grep("^pc2:", nms)
  if (length(pc2_idx) > 0)
    eta <- eta + as.numeric(latent[pc2_idx]) * data$pc2

  eta
}

# AGGEGATED MEANS PREDICT ----

resumen_anual_samples <- function(modelo_inla, data, spde,
                                   A_obs = NULL, obs_idx = NULL,
                                   n_sim = 1000) {

  samples <- INLA::inla.posterior.sample(n = n_sim, result = modelo_inla)
  years   <- sort(unique(data$year))

  prev_matrix <- sapply(samples, function(s) {

    if (spde) {
      p <- plogis(reconstruir_eta_spde(s, A_obs, data))
    } else {
      idx_all <- which(grepl("^Predictor", rownames(s$latent)))
      p       <- plogis(as.numeric(s$latent[idx_all])[obs_idx])
    }

    vapply(years, function(yr) {
      sel <- data$year == yr
      w   <- if (!is.null(data$sum_w)) data$sum_w[sel] else data$n[sel]
      sum(p[sel] * w) / sum(w)
    }, numeric(1))
  })

  tibble::tibble(
    year  = years,
    media = rowMeans(prev_matrix, na.rm = TRUE),
    li_95 = apply(prev_matrix, 1, quantile, probs = 0.025, na.rm = TRUE),
    ls_95 = apply(prev_matrix, 1, quantile, probs = 0.975, na.rm = TRUE)
  )
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
