# WI m3 without rural (main model) vs. with rural vs. with rural and without pc1

library(INLA)
library(tidyverse)
library(gt)

source("./_functions/functions.R")

# m3 with rural ----

rural_frac<- read.csv("./data/censo_2017/rural_frac_encarea.csv") %>%
  mutate(ubigeo = as.character(ubigeo))

cluster_rural<- cluster_wi_pca %>%
  left_join(wi_edu %>% distinct(hv001, ubigeo, year, rural = hv025),
            by = c("hv001", "ubigeo", "year"))

pred_rural<- pred_grid_pca %>%
  crossing(rural = c(0, 1)) %>%
  left_join(rural_frac, by = "ubigeo") %>%
  mutate(w_stratum = if_else(rural == 1, f_rural, 1 - f_rural))

inla_data_rural<- bind_rows(
  cluster_rural %>% select(ubigeo, year, id.sp, id.time, y, n, pc1, rural),
  pred_rural %>% mutate(y = NA, n = NA_integer_) %>%
    select(ubigeo, year, id.sp, id.time, y, n, pc1, rural)
) %>% mutate(id.sp2 = id.sp)

pred_idx_rural<- (nrow(cluster_rural) + 1):nrow(inla_data_rural)

formula_m3_rural<- y ~ 1 + rural + pc1 +
  f(id.sp, model = "bym2", graph = inla_graph,
    scale.model = TRUE, hyper = pc_bym2) +
  f(id.time, model = "rw2", scale.model = TRUE, hyper = pc_prec) +
  f(id.sp2, model = "besag", graph = inla_graph, scale.model = TRUE,
    group = id.time, control.group = st_group, hyper = pc_prec)

fit_rural<- inla(
  formula_m3_rural,
  family = "betabinomial",
  Ntrials = inla_data_rural$n,
  data = inla_data_rural,
  control.family = prior_bb_rho,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
  control.inla = ctrl_inla_wi,
  verbose = FALSE
)

pred_m3_rural<- district_samples(fit_rural, pred_rural, pred_idx_rural, "rural")

# Reruns ----

rerun1_norural<- inla.rerun(fit_bb_m3)
rerun2_norural<- inla.rerun(rerun1_norural)
rerun3_norural<- inla.rerun(rerun2_norural)
rerun4_norural<- inla.rerun(rerun3_norural)

rerun1_rural<- inla.rerun(fit_rural)
rerun2_rural<- inla.rerun(rerun1_rural)
rerun3_rural<- inla.rerun(rerun2_rural)
rerun4_rural<- inla.rerun(rerun3_rural)

# Hyperparameters

list(fit = fit_bb_m3, rerun1 = rerun1_norural, rerun2 = rerun2_norural,
     rerun3 = rerun3_norural, rerun4 = rerun4_norural) %>%
  imap_dfr(~ .x$summary.hyperpar %>%
             rownames_to_column("term") %>%
             mutate(fit = .y)) %>%
  mutate(prec = str_starts(term, "Precision"),
         median = if_else(prec, 1/sqrt(`0.5quant`), `0.5quant`),
         lower = if_else(prec, 1/sqrt(`0.975quant`), `0.025quant`),
         upper = if_else(prec, 1/sqrt(`0.025quant`), `0.975quant`),
         term = str_replace(term, "Precision", "SD"),
         value = sprintf("%.3f (%.3f, %.3f)", median, lower, upper)) %>%
  select(term, fit, value) %>%
  pivot_wider(names_from = fit, values_from = value) %>%
  gt() %>%
  tab_header(title = md("**WI m3 without rural — hyperparameters along the reruns**"))

list(fit = fit_rural, rerun1 = rerun1_rural, rerun2 = rerun2_rural,
     rerun3 = rerun3_rural, rerun4 = rerun4_rural) %>%
  imap_dfr(~ .x$summary.hyperpar %>%
             rownames_to_column("term") %>%
             mutate(fit = .y)) %>%
  mutate(prec = str_starts(term, "Precision"),
         median = if_else(prec, 1/sqrt(`0.5quant`), `0.5quant`),
         lower = if_else(prec, 1/sqrt(`0.975quant`), `0.025quant`),
         upper = if_else(prec, 1/sqrt(`0.025quant`), `0.975quant`),
         term = str_replace(term, "Precision", "SD"),
         value = sprintf("%.3f (%.3f, %.3f)", median, lower, upper)) %>%
  select(term, fit, value) %>%
  pivot_wider(names_from = fit, values_from = value) %>%
  gt() %>%
  tab_header(title = md("**WI m3 with rural — hyperparameters along the reruns**"))

# With rural vs. main model 

sampled<- cluster_wi_pca %>%
  distinct(ubigeo, year) %>%
  mutate(sampled = TRUE)

pred_m3_rural %>%
  select(ubigeo, year, mean) %>%
  left_join(pred_bb_m3 %>% select(ubigeo, year, ref_mean = mean),
            by = c("ubigeo", "year")) %>%
  left_join(sampled, by = c("ubigeo", "year")) %>%
  mutate(sampled = replace_na(sampled, FALSE)) %>%
  group_by(sampled) %>%
  summarise(
    n = n(),
    mean_abs_diff = mean(abs(mean - ref_mean)),
    cor_mean = cor(mean, ref_mean),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(title = md("**District-year predictions — difference from the main model (without rural)**")) %>%
  fmt_number(columns = c(mean_abs_diff, cor_mean), decimals = 3)
