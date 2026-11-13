# EDU 2017: Census vs. m3 at department, province and district level

library(INLA)
library(tidyverse)
library(haven)
library(gt)
library(sf)
library(innovar)

census_pob <- "~/Downloads/CPV 2017 DISTRITOS disponible en SPSS/censo2017_data/CPV2017_POB.sav"

# Census ----

census_district <- read_sav(census_pob,
                            col_select = c("ubigeo", "departamento", "provincia",
                                           "distrito", "c5_p13_niv")) %>%
  filter(departamento == "LORETO") %>%
  zap_labels() %>%
  group_by(ubigeo, departamento, provincia, distrito) %>%
  summarise(n_census   = n(),
            edu_census = sum(c5_p13_niv >= 3, na.rm = TRUE),
            .groups    = "drop")

census_area <- function(group) {
  census_district %>%
    group_by(area = .data[[group]]) %>%
    summarise(census = sum(edu_census) / sum(n_census))
}

# m3 ----

m3 <- readRDS("./data/models/edu_fits.rds")$m3

pred_2017 <- m3$pred_grid %>%
  mutate(pred_row = m3$pred_idx) %>%
  filter(year == 2017) %>%
  left_join(select(census_district, ubigeo, departamento, provincia, n_census), by = "ubigeo") %>%
  mutate(w = n_census * w_stratum)

stopifnot(!anyNA(pred_2017$w), !anyNA(pred_2017$provincia))

samples <- inla.posterior.sample(n = 5000, result = m3$fit)

p_matrix <- sapply(samples, function(s) {
  nms <- rownames(s$latent)
  pred_rows <- grep("^APredictor", nms)
  if (length(pred_rows) == 0) pred_rows <- grep("^Predictor", nms)
  plogis(as.numeric(s$latent[pred_rows])[pred_2017$pred_row])
})

model_area <- function(group) {
  g   <- pred_2017[[group]]
  agg <- rowsum(p_matrix * pred_2017$w, g, reorder = FALSE) /
    as.numeric(rowsum(pred_2017$w, g, reorder = FALSE))

  tibble(area  = rownames(agg),
         m3    = rowMeans(agg),
         lower = apply(agg, 1, quantile, probs = 0.025),
         upper = apply(agg, 1, quantile, probs = 0.975))
}

# Comparison ----

comparison <- tribble(
  ~level,       ~group,
  "department", "departamento",
  "province",   "provincia",
  "district",   "ubigeo"
) %>%
  mutate(data = map(group, ~ left_join(census_area(.x), model_area(.x), by = "area"))) %>%
  select(-group) %>%
  unnest(data) %>%
  left_join(select(census_district, ubigeo, distrito, provincia), by = c("area" = "ubigeo")) %>%
  mutate(ubigeo = if_else(level == "district", area, NA_character_),
         area = coalesce(distrito, area),
         diff = round((m3 - census),2),
         level = fct_inorder(level)) %>%
  select(-distrito)

comparison %>%
  filter(level != "district") %>%
  select(-provincia, -ubigeo) %>%
  gt() %>%
  tab_header(title = md("**EDU 2017 — Census vs. m3, department and province**")) %>%
  fmt_number(columns = c(census, m3, lower, upper, diff), decimals = 3)

comparison %>%
  filter(level == "district") %>%
  arrange(census) %>%
  mutate(across(c(census, m3, lower, upper, diff), ~ round(.x, 3))) %>%
  DT::datatable(filter = "top", options = list(pageLength = 53))

# Error maps ----

data(Peru)

map_district <- Peru %>%
  mutate(ubigeo = as.character(ubigeo)) %>%
  filter(dep == "LORETO") %>%
  left_join(comparison %>%
              filter(level == "district") %>%
              select(ubigeo, provincia, diff),
            by = "ubigeo")

map_province <- map_district %>%
  group_by(provincia) %>%
  summarise() %>%
  left_join(comparison %>%
              filter(level == "province") %>%
              select(provincia = area, diff),
            by = "provincia")

signed_limit <- max(abs(map_district$diff), na.rm = TRUE) * 100

plot_grid(
ggplot(map_district) +
  geom_sf(aes(fill = diff * 100), color = "white", linewidth = 0.2) +
  geom_sf(data = map_province, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-signed_limit, signed_limit),
                       name = "m3 - Census (pp)") +
  labs(title = "District") +
  theme_void() +
  theme(legend.position = "bottom"),

ggplot(map_province) +
  geom_sf(aes(fill = diff), color = "white", linewidth = 0.2) +
  geom_sf(data = st_union(map_province), fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_gradient(low = "#08306B", high = "#DCE9F5", limits = c(-0.06, 0),
                      name = "m3 - Census (pp)") +
  labs(title = "Province") +
  theme_void() +
  theme(legend.position = "bottom")
)

distrito<-
  comparison %>%
  filter(level == "district") %>% 
  mutate(
    provcode = substr(ubigeo,1,4),
    sampled  = ubigeo %in% (read.csv("./data/wi_edu_2010_2020.csv") %>% filter(year == 2017) %>% pull(ubigeo) %>% as.character())
  ) %>%
  ggplot()+
  geom_pointrange(aes(x = fct_rev(ubigeo), y = m3,
                      ymin = lower, ymax = upper, shape = sampled),
                  col = "red4",
                  alpha = .5, linewidth = 1) +
  geom_point(aes(x  = fct_rev(ubigeo), y = census), col = "blue4") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "Sampled 2017", "FALSE" = "Not sampled 2017"),
                     name = NULL) +
  facet_grid(provcode~.,scales = "free_y",
             switch = "y",space = "free_y") +
  labs( x= "ubigeo", y = "proportion") +
  coord_flip() +
  theme_classic() +
  theme(
    strip.placement = "outside"
  )

provincia<-
comparison %>%
  filter(level == "province") %>%
  left_join(distinct(census_district, area = provincia, provcode = substr(ubigeo, 1, 4)),
            by = "area") %>%
  mutate(area = fct_reorder(area, as.numeric(provcode), .desc = TRUE)) %>%
  ggplot()+
  geom_pointrange(aes(x = area, y = m3,
                      ymin = lower, ymax = upper),
                  col = "red4",
                  alpha = .5, linewidth = 1) +
  geom_point(aes(x  = area, y = census), col = "blue4") +
  labs( x= "province", y = "proportion") +
  coord_flip() +
  theme_classic() +
  theme(
    strip.placement = "outside"
  )


cowplot::plot_grid(
  provincia,distrito,
  
  labels = c("a","b")
)

ggsave("./figures/sup_result_fig3.8.png", dpi = 300, width = 9.4)
ggsave("./figures/sup_result_fig3.8.pdf", dpi = 300, width = 9.4)
# MAE ----

comparison %>%
  filter(level != "department") %>%
  group_by(level) %>%
  summarise(n   = n(),
            MAE = mean(abs(m3 - census)),
            .groups = "drop") %>%
  gt() %>%
  tab_header(title = md("**EDU 2017 — MAE m3 vs. Census (pp)**")) %>%
  fmt_number(columns = MAE, decimals = 2)

