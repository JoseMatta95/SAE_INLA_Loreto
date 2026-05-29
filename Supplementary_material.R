library(tidyverse)
library(sf)
library(innovar)
library(janitor)
library(cowplot)
library(ggspatial)
library(rcartocolor)

data(Peru)
distritos <- Peru %>% filter(dep == "LORETO")
data_malaria <- read_csv("./data/malaria_deforest_data.csv")

rivers<- st_read("./data/aux_data/data_shapefiles/rios-ana/Rios.shp")
rivers<- st_transform(rivers,st_crs(distritos))

ccpp <- st_read("./data/aux_data/data_shapefiles/boundaries_towncenter/Centros_Poblados_2023_geogpsperu_SuyoPomalia.shp")
ccpp<- st_transform(ccpp,st_crs(distritos))

# Supplementary materials

## 1. Supplementary information ----
### Loreto districts

### Districts from 2016-2020
map1<-
  distritos %>% 
  ggplot() +
  geom_sf(col = '#3c4856', fill = '#0d585f', alpha = .3) +
  geom_sf_text(aes(label = distr,
                   fontface = ifelse(distr == "IQUITOS", "bold", "plain")), size = 1.5) +
  theme_void() +
  geom_rect(aes(xmin = -74, xmax = -73.0, 
                ymin = -4.4, ymax = -3.2), 
            color = "black", fill = NA)

### Zoom plot to Iquitos

map2<-
  distritos %>% 
  filter(distr %in%c("IQUITOS","BELEN","PUNCHANA",
                     "SAN JUAN BAUTISTA")) %>% 
  ggplot()+
  geom_sf(col = '#3c4856', fill = '#0d585f', alpha = .3) +
  geom_sf_text(aes(label = distr,
                   fontface = ifelse(distr == "IQUITOS", "bold", "plain")), 
               size = 2.5) +
  theme_void() +
  
  geom_rect(aes(xmin = -74.135437, xmax = -73.055078, 
                ymin = -4.449160, ymax = -3.283977), 
            color = "black", fill = NA)

### Districts from 2010-2015
distritos_union <- distritos %>% 
  mutate(grupo = ifelse(ubigeo %in% c("160801", "160802", "160803","160804"), 
                        "mi_union",   # nombre para los unidos
                        ubigeo),
         
         distr = ifelse(ubigeo %in% c("160801", "160802", "160803","160804"), 
                        "PUTUMAYO",   # nombre para los unidos
                        distr)) %>%   # los demás mantienen su nombre
  group_by(grupo,distr) %>% 
  summarise(geometry = st_union(geometry))


map3<-
  distritos_union %>% 
  ggplot() +
  geom_sf(col = '#3c4856', fill = '#0d585f', alpha = .3) +
  geom_sf_text(aes(label = distr,
                   fontface = ifelse(distr == "IQUITOS", "bold", "plain")), size = 1.5) +
  theme_void() +
  annotation_north_arrow(
    location = "br",              # esquina (tl, tr, bl, br)
    which_north = "true",
    pad_x = unit(1, "cm"),      # separación horizontal
    pad_y = unit(0.8, "cm"),      # separación vertical
    style = north_arrow_orienteering
  ) +
  
  # Barra de escala
  annotation_scale(
    width_hint = 0.3,
    location = "br"
    
  )
####

divider <- ggdraw() + theme_void() +
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 1),
               inherit.aes = FALSE,
               linewidth = 1,
               color = "black")


plot_grid(ggdraw() +
            draw_plot(map1) +
            draw_plot(map2, x = .65, y = -.3, 
                      width = .4, height = 1.1),
          divider,
          map3,
          nrow = 1,
          rel_widths = c(.8,.1,.8),
          labels = c("a","","b"))

##### sm fig 1 ----
ggsave("./figures/sm_fig1.pdf", dpi = 800, bg = "white", height = 7, width = 12)

### Loreto urbanization

g2<-
  distritos %>% 
  mutate(
    ubigeo = as.numeric(ubigeo)
  ) %>% 
  inner_join(
    data_malaria %>% 
      select(year,iddist,prop_urbanization,lclu_urban),
    by = c("ubigeo" = "iddist")
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = log1p(lclu_urban))) +
  
  scale_fill_carto_c(palette = "Mint", 
                     na.value = "gray", 
                     name = "log(1 + Urban settlements 2010-2020 (km²)) ",
                     limits = c(0,4)) +

  guides(fill = guide_colourbar(barheight = 0.5, 
                                barwidth = 20,
                                title.position = "top",
                                direction = "horizontal")) +
  theme_void()+
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) 
  #facet_wrap(~year)

g3<-
  distritos %>% 
  mutate(
    ubigeo = as.numeric(ubigeo)
  ) %>% 
  inner_join(
    data_malaria %>% 
      select(year,iddist,prop_urbanization,lclu_urban),
    by = c("ubigeo" = "iddist")
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = (prop_urbanization))) +
  
  scale_fill_carto_c(palette = "Mint", 
                     na.value = "gray", 
                     name = "Proportion of urban areas (2010-2020)",
                     limits = c(0, 1)) +
  
  guides(fill = guide_colourbar(barheight = 0.5, 
                                barwidth = 20,
                                title.position = "top",
                                direction = "horizontal")) +
  theme_void()+
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
  #facet_wrap(~year)

### Loreto rivers and ccpp

rivers_loreto<-st_intersection(distritos,rivers)
ccpp <- ccpp %>% filter(cod_dpto == 16)

g4<-
  ggplot() +
  geom_sf(data = distritos,
          aes(fill = "Distritos"),
          color = "black", show.legend = F) +
  
  geom_sf(data = rivers_loreto,
          aes(color = "Rivers"),
          size = 0.8, 
          key_glyph = "path") +  
  
  geom_sf(data = ccpp,
          aes(color = "Population centers"),
          shape = 19, size = 1.5, alpha = .5,
          key_glyph = "point") + 
  
  scale_fill_manual(name = "", values = c("Distritos" = "white")) +
  scale_color_manual(name = "",
                     values = c("Rivers" = "blue4",
                                "Population centers" = "green4")) +
  theme_void() +
  theme(legend.position = "bottom")

##### sm fig 2 ----
plot_grid(
  plot_grid(g4,labels = c("a")),
  plot_grid(g2,g3, labels = c("b","c")),
  nrow = 2)

ggsave("./figures/sm_fig2.pdf", dpi = 800, bg = "white", 
       height = 6, width = 9)

## 2. Supplementary methods ----
##### sm fig 3 ----

# code is part of PCA in 00.raw_data_extraction.R

## 3. Supplementary results ----
##### sm fig 4 ----
library(cowplot)

a<- distritos_sf_predict_prop %>% 
  select(ubigeo,year,real_cases,fit2_spat_2) %>% 
  filter(year %in% c(2010:2015)) %>% 
  pivot_longer(cols = c(real_cases:fit2_spat_2)) %>% 
  mutate(
    name = factor(name, levels = c("real_cases","fit2_spat_2"), 
                  labels = c("Direct estimate","PC 02-2"))
  ) %>% 
  ggplot()+
  geom_sf(aes(fill=value), alpha = .7)+
  scale_fill_gradient(low = "white", high = "#0d585f")+
  theme_void()+
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )+
  facet_grid(name~year)

b<-  distritos_sf_predict_prop %>% 
  select(ubigeo,year,real_cases,fit2_spat_2) %>% 
  filter(year %in% c(2016:2020)) %>% 
  pivot_longer(cols = c(real_cases:fit2_spat_2)) %>% 
  mutate(
    name = factor(name, levels = c("real_cases","fit2_spat_2"), 
                  labels = c("Direct estimate","PC 02-2"))
  ) %>% 
  ggplot()+
  geom_sf(aes(fill=value), alpha = .7)+
  scale_fill_gradient(low = "white", high = "#0d585f")+
  labs(fill = "Poverty proportion") +
  theme_void()+
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )+
  facet_grid(name~year)


plot_grid(a,b, ncol = 1, rel_heights = c(.8,.9))

ggsave("./figures/sm_fig4.pdf", dpi = 800,bg = "white", width = 8)

##### sm fig 5 ----

a<-
  distritos_sf_predict_prop %>% 
  select(ubigeo,year,estimacion_real,fit2_spat_2) %>% 
  filter(year %in% c(2010:2015)) %>% 
  pivot_longer(cols = c(estimacion_real:fit2_spat_2)) %>% 
  mutate(
    name = factor(name, levels = c("estimacion_real","fit2_spat_2"), labels = c("Direct estimate","PC 02-2"))
  ) %>% 
  ggplot()+
  geom_sf(aes(fill=value), alpha = .7)+
  scale_fill_gradient(low = "white", high = "#0d585f")+
  theme_void()+
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )+
  facet_grid(name~year)

b<-
  distritos_sf_predict_prop %>% 
  select(ubigeo,year,estimacion_real,fit2_spat_2) %>% 
  filter(year %in% c(2016:2020)) %>% 
  pivot_longer(cols = c(estimacion_real:fit2_spat_2)) %>% 
  mutate(
    name = factor(name, levels = c("estimacion_real","fit2_spat_2"), labels = c("Direct estimate","PC 02-2"))
  ) %>% 
  ggplot()+
  geom_sf(aes(fill=value), alpha = .7)+
  scale_fill_gradient(low = "white", high = "#0d585f")+
  labs(fill = "Primary education proportion") +
  theme_void()+
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )+
  facet_grid(name~year)

plot_grid(a,b, ncol = 1, rel_heights = c(.8,.9))

ggsave("./figures/sm_fig5.pdf", dpi = 800,bg = "white", width = 8)


