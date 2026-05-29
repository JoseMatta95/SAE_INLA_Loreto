# Mapping subnational socioeconomic indicators and malaria risk in highly dispersed regions using small area estimation: a retrospective ecological cohort study

## Study description

This study applies small area estimation (SAE) to produce district-level estimates of education level and wealth index for the 53 districts of Loreto, Peru, over the period 2010–2020. Direct estimates from the Demographic and Health Survey (ENDES) are unreliable at the district level due to small sample sizes, particularly in remote Amazonian areas. We use INLA-based Bayesian models with spatial (SPDE, BYM2) and temporal (RW2) components fitted at the primary sampling unit (PSU) level, incorporating 2017 Census covariates via principal component analysis. Model selection is based on CPO and predictive accuracy against design-based direct estimates. Final district-year predictions are used to characterize socioeconomic inequalities and their association with malaria incidence.

![](https://github.com/JoseMatta95/SAE_INLA_Loreto/blob/main/figures/main%20figures/fig1.png)

> **Fig 1.** Study area. (a) PSU locations by period. (b) Loreto provinces and malaria incidence trends 2010–2020. (c) Spatial distribution of malaria incidence across districts by year.

## Repository structure

1. [data](https://github.com/josematta/SAE_INLA_Loreto/tree/main/data) — Input and output datasets
    - `wi_edu_2010_2020.csv`: ENDES 2010–2020 household survey data for Loreto
    - `censo_2017/`: 2017 Census variables and PCA scores (53 districts)
    - `aux_data/`: Malaria incidence, shapefiles (district boundaries and rivers)
    - `final_data/`: District-year SAE predictions for education and wealth index (best model)
    - `models/`: Full model prediction objects (.rds)
2. [figures](https://github.com/josematta/SAE_INLA_Loreto/tree/main/figures) — Figures in the main text and supplementary material
    - Fig 1: Study area and malaria incidence
    - Fig 2: Model validation — predicted vs. direct estimates with MAE, SMAPE and RMSE
    - Fig 3: Annual MSE decomposition and CPO model comparison
    - Fig 4–7: District-level SAE maps and biscale malaria analysis
3. [_functions](https://github.com/josematta/SAE_INLA_Loreto/tree/main/_functions) — Helper functions
    - `consulta_endes2`: Downloads ENDES microdata modules from INEI (updated from [horaciochacon/ENDES.PE](https://github.com/horaciochacon/ENDES.PE))
    - `reconstruir_eta_spde`: Reconstructs η from INLA posterior samples for SPDE models
    - `resumen_anual_samples`: Aggregated annual prevalence with 95% credible intervals from posterior samples
    - `get_legend2`: Extracts legend grob from ggplot for composite figures
4. [00.raw_data_extraction.R](https://github.com/josematta/SAE_INLA_Loreto/blob/main/00.raw_data_extraction.R) — Downloads ENDES microdata and processes 2017 Census raw files
5. [01_SAE_INLA_I_part_modified.Rmd](https://github.com/josematta/SAE_INLA_Loreto/blob/main/01_SAE_INLA_I_part_modified.Rmd) — Direct estimates (design-based) and census covariate preparation
6. [02.SAE_INLA_EDU_method2.Rmd](https://github.com/josematta/SAE_INLA_Loreto/blob/main/02.SAE_INLA_EDU_method2.Rmd) — SAE models for education (BYM2 and SPDE, with and without PCA covariates)
7. [03.SAE_INLA_WI_method2.Rmd](https://github.com/josematta/SAE_INLA_Loreto/blob/main/03.SAE_INLA_WI_method2.Rmd) — SAE models for wealth index (binomial and beta-binomial, BYM2 and SPDE)
8. [05.Aggregated_consistency.Rmd](https://github.com/josematta/SAE_INLA_Loreto/blob/main/05.Aggregated_consistency.Rmd) — Aggregated consistency check against ENDES direct estimates
9. [06.biscale_malaria.Rmd](https://github.com/josematta/SAE_INLA_Loreto/blob/main/06.biscale_malaria.Rmd) — Biscale maps combining SAE predictions with malaria incidence
10. [Supplementary_material.R](https://github.com/josematta/SAE_INLA_Loreto/blob/main/Supplementary_material.R) — Supplementary figures and tables
11. [SAE_INLA_Loreto.Rproj](https://github.com/josematta/SAE_INLA_Loreto/blob/main/SAE_INLA_Loreto.Rproj) — R project file
12. README.md

## Environment and version

```
platform       aarch64-apple-darwin20      
arch           aarch64                     
os             darwin20                    
system         aarch64, darwin20           
status                                     
major          4                           
minor          4.2                         
year           2024                        
month          10                          
day            31                          
svn rev        87279                       
language       R                           
version.string R version 4.4.2 (2024-10-31)
nickname       Pile of Leaves              
```
