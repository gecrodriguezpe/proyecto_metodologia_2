####
### Script con estimaciones adicionales ###
####

# Bibliotecas generales de R
library(tidyverse)   # Conjunto de paquetes (contiene dplyr y ggplot2)
library(readxl)      # Importar bases de datos tipo excel
library(haven)       # Importar bases de datos tipo stata 

# RD Packages (Conjunto de paquetes para realizar RDDs creados por Cattaneo)
library(rdrobust)
library(rddensity)
library(lpdensity)
library(rdlocrand)

setwd("~/Documents/GitHub/semestre5_git/proyecto_metodologia_2/trabajo_empirico/bases_de_datos/bases_de_datos_SISBEN")

#---- Indicaciones/información estimación ----

### El script se concetrará en hacer las estimaciones desagregando por grupos ###
# g1: 6 a 11 años 
# g2: 12 a 17 años 

###
# Fucniones para la estimación por medio de un RDD

## rdrobust: Estimación
## rdplot: Gráficas
## 

###
# Información sobre la regresión

####
# Variables:
####

# Outcome: 
### per014: Actividad en el último mes (que se reescribio para que quedará estudio o no el último mes)
### Variable binaria: 0: el último mes no estudio, 1: el último mes estudio
### Tiene sentido en la medida que los grupos considerados tienen edades entre 6 a 17 años de edad

# Running variable: 
### puntaje_sisben_3: variable continua

# Controles: 
### per001: Sexo, 0: Mujer, 1: Hombre
### per010: Embarazada, 0: No, 1: Sí
### per016: Percibe ingresos: 0: No, 1: Sí

#---- Estimaciones adicionales ----

# Base de datos general

sisben3 = read_dta("Sisben_III_PERS_DIC2016.dta")

general = sisben3 %>% 
  relocate(llave, hogar, orden, fex, .before = depto) %>% 
  relocate(zona, .before = area) %>% 
  relocate(corte, .before = puntaje_sisben_3) %>% 
  filter(per002 == 2 | per002 == 3) %>%                   # per002: Edad: 2: 6 a 11 años, 3: 12 a 17 años 
  mutate(asiste_centro_educativo = ifelse(per011 == 2, 0, 1),
         sexo = ifelse(per001 == 2, 0, 1),
         paga_alimentax = ifelse(per008 == 2, 0, 1),
         embarazada = ifelse(per010 == 2, 0, 1),
         percibe_ing = ifelse(per016 == 2, 0, 1),
         estudio_ult_mes = ifelse(per014 == 4, 1, 0)
  ) %>%  
  select(llave, depto, puntaje_sisben_3, per002, sexo, paga_alimentax, embarazada,
         asiste_centro_educativo, percibe_ing, estudio_ult_mes)

#---- Estimax grupo1: 6 a 11 años ----

# Base de datos grupo 1
g1 =  general %>% 
  filter(per002 == 2)

# validación de supuestos:

## sexo
sexo_g1_reg = rdrobust(g1$sexo, g1$puntaje_sisben_3, c = 30.56,
                       kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(sexo_g1_reg)

mean_sex_g1 = mean(g1$sexo)

## embarazada

embaraz_g1_reg = rdrobust(g1$embarazada, g1$puntaje_sisben_3, c = 30.56,
                          kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(embaraz_g1_reg)

mean_embaraz_g1 = mean(g1$embarazada)

## percibe ingreso 

percibe_ing_g1_reg = rdrobust(g1$percibe_ing, g1$puntaje_sisben_3, c = 30.56,
                           kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(percibe_ing_g1_reg)

mean_percibe_ing_g1 = mean(g1$percibe_ing)

# estimación de interés :
covs = cbind.data.frame(g1$sexo, g1$embarazada, g1$percibe_ing)

### estimación sin controles 

estudio_ult_mes_g1_reg = rdrobust(g1$estudio_ult_mes, g1$puntaje_sisben_3, c = 30.56, 
                               kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(estudio_ult_mes_g1_reg)

### estimación con controles 

estudio_ult_mes_reg_g1_covs = rdrobust(g1$estudio_ult_mes, g1$puntaje_sisben_3, c = 30.56,
                                    kernel = "uniform", vce = "hc0", p = 1, h = 1, covs = covs); summary(estudio_ult_mes_reg_g1_covs)

main_reg_plot_not_interval_g1 = rdplot(g1$estudio_ult_mes, g1$puntaje_sisben_3,  
                                    c = 30.56, kernel = "uniform", h = 3, p = 1,
                                    support = c(28,35), x.lim = c(26,35), y.lim = c(0.8, 1))


#---- Estimax grupo2: 12 a 17 años ----

# Base de datos grupo 2
g2 =  general %>% 
  filter(per002 == 3)

# validación de supuestos:

## sexo
sexo_g2_reg = rdrobust(g2$sexo, g2$puntaje_sisben_3, c = 30.56,
                       kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(sexo_g2_reg)

mean_sex_g2 = mean(g2$sexo)

## embarazada

embaraz_g2_reg = rdrobust(g2$embarazada, g2$puntaje_sisben_3, c = 30.56,
                          kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(embaraz_g2_reg)

mean_embaraz_g2 = mean(g2$embarazada)

## percibe ingreso 

percibe_ing_g2_reg = rdrobust(g2$percibe_ing, g2$puntaje_sisben_3, c = 30.56,
                              kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(percibe_ing_g2_reg)

mean_percibe_ing_g2 = mean(g2$percibe_ing)

# estimación de interés :
covs = cbind.data.frame(g2$sexo, g2$embarazada, g2$percibe_ing)

### estimación sin controles 

estudio_ult_mes_g2_reg = rdrobust(g2$estudio_ult_mes, g2$puntaje_sisben_3, c = 30.56, 
                                  kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(estudio_ult_mes_g2_reg)

### estimación con controles 

estudio_ult_mes_reg_g2_covs = rdrobust(g2$estudio_ult_mes, g2$puntaje_sisben_3, c = 30.56,
                                       kernel = "uniform", vce = "hc0", p = 1, h = 1, covs = covs); summary(estudio_ult_mes_reg_g2_covs)

main_reg_plot_not_interval_g2 = rdplot(g2$estudio_ult_mes, g2$puntaje_sisben_3,  
                                       c = 30.56, kernel = "uniform", h = 3, p = 1,
                                       support = c(28,35), x.lim = c(28,33), y.lim = c(0.7, 0.9))
