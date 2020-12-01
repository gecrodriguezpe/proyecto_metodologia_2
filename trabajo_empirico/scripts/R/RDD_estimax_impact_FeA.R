####
### Script con estimación principal ###
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
### estudio_ult_mes: Actividad en el último mes (que se reescribio para que quedará estudio o no el último mes)
### Variable binaria: 0: el último mes no estudio, 1: el último mes estudio
### Tiene sentido en la medida que los grupos considerados tienen edades entre 6 a 17 años de edad

# Running variable: 
### puntaje_sisben_3: variable continua

# Controles: 
### sexo: Sexo, 0: Mujer, 1: Hombre
### embarazada: Embarazada, 0: No, 1: Sí
### percibe_ing: Percibe ingresos: 0: No, 1: Sí

#---- Base de Datos para estimación principal ----
sisben3 = read_dta("Sisben_III_PERS_DIC2016.dta")

# Base de datos modificada original (general)
general = sisben3 %>% 
  relocate(llave, hogar, orden, fex, .before = depto) %>% 
  relocate(zona, .before = area) %>% 
  relocate(corte, .before = puntaje_sisben_3) %>% 
  filter(per002 == 2 | per002 == 3) %>% 
  mutate(asiste_centro_educativo = ifelse(per011 == 2, 0, 1),
         sexo = ifelse(per001 == 2, 0, 1),
         paga_alimentax = ifelse(per008 == 2, 0, 1),
         embarazada = ifelse(per010 == 2, 0, 1),
         percibe_ing = ifelse(per016 == 2, 0, 1),
         estudio_ult_mes = ifelse(per014 == 4, 1, 0)
         ) %>%  
  select(llave, depto, puntaje_sisben_3, sexo, paga_alimentax, embarazada,
         asiste_centro_educativo, percibe_ing, estudio_ult_mes)

# Ver cuántas personas estudiaron y no estudiaron el último mes
summary(as.factor(general$estudio_ult_mes))  

# Nota para la base de datos principal: 
### per001: 0: mujer, 1: hombre
### per008: 0: no paga arriendo a otro miembro del hogar, 1: paga arriendo a otro miembro del hogar
### per010: 0: no ha estado embarazada o no ha tenido hijos, 1: ha estado embaraza o a tenido hijos 
### per011: 0: no asiste a centro educativo, 1: asiste a centro educativo
### per014: 0: no estaba estudiadno en el último mes, 1: estaba estudiando en el último mes
### per016: 0: no percibe ingresos, 1: sí percibe ingresos
  
#---- Histograma (no manipulación) ----  
           
histogram_puntaje_sisben = general %>% 
  ggplot(aes(x = puntaje_sisben_3)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 30.56, color = "red") + 
  theme_light() +
  xlab("Puntaje del Sisbén ") + 
  ylab("Frequency") +
  ggtitle("Histograma para el Puntaje del sisbén")

histogram_puntaje_sisben

#---- Test de McCrary (No manipulación) ----  

### Para el valor de umbral puntaje del sisbén = 30.56
test_30.56 = rddensity(general$puntaje_sisben_3, c = 30.56, vce = "plugin")
summary(test_30.56)

plot_mccrary_30.56 = rdplotdensity(test_30.56, general$puntaje_sisben_3)

####
#---- Validación de la hipótesis de identificación ----  
####

#---- estimación de variables de control (validación de continuidad de los controles) ----

# Sexo (No hay discontinuidad, se cumple el supuesto)

sexo_reg = rdrobust(general$sexo, general$puntaje_sisben_3, c = 30.56,
                   kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(sexo_reg)

sexo_graph = rdplot(general$sexo, general$puntaje_sisben_3,  
                    c = 30.56, kernel = "uniform", h = 3, p = 1,
                    support = c(28,35), x.lim = c(27, 34))

#---- gráficos de variables de control (validación del supuesto de identificación) ----

embarada_reg = rdrobust(general$embarazada, general$puntaje_sisben_3, c = 30.56,
                    kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(embarada_reg)

embarazada_graph = rdplot(general$embarazada, general$puntaje_sisben_3,  
                    c = 30.56, kernel = "uniform", h = 3, p = 1,
                    support = c(28,35), x.lim = c(27, 34), y.lim = c(-0.1, 0.1))

#---- gráficos de variables de control (validación del supuesto de identificación) ----

percibe_ing_reg = rdrobust(general$percibe_ing, general$puntaje_sisben_3, c = 30.56,
                           kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(percibe_ing_reg)

percibe_ing_graph = rdplot(general$percibe_ing, general$puntaje_sisben_3,  
                          c = 30.56, kernel = "uniform", h = 3, p = 1,
                          support = c(28,35), x.lim = c(27, 34), y.lim = c(-0.2, 0.3))

#---- Estimaciones de interés ----

## Se especifica una matriz de covariadas: sexo, embarazada
covs = cbind.data.frame(general$sexo, general$embarazada, general$percibe_ing)

#---- Estimación sin controles ----

### estudio en el último mes vs puntaje del sisben (sin covariadas)
estudio_ult_mes_reg = rdrobust(general$estudio_ult_mes, general$puntaje_sisben_3, c = 30.56, 
                               kernel = "uniform", vce = "hc0", p = 1, h = 1); summary(asistencia_centro_educativo_reg)

#---- Estimación con controles ----

### asistencia centro educativo vs puntaje del sisben (con covariadas)
estudio_ult_mes_reg_covs = rdrobust(general$estudio_ult_mes, general$puntaje_sisben_3, c = 30.56,
                                    kernel = "uniform", vce = "hc0", p = 1, h = 1, covs = covs); summary(asistencia_centro_educativo_reg_covs)

#---- gráfica estimación principal ----

### gráfica asistencia centro educativo vs puntaje del sisben (sin intervalos de confianza)
main_reg_plot_not_interval = rdplot(general$estudio_ult_mes, general$puntaje_sisben_3,  
                                      c = 30.56, kernel = "uniform", h = 3, p = 1,
                                      support = c(28,35), x.lim = c(26,35), y.lim = c(0.8, 0.88))


### gráfica asistencia centro educativo vs puntaje del sisben (con intervalos de confianza)
main_reg_plot_with_interval = rdplot(general$estudio_ult_mes, general$puntaje_sisben_3,  
                                      c = 30.56, kernel = "uniform", h = 3, p = 1,
                                      support = c(28,35), x.lim = c(26,35), y.lim = c(0.8, 0.9), 
                                     ci = 95)
