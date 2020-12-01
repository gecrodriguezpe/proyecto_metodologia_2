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

#---- Estimax grupo2: 12 a 17 años ----

# Base de datos grupo 2
g2 =  general %>% 
  filter(per002 == 3)

#---- Estimax sin bogota ----

sin_bogota = general %>% 
  filter(! depto == 11) 

#---- Estimax solo bogota ----

solo_bogota = general %>% 
  filter(depto == 11) 

#---- Estimax solo antioquia ----

antioquia = general %>% 
  filter(depto == 5) 

