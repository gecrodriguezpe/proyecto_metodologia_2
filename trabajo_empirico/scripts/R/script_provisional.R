# Bibliotecas generales de R
library(tidyverse)   # Conjunto de paquetes (contiene dplyr y ggplot2)
library(readxl)      # Importar bases de datos tipo excel
library(haven)       # Importar bases de datos tipo stata 

# RD Packages (Conjunto de paquetes para realizar RDDs creados por Cattaneo)
library(rdrobust)
library(rddensity)
library(lpdensity)
library(rdlocrand)

# Directorio de trabajo 
# setwd("~/Documents/GitHub/semestre5_git/proyecto_metodologia_2/trabajo_empirico/bases_de_datos/base_de_datos_familias_en_accion")
# fam_ax = read_csv("Beneficiarios_M_s_Familias_en_Acci_n.csv")

setwd("~/Documents/GitHub/semestre5_git/proyecto_metodologia_2/trabajo_empirico/bases_de_datos/bases_de_datos_SISBEN")

#---- Base de Datos personas sisbén ----
sisben3 = read_dta("Sisben_III_PERS_DIC2016.dta")

sisben3_hogar = read_dta("Sisben_III_HOG_DIC2016.dta")

# Base de datos modificada original
general = sisben3 %>% 
  relocate(llave, hogar, orden, fex, .before = depto) %>% 
  relocate(zona, .before = area) %>% 
  relocate(corte, .before = puntaje_sisben_3) %>% 
  filter(per002 == 2 | per002 == 3) %>% 
  mutate(per011 = ifelse(per011 == 2, 0, 1),
         per001 = ifelse(per001 == 2, 0, 1),
         per008 = ifelse(per008 == 2, 0, 1),
         per010 = ifelse(per010 == 2, 0, 1),
         per016 = ifelse(per016 == 2, 0, 1),
         per014 = ifelse(per014 == 4, 1, 0)
         )

summary(as.factor(general$per011))  

# Nota para la base de datos principal: 
### per011: 0: no asiste a centro educativo, 1: asiste a centro educativo
### per001: 0: mujer, 1: hombre
### per008: 0: no paga arriendo a otro miembro del hogar, 1: paga arriendo a otro miembro del hogar
### per010: 0: no ha estado embarazada o no ha tenido hijos, 1: ha estado embaraza o a tenido hijos 
### per016: 0: no percibe ingresos, 1: sí percibe ingresos
### per014: 0: no estaba estudiadno en el último mes, 1: estaba estudiando en el último mes

# Base de datos grupo 1
g1 =  sisben3 %>% 
  relocate(llave, hogar, orden, fex, .before = depto) %>% 
  relocate(zona, .before = area) %>% 
  relocate(corte, .before = puntaje_sisben_3) %>% 
  filter(per002 == 2) %>% 
  mutate(per011 = ifelse(per011 == 2, 0, 1),
         per001 = ifelse(per001 == 2, 0, 1),
         per008 = ifelse(per008 == 2, 0, 1),
         per010 = ifelse(per010 == 2, 0, 1),
         per016 = ifelse(per016 == 2, 0, 1)
  ) 

# Base de datos grupo 2
g2 =  sisben3 %>% 
  relocate(llave, hogar, orden, fex, .before = depto) %>% 
  relocate(zona, .before = area) %>% 
  relocate(corte, .before = puntaje_sisben_3) %>% 
  filter(per002 == 3) %>% 
  mutate(per011 = ifelse(per011 == 2, 0, 1),
         per001 = ifelse(per001 == 2, 0, 1),
         per008 = ifelse(per008 == 2, 0, 1),
         per010 = ifelse(per010 == 2, 0, 1),
         per016 = ifelse(per016 == 2, 0, 1)
  ) 

# Base de datos sin bogota
sin_bogota = sisben3 %>% 
  relocate(llave, hogar, orden, fex, .before = depto) %>% 
  relocate(zona, .before = area) %>% 
  relocate(corte, .before = puntaje_sisben_3) %>% 
  filter(per002 == 2 | per002 == 3) %>% 
  filter(! depto == 11) %>% 
  mutate(per011 = ifelse(per011 == 2, 0, 1),
         per001 = ifelse(per001 == 2, 0, 1),
         per008 = ifelse(per008 == 2, 0, 1),
         per010 = ifelse(per010 == 2, 0, 1),
         per016 = ifelse(per016 == 2, 0, 1)
  )


# Base de datos solo antioquia
antioquia = sisben3 %>% 
  relocate(llave, hogar, orden, fex, .before = depto) %>% 
  relocate(zona, .before = area) %>% 
  relocate(corte, .before = puntaje_sisben_3) %>% 
  filter(per002 == 2 | per002 == 3) %>% 
  filter(! depto == 11) %>% 
  mutate(per011 = ifelse(per011 == 2, 0, 1),
         per001 = ifelse(per001 == 2, 0, 1),
         per008 = ifelse(per008 == 2, 0, 1),
         per010 = ifelse(per010 == 2, 0, 1),
         per016 = ifelse(per016 == 2, 0, 1)
  )

  
#---- Histograma ----  
           
histogram_puntaje_sisben = sisben_f %>% 
  ggplot(aes(x = puntaje_sisben_3)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 30.56, color = "red") + 
  theme_light() +
  xlab("Puntaje del Sisbén ") + 
  ylab("Frequency") +
  ggtitle("Histograma para el Puntaje del sisbén")

histogram_puntaje_sisben

#---- Test de McCrary ----  

### Para el valor de umbral puntaje del sisbén = 30.56
test_30.56 = rddensity(sisben_f$puntaje_sisben_3, c = 30.56, vce = "plugin")
summary(test_30.56)

plot_mccrary_30.56 = rdplotdensity(test_30.56, sisben_f$puntaje_sisben_3)

#---- Validación de la hipótesis de identificación ----  



#---- Estimación de interés ----

## Se especifica una matriz de covariadas. 
covs = cbind.data.frame(sisben_f$per001, sisben_f$per008, sisben_f$per010, sisben_f$per016)

#covs = cbind.data.frame(per001, per008)
covs = cbind.data.frame(sisben3$per001, sisben3$per008, sisben3$per010, sisben3$per016)

### asistencia centro educativo vs puntaje del sisben
asistencia_centro_educativo_reg = rdrobust(general$per014, general$puntaje_sisben_3,
                                           c = 30.56, kernel = "uniform", vce = "hc0", p = 1, h = 1)
summary(asistencia_centro_educativo_reg)

### gráfica asistencia centro educativo vs puntaje del sisben (sin intervalos de confianza)
recivitism_plot_not_interval = rdplot(general$per010, general$puntaje_sisben_3,  
                                      c = 30.56, kernel = "uniform", h = 2, p = 1,
                                      support = c(27,34))


### gráfica asistencia centro educativo vs puntaje del sisben (con intervalos de confianza)
recivitism_plot_interval = rdplot(sisben_f$per011, sisben_f$puntaje_sisben_3, x.lim = c(26, 35), 
                                      y.lim = c(0.6,1), c = 30.56, kernel = "uniform", h = 2,
                                      ci = 95 )

#---- Gráficos de control ----





#---- Indicaciones ----
# rdrobust: Estimación
# rdplot: Gráficas