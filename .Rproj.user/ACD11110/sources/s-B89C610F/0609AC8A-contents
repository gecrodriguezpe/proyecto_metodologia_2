# Bibliotecas generales de R
library(tidyverse)   # Conjunto de paquetes (contiene dplyr y ggplot2)
library(readxl)      # Importar bases de datos tipo excel
library(haven)       # Importar bases de datos tipo stata 

# RD Packages (Conjunto de paquetes para realizar RDDs creados por Cattaneo)
library(rdrobust)
library(rddensity)
library(lpdensity)
library(rdlocrand)

setwd("~/Documents/GitHub/semestre5_git/Evaluacion-de-Impacto-/Datos")

# Imporatando la base de datos 

base_stata = read_dta("hansen_dwi.dta")  # For reading stata files 
base_excel = read_excel("RDD datos.xlsx")

############################################
# Parte II (Reprdoucción empírica del paper)

base = base_stata %>% 
       mutate(bac = low_score / 1000, 
              dui = ifelse(bac < 0.08, 0, 1),
              dui_ag = ifelse(bac < 0.15, 0, 1))
attach(base)

# Punto 1
## Histograma del BAC(running variable)

histogram_BAC = base %>% 
  ggplot(aes(x = bac)) +
  geom_histogram(binwidth = 0.0001) +
  geom_vline(xintercept = 0.08, color = "red") + 
  geom_vline(xintercept = 0.15, color = "blue") + 
  theme_light() +
  xlab("BAC") + 
  ylab("Frequency") +
  ggtitle("Histograma BAC")

histogram_BAC

# Punto 2 
## Test de McCrary

### Para el valor de umbral bac = 0.08
test_0.08 = rddensity(bac, c = 0.08, vce = "plugin")
summary(test_0.08)

plot_mccrary_0.08 = rdplotdensity(test_0.08, bac)

### Para el valor de umbral bac = 0.15
test_0.15 = rddensity(bac, c = 0.15, vce = "plugin")
summary(test_0.15)

plot_mccrary_0.15 = rdplotdensity(test_0.15, bac)

# Nota agregar los titulos, xlabels y labels a cada grafica

# Punto 3 
## Reproducción parcial de la Tabla 2 columnas (1)-(4), Panel (A)

### male vs bac
male_reg = rdrobust(male, bac, c = 0.08, kernel = "uniform", p = 1, h = 0.05, vce = "hc0")
summary(male_reg)

### white vs bac
white_reg = rdrobust(white, bac, c = 0.08, kernel = "uniform", p = 1, h = 0.05, vce = "hc0")
summary(white_reg)

### age vs bac
aged_reg = rdrobust(aged, bac, c = 0.08, kernel = "uniform", p = 1, h = 0.05, vce = "hc0")
summary(aged_reg)

### acc vs bac
acc_reg = rdrobust(acc, bac, c = 0.08, kernel = "uniform", p = 1, h = 0.05, vce = "hc0")
summary(acc_reg)

### means 
mean(male)
mean(white)
mean(aged)
mean(acc)
          
# Punto 4
## Reproducción parcial de la Tabla 3, columna 1, Panel (A) y (B), (Estimación de interés)

## Se especifica una matriz de covariadas. 
## Dicha matriz de covariadas va a ser empleada en los punto 4, 5, 6 y 7
covs = cbind.data.frame(male, white, year, aged)

### recidivism vs bac (bandwidth = 0.05)

recidivism_reg_0.05 = rdrobust(recidivism, bac, c = 0.08, kernel = "uniform", 
                               p = 1, h = 0.05, vce = "hc0", covs = covs)
summary(recidivism_reg_0.05)

### recidivism vs bac (bandwidth = 0.025)  
recidivism_reg_0.025 = rdrobust(recidivism, bac, c = 0.08, kernel = "uniform", 
                               p = 1, h = 0.025, vce = "hc0", covs = covs)
summary(recidivism_reg_0.025)

### mean 
mean(recidivism)

# Punto 5
## Muestre en una gráfica el outcome sobre la running variable, con cutoff a 0.08 
## y soporte del BAC entre 0.03 y 0.20, con intervalos de confianza, kernel rectangular

### gráfica recidivism vs bac (sin intervalos de confianza)
recivitism_plot_not_interval = rdplot(recidivism, bac, x.lim = c(0.03, 0.2), y.lim = c(0, 0.25), 
       c = 0.08, kernel = "uniform", support = c(0.03, 0.2), p = 1, h = 0.05)

### gráfica recidivism vs bac (con intervalos de confianza)
recivitism_plot_with_interval = rdplot(recidivism, bac, x.lim = c(0.03, 0.2), y.lim = c(0, 0.25), 
                                      c = 0.08, kernel = "uniform", support = c(0.03, 0.2), 
                                      p = 1, shade = TRUE, ci = 95, h = 0.05, x.label = "bac",
                                      y.label = "recidivism", title = "recidivism vs bac")

# Punto 6
## Calcule el pvalor del efecto del DUI usando inferencia randomizada

random_inf_treshold = rdrandinf(recidivism, bac, cutoff = 0.08, statistic = "diffmeans", p = 1, kernel = "uniform", 
                                reps = 4000, seed = 1234);summary(random_inf_treshold)

# Punto 7
## Realice un experimento placebo a 0.04, con ancho de banda .04  

### Parámetros para el placebo
placebo_cutoff = 0.04
bandwidth_placebo = 0.04

### Gráfica placebo sin intervalos de confianza
placebo_without = rdplot(recidivism, bac, x.lim = c(0, 0.08), y.lim = c(0, 0.25), 
                                       c = placebo_cutoff, kernel = "uniform", support = c(0, 0.08), 
                                       p = 1, h = bandwidth_placebo)

### Gráfica placebo con intervalos de confianza
placebo_with = rdplot(recidivism, bac, x.lim = c(0, 0.2), y.lim = c(0, 0.25), 
                                       c = placebo_cutoff, kernel = "uniform", support = c(0, 0.2), 
                                       p = 1, shade = TRUE, ci = 95, h = bandwidth_placebo, 
                                       x.label = "bac", y.label = "recidivism",
                                       title = "recidivism vs bac (caso placebo)")

### Estimando el modelo en el placebo 
placebo_reg_0.04 = rdrobust(recidivism, bac, c = placebo_cutoff, kernel = "uniform", 
                               p = 1, h = bandwidth_placebo, vce = "hc0", covs = covs)
summary(placebo_reg_0.04)
