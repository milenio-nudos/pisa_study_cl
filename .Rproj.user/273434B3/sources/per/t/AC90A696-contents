pacman::p_load(haven,
               dplyr,
               gginference,
               gtsummary,
               sjPlot,
               table1)

options(scipen = 999) #Desactivar notación científica
rm(list = ls())       #Limpieza enviroment

datos <- readRDS("input/data_proc/datos_proc.rds")

#Crear subset con variables de interés

subset <- datos %>% 
  select(ST004D01T, ST250Q02JA, ST250Q04JA, ST250Q05JA, ICTRES, ICTEFFIC, ICTWKDY, ICTWKEND, ICTAVSCH, ICTSCH, ICTHOME, ICTAVHOM, ICTQUAL)


#Tabla de descriptivos

table1::table1(~ + ST004D01T + ST250Q02JA + ST250Q04JA + ST250Q05JA + ICTRES + ICTEFFIC + ICTWKDY + ICTAVSCH + ICTSCH + ICTHOME + ICTAVHOM + ICTQUAL, data = subset,
               caption = "Tabla 1: Estadísticos descriptivos muestra",
               footnote = "Fuente: Elaboración propia con datos de PISA.")

