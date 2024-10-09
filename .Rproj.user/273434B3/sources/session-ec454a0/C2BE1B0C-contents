pacman::p_load(haven,
               dplyr,
               gginference,
               gtsummary,
               sjPlot,
               table1,
               flextable,
               rempsyc,
               broom)

options(scipen = 999) #Desactivar notación científica
rm(list = ls())       #Limpieza enviroment

datos <- readRDS("input/data_proc/datos_proc.rds")

#Crear subset con variables de interés

subset <- datos %>% 
  select(ST004D01T, ST250Q02JA, ST250Q04JA, ST250Q05JA, ICTRES, ICTEFFIC, ICTWKDY, ICTWKEND, ICTAVSCH, ICTSCH, ICTHOME, ICTAVHOM, ICTQUAL)


#Tabla de descriptivos

table1::table1(~ + ST004D01T + ST250Q02JA + ST250Q04JA + ST250Q05JA + ICTEFFIC + ICTWKDY + ICTAVSCH + ICTAVHOM + ICTQUAL, data = subset,
               caption = "Tabla 1: Estadísticos descriptivos muestra",
               footnote = "Fuente: Elaboración propia con datos de PISA.")

#Análisis bivariado género y autoeficacia

genero_efficacy <- t.test(datos$ICTEFFIC ~ datos$ST004D01T, 
                           alternative = "greater",
                           conf.level = 0.95)

#Tabla estilizada

stats.table <- broom::tidy(genero_efficacy, conf_int = T)

nice_table(stats.table, broom = "t.test", 
           note = c("Fuente: Elaboración propia con datos de PISA.",
                   "* p < .05, ** p < .01, *** p < .001",
                   "Mean 1 = Media mujer"), 
           title = c("Tabla 2: análisis bivariado género y autoeficacia"),
           )

#Regresión lineal

LM1 <- lm(ICTEFFIC ~ ST004D01T, data = datos)

sjPlot::tab_model(LM1,
                  title = "Tabla 3",
                  p.style = "stars",
                  dv.labels = c("Regresión Lineal"),
                  p.threshold = c(0.05, 0.01, 0.001),
                  show.se = TRUE,
                  digits =3)
