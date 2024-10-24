#Cargar librerías

install.packages("pacman") 
pacman::p_load(dplyr,
               gginference,
               gtsummary,
               haven)

#Cargar base de datos. Esta ya fue procesada en otro script seleccionando solo el identificador de país chile.
#Esto debido al gran peso de la base internacional. 

data <- readRDS("input/data_orig/datos_stu_chile.rds")

#Seleccionar variables de interés

datos_proc <- data %>%
  select(CNTSCHID, CNTSTUID, STRATUM, ST001D01T, ST004D01T, starts_with("ST250"), starts_with("ST254"), ST253Q01JA, starts_with("ST326"), starts_with("ST322"), ST337Q08JA, ST338Q08JA, starts_with("IC"))

#Convertir a factor 

datos_proc <- datos_proc %>%
  mutate(across(c(ST004D01T, ST250Q02JA, ST250Q04JA, ST250Q05JA), haven::as_factor))

#Guardar base de datos procesada

saveRDS(datos_proc, "input/data_proc/datos_proc.rds")
