pacman::p_load(dplyr, haven)
options(scipen = 999) #Desactivar notación científica
rm(list = ls())       #Limpieza enviroment

pisa22 <- read_sav("input/raw_data/STU_QQQ.sav")

#Filtrar países que hayan aplicado ICT y seleccionar variables de interes
pisa22_proc <- pisa22 %>%
  filter(Option_ICTQ == 1) %>%
  select(CNT, CNTRYID, CNTSCHID, CNTSTUID, ST001D01T, ST004D01T, starts_with("ST322"), ST337Q08JA, ST338Q08JA, starts_with("IC"))

#Elimiar respuesta no sabe
pisa22_proc$IC183Q01JA[pisa22_proc$IC183Q01JA==5]<-NA
pisa22_proc$IC183Q02JA[pisa22_proc$IC183Q02JA==5]<-NA
pisa22_proc$IC183Q03JA[pisa22_proc$IC183Q03JA==5]<-NA
pisa22_proc$IC183Q04JA[pisa22_proc$IC183Q04JA==5]<-NA
pisa22_proc$IC183Q05JA[pisa22_proc$IC183Q05JA==5]<-NA
pisa22_proc$IC183Q07JA[pisa22_proc$IC183Q07JA==5]<-NA
pisa22_proc$IC183Q08JA[pisa22_proc$IC183Q08JA==5]<-NA
pisa22_proc$IC183Q09JA[pisa22_proc$IC183Q09JA==5]<-NA
pisa22_proc$IC183Q10JA[pisa22_proc$IC183Q10JA==5]<-NA
pisa22_proc$IC183Q12JA[pisa22_proc$IC183Q12JA==5]<-NA
pisa22_proc$IC183Q13JA[pisa22_proc$IC183Q13JA==5]<-NA
pisa22_proc$IC183Q14JA[pisa22_proc$IC183Q14JA==5]<-NA
pisa22_proc$IC183Q15JA[pisa22_proc$IC183Q15JA==5]<-NA
pisa22_proc$IC183Q16JA[pisa22_proc$IC183Q16JA==5]<-NA

##Construcción del índice autoeficacia específica y general
pisa22_proc <- pisa22_proc %>%
  mutate(
    effspec = rowMeans(select(., IC183Q10JA, IC183Q14JA, IC183Q15JA, IC183Q16JA), na.rm = TRUE),
    effgen = rowMeans(select(., IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA, IC183Q05JA, IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q12JA, IC183Q13JA), na.rm = TRUE),
    sex = ST004D01T
)

#summary(pisa22_proc$effspec)

#summary(pisa22_proc$effgen)

saveRDS(pisa22_proc, "input/proc_data/pisa22ict.rds")
