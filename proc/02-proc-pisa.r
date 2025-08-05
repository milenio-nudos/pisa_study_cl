library(pacman)
p_load(dplyr, haven)
options(scipen = 999)
rm(list = ls())

pisa22 <- readRDS("input/proc_data/pisa22ict.rds")

pisa22_proc <- pisa22 %>%
  select(CNT, CNTRYID, CNTSCHID, sex, IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA,
         IC183Q05JA, IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q10JA,
         IC183Q12JA, IC183Q13JA, IC183Q14JA, IC183Q15JA, IC183Q16JA) # sex viene recodificada del script anterior

pisa22_proc <- pisa22_proc %>% rename("search_info"=IC183Q01JA,
                                      "asses_info"=IC183Q02JA,
                                      "share_info"=IC183Q03JA,
                                      "pair_collab"=IC183Q04JA,
                                      "how_to_share"=IC183Q05JA,
                                      "edit_text"=IC183Q07JA,
                                      "collect_data"=IC183Q08JA,
                                      "create_pres"=IC183Q09JA,
                                      "page_web"=IC183Q10JA,
                                      "change_settings"=IC183Q12JA,
                                      "select_app"=IC183Q13JA,
                                      "create_program"=IC183Q14JA,
                                      "identify_error"=IC183Q15JA,
                                      "logical_solution"=IC183Q16JA)

saveRDS(pisa22_proc, "input/proc_data/pisa22_proc.rds")
