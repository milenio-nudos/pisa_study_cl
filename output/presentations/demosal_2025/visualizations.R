pacman::p_load(dplyr, haven, psych, purrr, tidyr, sjPlot, ggplot2,
               parameters, table1, beeswarm, lme4, rio, skimr, effects,
               labelled, knitr, kableExtra, stringr, sjmisc, purr, ggbeeswarm,
               sjlabelled)

options(scipen = 999) 
options(digits = 2)
rm(list = ls()) 

variables_labels <- c(
  "buscar_info",
  "evaluar_info",
  "compartir_info", 
  "colaboracion_pares",
  "explicar_info_pares",
  "editar_texto",
  "manejar_datos",
  "crear_presentacion",
  "manejar_pag_web",
  "configuracion_privacidad",
  "selecc_mejor_app",
  "crear_programa",
  "hallar_error_programa",
  "representar_solucion_logica")

pisa22ict <- readRDS("../../../input/proc_data/pisa22ict.rds")

# "No sé" como NA
pisa22ict <- pisa22ict|>
  mutate(across(starts_with("IC183Q0"),~ ifelse(. == 5, NA, .)))

# Recodificación de variables
var_label(pisa22ict) <- list(
  IC183Q01JA = "buscar_info", 
  IC183Q02JA = "evaluar_info", 
  IC183Q03JA = "compartir_info", 
  IC183Q04JA = "colaboracion_pares",
  IC183Q05JA = "explicar_info_pares",
  IC183Q07JA = "editar_texto", 
  IC183Q08JA = "manejar_datos", 
  IC183Q09JA = "crear_presentacion", 
  IC183Q10JA = "manejar_pag_web",
  IC183Q12JA = "configuracion_privacidad",
  IC183Q13JA = "selecc_mejor_app", 
  IC183Q14JA = "crear_programa", 
  IC183Q15JA = "hallar_error_programa", 
  IC183Q16JA = "representar_solucion_logica"
)

pisa22ict <- pisa22ict %>% 
  mutate(OCDE = if_else(CNT %in% c("AUS", "AUT", "BEL", "CHL", "CRI",
                                   "CZE", "DNK", "EST", "FIN", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LTU", "LVA", "POL", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA"), 1, 0))

efficacy_variables <- pisa22ict |>
  select(CNT, #Country
         IC183Q01JA, IC183Q02JA, IC183Q03JA, IC183Q04JA, IC183Q05JA,
         IC183Q07JA, IC183Q08JA, IC183Q09JA, IC183Q10JA, IC183Q12JA,
         IC183Q13JA, IC183Q14JA, IC183Q15JA, IC183Q16JA)

# 1. Presentar tipos de autoeficacia ----
beeswarm_plot <- pisa22ict|>
  group_by(CNT)|>
  summarise(mean_geneff = mean(effgen, na.rm = T),
            mean_speceff = mean(effspec, na.rm = T))|>
  ungroup()|>
  pivot_longer(cols = 2:3,
               names_to = "type",
               values_to = "score")

beeswarm_labels <- beeswarm_plot%>%
  group_by(type) %>%
  arrange(score) %>%
  slice(c(1:3, (n() - 2):n())) %>% # First three and last three countries
  ungroup() %>%
  mutate(CNT = to_label(CNT))

  
ggplot(beeswarm_plot, aes(y = score, x = type, color = type)) +
  geom_beeswarm() +
  geom_text(
    data = beeswarm_labels, # Use only the filtered data for labels
    aes(label = CNT), # Add country labels
    hjust = -0.2, vjust = 0.5, size = 3, color = "black"
  ) +
  scale_color_manual(values = c("#fe3057", "#5f5758")) +
  labs(
    title = "Distribución de promedios por tipo de Autoeficacia Digital",
    subtitle = "Promedios entre países"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = c("General", "Especializada"))
  
  
# 2. Promedio de autoeficacia entre países -----

#Opcion 1
pisa22ict|>
  group_by(CNT)|>
  summarise(mean_speceff = mean(effspec, na.rm = T),
            mean_geneff = mean(effgen, na.rm = T))|>
  ungroup()|>
  pivot_longer(cols = 2:3,
               names_to = "type",
               values_to = "score")|>
  mutate(CNT = to_label(CNT))|>
  
  ggplot(aes(y = reorder(CNT, -score), x = score)) +
    geom_line(aes(group = CNT), size = 0.8) +
    geom_point(aes(color = type), size= 2.5) +
    scale_color_manual(values = c("#fe3057","#5f5758"),
                       labels = c("Especializada", "General") ) +
    labs(title = "Puntajes en los tipos de Autoeficacia digital",
          subtitle = "Promedios entre países",
          color = "Tipo",
          x = "Puntaje en la escala") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "top")

# Opcion 2
pisa22ict|>
  group_by(CNT)|>
  summarise(Especializada = mean(effspec, na.rm = T),
            General = mean(effgen, na.rm = T))|>
  ungroup()|>
  pivot_longer(cols = 2:3,
               names_to = "type",
               values_to = "score")|>
  mutate(CNT = to_label(CNT))|>
  
  ggplot(aes(y = reorder(CNT, -score), x = score)) +
  geom_point(aes(color = type), size= 2.5) +
  facet_wrap(~type) +
  scale_color_manual(values = c("#5f5758","#fe3057")) +
  labs(title = "Puntajes en los tipos de Autoeficacia digital",
       subtitle = "Promedios entre países",
       color = "Tipo",
       x = "Puntaje en la escala") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top")

#Opcion 3
pisa22ict|>
  group_by(CNT)|>
  summarise(General = mean(effgen, na.rm = T))|>
  ungroup()|>
  mutate(CNT = to_label(CNT))|>
  
  ggplot(aes(y = reorder(CNT, -General), x = General)) +
  geom_point(color = "#5f5758", size= 2.5) +
  labs(title = "Puntajes en Autoeficacia General",
       subtitle = "Promedios por país",
       x = "Puntaje en la escala") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

pisa22ict|>
  group_by(CNT)|>
  summarise(Especializada = mean(effspec, na.rm = T))|>
  ungroup()|>
  mutate(CNT = to_label(CNT))|>
  
  ggplot(aes(y = reorder(CNT, -Especializada), x = Especializada)) +
  geom_point(color = "#fe3057", size= 2.5) +
  labs(title = "Puntajes en Autoeficacia Especializada",
       subtitle = "Promedios por país",
       x = "Puntaje en la escala") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())


    

# 3. Promedio de autoeficacia básica por género entre países ----

pisa22ict|>
  group_by(CNT,sex)|>
  summarise(mean_geneff = mean(effgen, na.rm = T))|>
  ungroup()|>
  mutate(CNT = to_label(CNT),
         sex = to_label(sex))|>
  drop_na()|>
  
  ggplot(aes(y = reorder(CNT, mean_geneff), x = mean_geneff)) +
  geom_line(aes(group = CNT), size = 0.8) +
  geom_point(aes(color = sex), size= 2.5) +
  scale_color_manual(values = c("#fe3057","#5f5758")) +
  labs(title = "Diferencias de género en autoeficacia digital General",
       subtitle = "Promedios en cada país por género",
       color = "Género",
       x = "Puntaje en la escala") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top")

pisa22ict|>
  group_by(CNT,sex)|>
  summarise(mean_speceff = mean(effspec, na.rm = T))|>
  ungroup()|>
  mutate(CNT = to_label(CNT),
         sex = to_label(sex))|>
  drop_na()|>
  
  ggplot(aes(y = reorder(CNT, mean_speceff), x = mean_speceff)) +
  geom_line(aes(group = CNT), size = 0.8) +
  geom_point(aes(color = sex), size= 2.5) +
  scale_color_manual(values = c("#fe3057","#5f5758")) +
  labs(title = "Diferencias de género en autoeficacia digital Especializada",
       subtitle = "Promedios en cada país por género") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top")
  
# 4. Promedio de la magnitud de brechas de género por país ----

# Calculate the wide-format data and mean_gap
wide_data <- pisa22ict %>%
  group_by(CNT, sex) %>%
  summarise(mean_geneff = mean(effgen, na.rm = TRUE), .groups = 'drop') %>%
  ungroup() %>%
  mutate(CNT = to_label(CNT),
         sex = to_label(sex)) %>%
  drop_na() %>%
  pivot_wider(
    names_from = sex, # Column to use for new column names
    values_from = mean_geneff, # Column to use for values
    names_glue = "score_of_{sex}" # Customize new column names
  ) %>%
  mutate(gap = score_of_Male - score_of_Female) %>%
  mutate(mean_gap = mean(gap)) # Calculate mean_gap

# Extract the mean_gap value
mean_gap_value <- wide_data %>% pull(mean_gap) %>% unique()

# Create the plot
ggplot(wide_data, aes(y = reorder(CNT,-gap), x = gap)) +
  geom_point(aes(group = CNT), size = 3, color = "#fe3057") + # Add points
  geom_vline(xintercept = mean_gap_value, color = "#5f5758", linetype = "dashed", size = 1) + # Add mean_gap line
  labs(
    title = "Magnitud de la brecha de género en autoeficacia general por país",
  ) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Calculate the wide-format data and mean_gap
wide_data <- pisa22ict %>%
  group_by(CNT, sex) %>%
  summarise(mean_spec = mean(effspec, na.rm = TRUE), .groups = 'drop') %>%
  ungroup() %>%
  mutate(CNT = to_label(CNT),
         sex = to_label(sex)) %>%
  drop_na() %>%
  pivot_wider(
    names_from = sex, # Column to use for new column names
    values_from = mean_spec, # Column to use for values
    names_glue = "score_of_{sex}" # Customize new column names
  ) %>%
  mutate(gap = score_of_Male - score_of_Female) %>%
  mutate(mean_gap = mean(gap)) # Calculate mean_gap

# Extract the mean_gap value
mean_gap_value <- wide_data %>% pull(mean_gap) %>% unique()

# Create the plot
ggplot(wide_data, aes(y = reorder(CNT,-gap), x = gap)) +
  geom_point(aes(group = CNT), size = 3, color = "#fe3057") + # Add points
  geom_vline(xintercept = mean_gap_value, color = "#5f5758", linetype = "dashed", size = 1) + # Add mean_gap line
  labs(
    title = "Magnitud de la brecha de género en autoeficacia general por país",
  ) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

# 5. Scatterplots variables país (ISMA)