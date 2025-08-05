# Seteo de sesión

library(pacman)

pacman::p_load(
  dplyr, haven, sjlabelled,  psych,  purrr,  tidyr,  sjPlot,  ggplot2, 
  parameters,  table1,  car,  beeswarm,  lme4, scales, ggrepel, corrplot,
  ggtext, patchwork, lavaan, semTools, DT, knitr, kableExtra)

options(scipen = 999)
rm(list = ls())

# Cargamos los datos

pisa <- readRDS("./input/proc_data/pisa22_proc.rds")

# Exploratory factor analysis (No sabía si poner esta parte)

pisa_efa <- pisa %>% 
  select(search_info, asses_info, share_info, pair_collab, how_to_share, 
         edit_text, collect_data, create_pres, page_web, change_settings, 
         select_app, create_program, identify_error, logical_solution)
corMat <- round(cor(pisa_efa), 2)
KMO(corMat) 
cortest.bartlett(corMat, n = 393607)

fac_pa <- fa(r = pisa_efa, nfactors = 3, fm= "pa")
fac_pa

# CFA pooled model

model_cfa <- '
  gen_dse = ~ asses_info + share_info + pair_collab + how_to_share + 
         edit_text
  spec_dse = ~ create_program + identify_error + logical_solution
  '

m_cfa <- cfa(model = model_cfa, 
              data = pisa, 
              estimator = "DWLS",
              ordered = T,
              std.lv = F)

standardizedsolution(m_cfa) %>% 
  filter(op == "=~") %>% 
  select(Factor = lhs, Indicator = rhs, `Loading (DWLS)` = est.std) 


summary(m_cfa)

fit_pisa <- fitMeasures(m_cfa,
                         c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

knitr::kable(fit_pisa, digits = 3)

# CFA multigroup

multigroup_cfa <- '
  gen_dse = ~ asses_info + share_info + pair_collab + how_to_share + 
         edit_text
  spec_dse = ~ create_program + identify_error + logical_solution
  '

# multigroup_cfa <- '
#    gen_dse = ~ search_info + asses_info + share_info + pair_collab + how_to_share + 
#         edit_text + collect_data + create_pres + change_settings + select_app
#    spec_dse = ~ create_program + identify_error + logical_solution
#  '

# Invariance across countries

# Modelo configural
fitgroup <- cfa(model = multigroup_cfa, 
                data = pisa, 
                group = "CNT",
                ordered = TRUE
              )

# demora 2 minutos


# Modelo métrico (fijando las cargas)

fitgroup_metric <- cfa(model = multigroup_cfa, 
                       data = pisa, 
                       group = "CNT",
                       ordered = TRUE,
                      # missing = "fiml"
                      group.equal = "loadings")

# 3 minutos

# Modelo escalar (fijando cargas y interceptos)

fitgroup_scalar <- cfa(model = multigroup_cfa, 
                  data = pisa, 
                  group = "CNT",
                  ordered = TRUE,
                #  missing = "fiml",
                  group.equal = c("loadings", "intercepts"))

# 11 minutos... lo saltamos? quitamos el ordered?

# Extracción de indicadores de ajuste

fitmeasures_config <- fitMeasures(fitgroup,
                         c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

fitmeasures_metric <- fitMeasures(fitgroup_metric,
                         c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

fitmeasures_scalar <- fitMeasures(fitgroup_scalar,
                         c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

# Todo se demora aprox 2 minutos

# Tabla resumen (html y opción latex)

tabla_completa <- data.frame(
  Modelo = c("1. Configural", "2. Métrico", "3. Escalar"),
  chisq = c(fitmeasures_config["chisq"], fitmeasures_metric["chisq"], fitmeasures_scalar["chisq"]),
  df = c(fitmeasures_config["df"], fitmeasures_metric["df"], fitmeasures_scalar["df"]),
  CFI = c(fitmeasures_config["cfi"], fitmeasures_metric["cfi"], fitmeasures_scalar["cfi"]),
  TLI = c(fitmeasures_config["tli"], fitmeasures_metric["tli"], fitmeasures_scalar["tli"]),
  RMSEA = c(fitmeasures_config["rmsea"], fitmeasures_metric["rmsea"], fitmeasures_scalar["rmsea"]),
  SRMR = c(fitmeasures_config["srmr"], fitmeasures_metric["srmr"], fitmeasures_scalar["srmr"])
)

# Cálculo deltas

tabla_completa$delta_chisq <- c(NA, diff(tabla_completa$chisq))
tabla_completa$delta_df    <- c(NA, diff(tabla_completa$df))
tabla_completa$delta_CFI   <- c(NA, diff(tabla_completa$CFI))
tabla_completa$delta_RMSEA <- c(NA, diff(tabla_completa$RMSEA))

# ANOVA para significancia

test_anova <- anova(fitgroup, fitgroup_metric, fitgroup_scalar)
# 3-4 minutos

tabla_completa$p_value_delta_chisq <- test_anova$`Pr(>Chisq)`

invariance_table_cnt <- knitr::kable(
  tabla_completa,
  format = "html",
  digits = 3,
  caption = "Análisis de invarianza por países",
col.names = c(
  "Modelo", 
  "&#x03C7;<sup>2</sup>",  # chi²
  "df", "CFI", "TLI", "RMSEA", "SRMR", 
  "&#x0394;&#x03C7;<sup>2</sup>",  # Δχ²
  "&#x0394;df",              # Δdf
  "&#x0394;CFI",             # ΔCFI
  "&#x0394;RMSEA",           # ΔRMSEA
  "p-valor (&#x0394;&#x03C7;<sup>2</sup>)" # p-valor (Δχ²)
),
  booktabs = TRUE,
  escape=FALSE
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


save_kable(invariance_table_cnt, file = "output/graficos_y_tablas/tabla_invarianza_pais.html")

# Sex invariance analysis

# Modelo configural

pisa$sex <- as.factor(pisa$sex)

fitgroup_sx <- cfa(model = multigroup_cfa, 
                data = pisa, 
                group = "sex",
                ordered = TRUE
                #missing = "fiml"
              )

              
# Modelo métrico (fijando las cargas)

fitgroup_metric_sx <- cfa(model = multigroup_cfa, 
                       data = pisa, 
                       group = "sex",
                       ordered = TRUE,
                      # missing = "fiml"
                      group.equal = "loadings")

# Modelo escalar (fijando cargas y interceptos)

fitgroup_scalar_sx <- cfa(model = multigroup_cfa, 
                  data = pisa, 
                  group = "sex",
                  ordered = TRUE,
                #  missing = "fiml",
                  group.equal = c("loadings", "intercepts"))

# Extracción de indicadores de ajuste

fitmeasures_config_sx <- fitMeasures(fitgroup_sx,
                         c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

fitmeasures_metric_sx <- fitMeasures(fitgroup_metric_sx,
                         c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

fitmeasures_scalar_sx <- fitMeasures(fitgroup_scalar_sx,
                         c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

# Tabla resumen (html y opción latex)

tabla_completa_sx <- data.frame(
  Modelo = c("1. Configural", "2. Métrico", "3. Escalar"),
  chisq = c(fitmeasures_config_sx["chisq"], fitmeasures_metric_sx["chisq"], fitmeasures_scalar_sx["chisq"]),
  df = c(fitmeasures_config_sx["df"], fitmeasures_metric_sx["df"], fitmeasures_scalar_sx["df"]),
  CFI = c(fitmeasures_config_sx["cfi"], fitmeasures_metric_sx["cfi"], fitmeasures_scalar_sx["cfi"]),
  TLI = c(fitmeasures_config_sx["tli"], fitmeasures_metric_sx["tli"], fitmeasures_scalar_sx["tli"]),
  RMSEA = c(fitmeasures_config_sx["rmsea"], fitmeasures_metric_sx["rmsea"], fitmeasures_scalar_sx["rmsea"]),
  SRMR = c(fitmeasures_config_sx["srmr"], fitmeasures_metric_sx["srmr"], fitmeasures_scalar_sx["srmr"])
)

# Cálculo deltas

tabla_completa_sx$delta_chisq <- c(NA, diff(tabla_completa_sx$chisq))
tabla_completa_sx$delta_df    <- c(NA, diff(tabla_completa_sx$df))
tabla_completa_sx$delta_CFI   <- c(NA, diff(tabla_completa_sx$CFI))
tabla_completa_sx$delta_RMSEA <- c(NA, diff(tabla_completa_sx$RMSEA))

# ANOVA para significancia

test_anova_sx <- anova(fitgroup_sx, fitgroup_metric_sx, fitgroup_scalar_sx)

tabla_completa_sx$p_value_delta_chisq <- test_anova_sx$`Pr(>Chisq)`

# Tabla en formato latex

invariance_table_sx <- knitr::kable(
  tabla_completa_sx,
  format = "html",
  digits = 3,
  caption = "Análisis de invarianza por sexo",
col.names = c(
  "Modelo", 
  "&#x03C7;<sup>2</sup>",  # chi²
  "df", "CFI", "TLI", "RMSEA", "SRMR", 
  "&#x0394;&#x03C7;<sup>2</sup>",  # Δχ²
  "&#x0394;df",              # Δdf
  "&#x0394;CFI",             # ΔCFI
  "&#x0394;RMSEA",           # ΔRMSEA
  "p-valor (&#x0394;&#x03C7;<sup>2</sup>)" # p-valor (Δχ²)
),
  escape = FALSE,
  booktabs = TRUE # Usar booktabs = TRUE genera tablas más profesionales en LaTeX
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

save_kable(invariance_table_sx, file = "output/graficos_y_tablas/tabla_invarianza_sexo.html")

# Cargas factoriales por país SOlO CON MULTIGRUPO PAÍS SIN GROUP EQUAL

# Obtenemos todos los parámetros

format(Sys.time(), tz = "Chile/Continental")

all_params <- parameterEstimates(fitgroup, standardized = TRUE)

# Operador =~

factor_loadings_all_groups <- all_params %>%
  filter(op == "=~")

# Extraemos las labels

group_labels <- lavInspect(fitgroup, "group.label")

# Dataframe con todos los países

country_map <- data.frame(group = 1:length(group_labels), CNT = unlist(group_labels))

# Dataframe con ambas dimensiones

factor_loadings_all_groups <- factor_loadings_all_groups %>%
  left_join(country_map, by = "group")

# Dataframe gen dse

loadings_gen_dse <- factor_loadings_all_groups %>%
  filter(lhs == "gen_dse") %>%
  select(CNT, item = rhs, loading = std.all, latent_factor = lhs)

# Etiquetas tres países con peor ajuste

labels_gen_dse <- loadings_gen_dse %>%
  group_by(item) %>%
  slice_min(order_by = loading, n = 3) %>%
  ungroup()

# Dataframe spec dse

loadings_spec_dse <- factor_loadings_all_groups %>%
  filter(lhs == "spec_dse") %>%
  select(CNT, item = rhs, loading = std.all, latent_factor = lhs)

# Etiquetas tres países con menor ajuste

labels_spec_dse <- loadings_spec_dse %>%
  group_by(item) %>%
  slice_min(order_by = loading, n = 3) %>%
  ungroup()

# Gráfico cleaveland autoeficacia general

library(forcats)

plot_gen_dse <- ggplot(loadings_gen_dse,
                       aes(x = loading,
                           y = fct_reorder(item, loading, .fun = median, .desc = FALSE))) +
  geom_point(aes(color = '#B42012'), size = 3, alpha = 0.7) + 
    geom_text(data = labels_gen_dse, 
            aes(label = CNT), 
            nudge_y = 0.3,
            size = 2.5,
            color = "black",
            check_overlap = TRUE) +
  scale_color_identity(name = "Factor", 
                       guide = "legend", 
                       labels = c("General")) +
  labs(title = "Cargas Factoriales: Autoeficacia General",
       x = "Carga Factorial Estandarizada",
       y = "Ítem") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top")

plot_gen_dse

# Cleaveland autoeficacia especializada
# Fijar el gráfico a x = .3
plot_spec_dse <- ggplot(loadings_spec_dse,
                        aes(x = loading,
                            y = fct_reorder(item, loading, .fun = median, .desc = FALSE))) +
  geom_point(aes(color = '#E16462'), size = 3, alpha = 0.7) +
    geom_text_repel(data = labels_spec_dse, 
                  aes(label = CNT), 
                  size = 2.5,
                  color = "black",
                  box.padding = 0.4,
                  point.padding = 0.2,
                  min.segment.length = 0,
                  max.overlaps = Inf) +
  scale_color_identity(name = "Factor", 
                       guide = "legend", 
                       labels = c("Especializada")) +
  labs(title = "Cargas Factoriales: Autoeficacia Especializada", 
       x = "Carga Factorial Estandarizada",
       y = "Ítem") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top")

plot_spec_dse

# SEM por separado para cada país

lista_paises <- unique(unlist(pisa$CNT))

resultados_lista <- list()
summary_lista <- list()

# Iteramos por cada país con el mismo modelo
for (pais_actual in lista_paises) {
  format(Sys.time(), tz = "Chile/Continental")
  datos_pais <- pisa %>%
    filter(CNT == pais_actual)
  
  fit_pais <- tryCatch({
    
    sem(model = multigroup_cfa, 
        data = datos_pais,
        ordered = TRUE
       # missing = "fiml"
       )
  }, error = function(e) {
    
     cat("  -> ¡ERROR! No se pudo ajustar el modelo para", pais_actual, ". Razón:", conditionMessage(e), "\n")
     return(NULL)
  })
    
  if (!is.null(fit_pais)) {
     if (lavInspect(fit_pais, "converged")) {
        medidas_ajuste <- fitMeasures(fit_pais)
        resultados_lista[[pais_actual]] <- medidas_ajuste
     } else {
        cat("  -> ¡ADVERTENCIA! El modelo para", pais_actual, "no convergió. Se omitirán sus resultados.\n")
        resultados_lista[[pais_actual]] <- rep(NA)
        
     }
  }
  format(Sys.time(), tz = "Chile/Continental")
}

### Test con chile (o otros países)

datos_test <- pisa %>%
    filter(CNT == "CHL")

test_model <- sem(model = multigroup_cfa, 
        data = datos_test,
        ordered = TRUE)

fitMeasures(test_model)

# Convertimos resultados_lista en dataframe y agregamos CNT para detectar país

fit_indices_por_pais <- do.call(rbind, resultados_lista)

fit_indices_por_pais <- as.data.frame(fit_indices_por_pais)

fit_indices_por_pais <- fit_indices_por_pais %>%
  tibble::rownames_to_column(var = "CNT")

# Seleccionamos indicadores de interés

indices_ajuste <- fit_indices_por_pais %>% 
  select(CNT, chisq, df, rmsea, rmsea.ci.lower,  rmsea.ci.upper, srmr, cfi, tli)

datatable(
  indices_ajuste,
  options = list(
    pageLength = 10,
    autoWidth = TRUE,
    searchHighlight = TRUE
  ),
  filter = 'top',
  rownames = FALSE,
  caption = "Índices de Ajuste del Modelo (Interactiva)"
)
# Quitar decimales a 3
table1::table1(~ . + -CNT,data=indices_ajuste)


group_summary <- summary(fitgroup)

valores_chi <- group_summary$test$standard$stat.group

nombres_paises <- group_summary$data$group.label

df_chi <- data.frame(
  Pais = nombres_paises,
  Chi.Cuadrado = valores_chi
)

knitr::kable(df_chi)

datatable(
  df_chi,
  options = list(
    pageLength = 10,
    autoWidth = TRUE,
    searchHighlight = TRUE
  ),
  filter = 'top',
  rownames = FALSE,
  caption = "Chi cuadrado por país"
)

fitMeasures(fitgroup)
