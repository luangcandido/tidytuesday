########### 2 Gráficos da Thread: negros na educação superior. Estudantes vs professores. ----

#### 2.1 Pacotes ----

library(ggplot2)
library(extrafont)
library(dplyr)
library(geobr)
library(sf)

font_import(pattern = "[M/m]ontserrat")
loadfonts(device = "win")


#### 2.2 Scatter 1 - Negros ----
DF1_NEGROS <- subset(DF1, TP_COR_RACA == "Negro")
DF1_ND <- subset(DF1, TP_COR_RACA == "Não declarada") %>% 
  ungroup() %>% 
  select(CO_IES, Freq_Percent) %>% 
  rename("Freq_NaoDeclarado" = Freq_Percent)
DF1_NEGROS <- inner_join(DF1_NEGROS, DF1_ND, by = "CO_IES") %>% 
  mutate(
    "Freq_NaoDeclarado" = case_when(
      Freq_NaoDeclarado > 0.75 ~ "> 75%",
      Freq_NaoDeclarado <= 0.75 ~ "< 75%"
    )
  ) %>% 
  mutate(
    "TOTAL_MATn_factor" = case_when(
      between(TOTAL_MATn, 0, 15000) ~ "0 - 15 mil",
      between(TOTAL_MATn, 15000, 30000) ~ "15 mil - 30 mil",
      TOTAL_MATn > 30000 ~ "> 30 mil"
    )
  )

DF1_NEGROS <- DF1_NEGROS %>%
  mutate(
    "CO_REGIAO" = case_when(
      CO_REGIAO == 1 ~ "Norte",
      CO_REGIAO == 2 ~ "Nordeste",
      CO_REGIAO == 3 ~ "Sudeste",
      CO_REGIAO == 4 ~ "Sul",
      CO_REGIAO == 5 ~ "Centro-Oeste"
    )
  )

DF1_NEGROS$TOTAL_MATn_factor <- factor(DF1_NEGROS$TOTAL_MATn_factor,
                                       levels = 
                                         c("0 - 15 mil",
                                           "15 mil - 30 mil",
                                           "> 30 mil"))

ggplot(subset(DF1_NEGROS, Freq_NaoDeclarado == "< 75%"),
       aes(
         x = MATn_Percent,
         y = Freq_Percent,
         size = TOTAL_MATn_factor,
         colour = factor(CO_REGIAO)
       )) +
  geom_point(alpha = 0.65) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = 2) +
  annotate("text", 
           x = 0.70, y = 0.705,
           size = 6,
           label = "Mesmo percentual de professores\ne alunos matriculados negros",
           angle = 37.1,
           family = "Montserrat") +
  scale_colour_brewer(palette = "BuPu") +
  scale_size_discrete(range = c(3, 17)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  guides(
    colour = guide_legend(title = "Região",
                          title.position = "top",
                          override.aes = list(size = 5)),
    size = guide_legend(title = "N. de estudantes\nmatriculados",
                        title.position = "top")
  ) +
  labs(
    x = "Percentual de estudantes matriculados autodeclarados negros",
    y = "Percentual de professores autodeclarados negros",
    title = "Relação entre o percentual de estudantes matriculados negros\ne o percentual de professores negros",
    subtitle = "Para as Instituições de Ensino Superior Públicas em 2018",
    tag = "Fonte: Censo da Educação Superior (2018)\nElaboração: @luangcandido"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.justification = "center",
    legend.key.width = unit(1.5, "lines"),
    legend.key.height = unit(0.9, "lines"),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(face = "italic", size = 15),
    plot.caption = element_text(size = 13),
    legend.text = element_text(colour = "gray50", size = 13),
    legend.title = element_text(colour = "gray50", size = 15),
    plot.tag = element_text(margin = margin(0.1,-19,0,0, unit = "cm"), hjust = 1),
    plot.tag.position = "bottom",
    axis.text.x = element_text(colour = "gray50", size = 13),
    axis.title.x = element_text(colour = "gray50", size = 15),
    axis.text.y = element_text(colour = "gray50", size = 13),
    axis.title.y = element_text(colour = "gray50", size = 15)
  )

#### 2.3 Scatter 2 - Brancos ----
DF1_BRANCOS <- subset(DF1, TP_COR_RACA == "Branco")
DF1_ND <- subset(DF1, TP_COR_RACA == "Não declarada") %>% 
  ungroup() %>% 
  select(CO_IES, Freq_Percent) %>% 
  rename("Freq_NaoDeclarado" = Freq_Percent)
DF1_BRANCOS <- inner_join(DF1_BRANCOS, DF1_ND, by = "CO_IES") %>% 
  mutate(
    "Freq_NaoDeclarado" = case_when(
      Freq_NaoDeclarado > 0.75 ~ "> 75%",
      Freq_NaoDeclarado <= 0.75 ~ "< 75%"
    )
  ) %>% 
  mutate(
    "TOTAL_MATn_factor" = case_when(
      between(TOTAL_MATn, 0, 15000) ~ "0 - 15 mil",
      between(TOTAL_MATn, 15000, 30000) ~ "15 mil - 30 mil",
      TOTAL_MATn > 30000 ~ "> 30 mil"
    )
  )

DF1_BRANCOS <- DF1_BRANCOS %>%
  mutate(
    "CO_REGIAO" = case_when(
      CO_REGIAO == 1 ~ "Norte",
      CO_REGIAO == 2 ~ "Nordeste",
      CO_REGIAO == 3 ~ "Sudeste",
      CO_REGIAO == 4 ~ "Sul",
      CO_REGIAO == 5 ~ "Centro-Oeste"
    )
  )

DF1_BRANCOS$TOTAL_MATn_factor <- factor(DF1_BRANCOS$TOTAL_MATn_factor,
                                       levels = 
                                         c("0 - 15 mil",
                                           "15 mil - 30 mil",
                                           "> 30 mil"))


ggplot(subset(DF1_BRANCOS, Freq_NaoDeclarado == "< 75%"),
       aes(
         x = MATn_Percent,
         y = Freq_Percent,
         size = TOTAL_MATn_factor,
         colour = factor(CO_REGIAO)
       )) +
  geom_point(alpha = 0.65) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = 2) +
  annotate("text", 
           x = 0.70, y = 0.705,
           size = 6,
           label = "Mesmo percentual de professores\ne alunos matriculados brancos",
           angle = 36.9,
           family = "Montserrat") +
  scale_colour_brewer(palette = "Reds") +
  scale_size_discrete(range = c(3, 17)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  guides(
    colour = guide_legend(title = "Região",
                          title.position = "top",
                          override.aes = list(size = 5)),
    size = guide_legend(title = "N. de estudantes\nmatriculados",
                        title.position = "top")
  ) +
  labs(
    x = "Percentual de estudantes matriculados autodeclarados brancos",
    y = "Percentual de professores autodeclarados brancos",
    title = "Relação entre o percentual de estudantes matriculados brancos\ne o percentual de professores brancos",
    subtitle = "Para as Instituições de Ensino Superior Públicas em 2018",
    tag = "Fonte: Censo da Educação Superior (2018)\nElaboração: @luangcandido"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.justification = "center",
    legend.key.width = unit(1.5, "lines"),
    legend.key.height = unit(0.9, "lines"),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(face = "italic", size = 15),
    plot.caption = element_text(size = 13),
    legend.text = element_text(colour = "gray50", size = 13),
    legend.title = element_text(colour = "gray50", size = 15),
    plot.tag = element_text(margin = margin(0.1,-19,0,0, unit = "cm"), hjust = 1),
    plot.tag.position = "bottom",
    axis.text.x = element_text(colour = "gray50", size = 13),
    axis.title.x = element_text(colour = "gray50", size = 15),
    axis.text.y = element_text(colour = "gray50", size = 13),
    axis.title.y = element_text(colour = "gray50", size = 15)
  )

#### 2.3 Scatter 3 - Indígenas ----
DF1_IND <- subset(DF1, TP_COR_RACA == "Indígena")
DF1_ND <- subset(DF1, TP_COR_RACA == "Não declarada") %>% 
  ungroup() %>% 
  select(CO_IES, Freq_Percent) %>% 
  rename("Freq_NaoDeclarado" = Freq_Percent)
DF1_IND <- inner_join(DF1_IND, DF1_ND, by = "CO_IES") %>% 
  mutate(
    "Freq_NaoDeclarado" = case_when(
      Freq_NaoDeclarado > 0.75 ~ "> 75%",
      Freq_NaoDeclarado <= 0.75 ~ "< 75%"
    )
  ) %>% 
  mutate(
    "TOTAL_MATn_factor" = case_when(
      between(TOTAL_MATn, 0, 15000) ~ "0 - 15 mil",
      between(TOTAL_MATn, 15000, 30000) ~ "15 mil - 30 mil",
      TOTAL_MATn > 30000 ~ "> 30 mil"
    )
  )

DF1_IND <- DF1_IND %>%
  mutate(
    "CO_REGIAO" = case_when(
      CO_REGIAO == 1 ~ "Norte",
      CO_REGIAO == 2 ~ "Nordeste",
      CO_REGIAO == 3 ~ "Sudeste",
      CO_REGIAO == 4 ~ "Sul",
      CO_REGIAO == 5 ~ "Centro-Oeste"
    )
  )

DF1_IND$TOTAL_MATn_factor <- factor(DF1_IND$TOTAL_MATn_factor,
                                        levels = 
                                          c("0 - 15 mil",
                                            "15 mil - 30 mil",
                                            "> 30 mil"))


ggplot(subset(DF1_IND, Freq_NaoDeclarado == "< 75%"),
       aes(
         x = MATn_Percent,
         y = Freq_Percent,
         size = TOTAL_MATn_factor,
         colour = factor(CO_REGIAO)
       )) +
  geom_point(alpha = 0.65) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = 2) +
  annotate("text", 
           x = 0.70, y = 0.705,
           size = 6,
           label = "Mesmo percentual de professores\ne alunos matriculados indígenas",
           angle = 37.2,
           family = "Montserrat") +
  scale_colour_brewer(palette = "YlGn") +
  scale_size_discrete(range = c(3, 17)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  guides(
    colour = guide_legend(title = "Região",
                          title.position = "top",
                          override.aes = list(size = 5)),
    size = guide_legend(title = "N. de estudantes\nmatriculados",
                        title.position = "top")
  ) +
  labs(
    x = "Percentual de estudantes matriculados autodeclarados indígenas",
    y = "Percentual de professores autodeclarados indígenas",
    title = "Relação entre o percentual de estudantes matriculados indígenas\ne o percentual de professores indígenas",
    subtitle = "Para as Instituições de Ensino Superior Públicas em 2018",
    tag = "Fonte: Censo da Educação Superior (2018)\nElaboração: @luangcandido"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.justification = "center",
    legend.key.width = unit(1.5, "lines"),
    legend.key.height = unit(0.9, "lines"),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(face = "italic", size = 15),
    plot.caption = element_text(size = 13),
    plot.tag = element_text(margin = margin(0.1,-19,0,0, unit = "cm"), hjust = 1),
    plot.tag.position = "bottom",
    legend.text = element_text(colour = "gray50", size = 13),
    legend.title = element_text(colour = "gray50", size = 15),
    axis.text.x = element_text(colour = "gray50", size = 13),
    axis.title.x = element_text(colour = "gray50", size = 15),
    axis.text.y = element_text(colour = "gray50", size = 13),
    axis.title.y = element_text(colour = "gray50", size = 15)
  )

#### 2.4 Mapa 1 - Negros
DF1_NEGROS_SUBSET <- subset(DF1_NEGROS, Freq_NaoDeclarado == "< 75%")
mapa_br <- read_state(code_state = "all", year = 2000)
