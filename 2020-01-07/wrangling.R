########### 1 Thread: negros na educação superior. Estudantes vs professores. ----

#### 1.1 Pacotes ----

"%ni%" <- Negate("%in%")

library(dplyr)
library(stringr)
library(tidyr)

#### 1.2 Carregando datasets ----
# Arquivo produzido a partir do ffdf (produzido previamente) da tabela de Alunos do Censo de 2018.
# O código utilizado para produzir o ffdf está disponível em github.com/luangcandido/TCC
# Nesta etapa, fazemos filtragens para manter apenas registros da graduação presencial de IES Públicas.
ALUNOS2018 <- ffbase::load.ffdf("ALUNOS2018")
ALUNOS2018 <- ffbase2::tbl_ffdf(ALUNOS2018[["DM_ALUNO"]])
rm(DM_ALUNO)
ALUNOS2018 <- ALUNOS2018 %>% select(
  ID_ALUNO,
  CO_ALUNO_CURSO,
  CO_CURSO,
  CO_IES,
  TP_CATEGORIA_ADMINISTRATIVA,
  TP_ORGANIZACAO_ACADEMICA,
  TP_SITUACAO,
  NU_ANO_INGRESSO,
  TP_NIVEL_ACADEMICO,
  TP_MODALIDADE_ENSINO,
  TP_COR_RACA
) %>%
  as_tibble() %>%
  filter(TP_NIVEL_ACADEMICO == 1) %>%
  filter(TP_MODALIDADE_ENSINO == 1) %>%
  filter(TP_CATEGORIA_ADMINISTRATIVA %in% c(1, 2, 3))

# Tabela de docentes do Censo de 2018.
DOCENTES2018 <- read.csv("DOCENTE2018.CSV", sep = "|")
# Tabela de IES do Censo de 2018.
IES2018 <- read.csv("IES2018.CSV", sep = "|")

#### 1.3 Calculando o percentual de alunos por IES por raça. ----

ALUNOS2018_1 <- ALUNOS2018 %>%
  mutate(
    "CO_CATEGORIA_ADMINISTRATIVA" = case_when(
      TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "Pública Federal",
      TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "Pública Estadual",
      TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "Pública Municipal"
    )
  ) %>%
  mutate(
    "TP_COR_RACA" = case_when(
      TP_COR_RACA %in% c(0, 9) ~ "Não declarada",
      TP_COR_RACA == 1 ~ "Branco",
      TP_COR_RACA %in% c(2, 3) ~ "Negro",
      TP_COR_RACA == 4 ~ "Amarelo",
      TP_COR_RACA == 5 ~ "Indígena"
    )
  ) %>%
  group_by(CO_IES, TP_COR_RACA) %>%
  summarize(
    CURn = sum(TP_SITUACAO == "2"),
    FORn = sum(TP_SITUACAO == "6" |
                 TP_SITUACAO == "1"),
    MATn = sum(TP_SITUACAO == "2" |
                 TP_SITUACAO == "6"),
    INGTn = sum(NU_ANO_INGRESSO == 2018)
  )

ALUNOS2018_2 <- ALUNOS2018_1 %>% group_by(CO_IES) %>%
  summarise(
    TOTAL_CURn = sum(CURn),
    TOTAL_FORn = sum(FORn),
    TOTAL_MATn = sum(MATn),
    TOTAL_INGTn = sum(INGTn)
  )


ALUNOS2018_3 <-
  inner_join(ALUNOS2018_1, ALUNOS2018_2, by = "CO_IES", all.x = TRUE)

ALUNOS2018_4 <- ALUNOS2018_3 %>%
  group_by(CO_IES, TP_COR_RACA) %>%
  mutate(
    CURn_Percent =  round(CURn / TOTAL_CURn, digits = 3),
    FORn_Percent =  round(FORn / TOTAL_FORn, digits = 3),
    MATn_Percent =  round(MATn / TOTAL_MATn, digits = 3),
    INGTn_Percent = round(INGTn / TOTAL_INGTn, digits = 3)
  )

complemento_ies <-
  IES2018 %>% select(CO_IES, NO_IES, SG_IES, CO_REGIAO,
                     CO_UF, CO_MUNICIPIO)

ALUNOS2018_5 <-
  inner_join(ALUNOS2018_4,
             complemento_ies,
             by = "CO_IES",
             all.x = TRUE) %>%
  select(CO_IES,
         NO_IES,
         SG_IES,
         CO_REGIAO,
         CO_UF,
         CO_MUNICIPIO,
         TP_COR_RACA,
         everything())

#### 1.4 Calculando percentual de professores por IES por raça. ----
DOCENTES2018_1 <- DOCENTES2018 %>% select(CO_IES, TP_COR_RACA)
DOCENTES2018_2 <- DOCENTES2018_1 %>%
  mutate(
    "TP_COR_RACA" = case_when(
      TP_COR_RACA %in% c(0, 6) ~ "Não declarada",
      TP_COR_RACA == 1 ~ "Branco",
      TP_COR_RACA %in% c(2, 3) ~ "Negro",
      TP_COR_RACA == 4 ~ "Amarelo",
      TP_COR_RACA == 5 ~ "Indígena"
    )
  ) %>%
  group_by(CO_IES, TP_COR_RACA) %>%
  summarise("Freq" = n())

DOCENTES2018_3 <- DOCENTES2018_1 %>%
  group_by(CO_IES) %>%
  summarise("Total_Freq" = n())

DOCENTES2018_4 <-
  inner_join(DOCENTES2018_2,
             DOCENTES2018_3,
             by = "CO_IES",
             all.x = TRUE) %>%
  filter(CO_IES %in% ALUNOS2018_5$CO_IES) %>%
  mutate("Freq_Percent" = round(Freq / Total_Freq, digits = 3))

#### 1.5 Dataset 1: matriculados vs professores ----
DF1_PART1 <- ALUNOS2018_5 %>% select(
  CO_IES,
  NO_IES,
  SG_IES,
  CO_REGIAO,
  CO_UF,
  CO_MUNICIPIO,
  TP_COR_RACA,
  TOTAL_MATn,
  MATn_Percent
)

DF1_PART2 <- DOCENTES2018_4 %>% select(CO_IES, TP_COR_RACA,
                                       Freq_Percent)

DF1 <-
  inner_join(DF1_PART1, DF1_PART2, by = c("CO_IES", "TP_COR_RACA"))
