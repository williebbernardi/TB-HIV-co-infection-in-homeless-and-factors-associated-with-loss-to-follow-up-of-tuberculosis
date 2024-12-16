

##### Abrir e instalar pacotes


library("read.dbc")

library("tidyverse")



################################################################################
#                                                                              #
#   Download e processamento primário do banco de dados                        #
#                                                                              #
################################################################################


##### Baixar e abrir o banco de dados

#### Definir link do banco SINAN (Tranferencia de arquivos)

### Banco de dados tuberculose Brasil ano de 2015
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/TUBEBR15.dbc"
download.file(url, destfile = "data/TUBEBR15.dbc", mode = "wb")
tuber_15 <- read.dbc("data/TUBEBR15.dbc")

### Banco de dados tuberculose Brasil ano de 2016
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/TUBEBR16.dbc"
download.file(url, destfile = "data/TUBEBR16.dbc", mode = "wb")
tuber_16 <- read.dbc("data/TUBEBR16.dbc")

### Banco de dados tuberculose Brasil ano de 2017
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/TUBEBR17.dbc"
download.file(url, destfile = "data/TUBEBR17.dbc", mode = "wb")
tuber_17 <- read.dbc("data/TUBEBR17.dbc")

### Banco de dados tuberculose Brasil ano de 2018
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/TUBEBR18.dbc"
download.file(url, destfile = "data/TUBEBR18.dbc", mode = "wb")
tuber_18 <- read.dbc("data/TUBEBR18.dbc")

### Banco de dados tuberculose Brasil ano de 2019
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR19.dbc"
download.file(url, destfile = "data/TUBEBR19.dbc", mode = "wb")
tuber_19 <- read.dbc("data/TUBEBR19.dbc")

### Banco de dados tuberculose Brasil ano de 2020
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR20.dbc"
download.file(url, destfile = "data/TUBEBR20.dbc", mode = "wb")
tuber_20 <- read.dbc("data/TUBEBR20.dbc")

### Banco de dados tuberculose Brasil ano de 2021
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR21.dbc"
download.file(url, destfile = "data/TUBEBR21.dbc", mode = "wb")
tuber_21 <- read.dbc("data/TUBEBR21.dbc")

### Banco de dados tuberculose Brasil ano de 2022
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR22.dbc"
download.file(url, destfile = "data/TUBEBR22.dbc", mode = "wb")
tuber_22 <- read.dbc("data/TUBEBR22.dbc")

### Banco de dados tuberculose Brasil ano de 2023
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR23.dbc"
download.file(url, destfile = "data/TUBEBR23.dbc", mode = "wb")
tuber_23 <- read.dbc("data/TUBEBR23.dbc")


### Conferir se todas as variáveis são iguais

all.equal(colnames(tuber_15),colnames(tuber_16),
          colnames(tuber_17),colnames(tuber_18),
          colnames(tuber_19),colnames(tuber_20),
          colnames(tuber_21),colnames(tuber_22),
          colnames(tuber_23))


### Como todas as colunas estão corretas, podemos fazer a união das linhas dos bancos de dados

## Juntar as linhas dos bancos de dados - Long

tuber_15_23 <- rbind(tuber_15, tuber_16, tuber_17, tuber_18, tuber_19,
                     tuber_20, tuber_21, tuber_22, tuber_23)

nrow(tuber_15_23)

#### REMOVER NOTIFICAÇÕES DUPLICADAS

tuber_15_23 <- tuber_15_23 %>% 
  distinct(DT_NOTIFIC, DT_INIC_TR, CS_SEXO, FORMA,
           SG_UF_NOT, ID_MUNICIP, TRATAMENTO, SITUA_ENCE,
           DT_ENCERRA, NU_ANO, .keep_all = TRUE)


write.csv(tuber_15_23, "data/banco_tuber_total_2015_2023.csv")



################################################################################
#                                                                              #
#                     Tabulação do banco de dados                              #
#                                                                              #
################################################################################


tuber_15_23 <- read.csv("data/banco_tuber_total_2015_2023.csv")

nrow(tuber_15_23)


### Seleção das variáveis que serão usadas no estudo

tuber_proc <- tuber_15_23 %>% 
  select(NU_ANO, ID_MUNIC_A, CS_SEXO, CS_RACA, CS_ESCOL_N, NU_IDADE_N, 
         BENEF_GOV, POP_RUA, POP_LIBER, POP_SAUDE, POP_IMIG,
         FORMA, EXTRAPU1_N, TRATAMENTO,  RAIOX_TORA, 
         HIV, ANT_RETRO, AGRAVAIDS, AGRAVALCOO, AGRAVDIABE, AGRAVDOENC, 
         AGRAVDROGA, AGRAVTABAC,AGRAVOUTRA, DT_INIC_TR, DT_ENCERRA,  
         DT_NOTI_AT, NU_COMU_EX, BACILOSC_E, BACILOSC_1, BACILOSC_2, 
         BACILOSC_3, BACILOSC_4, BACILOSC_5, BACILOSC_6,
         BAC_APOS_6, TRATSUP_AT, SITUA_ENCE)

#### Limpeza do banco de dados (tabulação das variáveis)

## corrigir variável raça para descritivos
tuber_proc <- tuber_proc %>% 
  mutate(CS_RACA =
           case_when(CS_RACA == "1" ~ "Branca",
                     CS_RACA == "2" ~ "Preta",
                     CS_RACA == "3" ~ "Outras",
                     CS_RACA == "4" ~ "Parda",
                     CS_RACA == "5" ~ "Outras",
                     CS_RACA == "6" ~ "Ignorado",
                     CS_RACA == "9" ~ "Ignorado",
                     FALSE ~ NA))


table(tuber_proc$CS_RACA, useNA = "always")

## Corrigir variável escolaridade
tuber_proc <- tuber_proc %>% 
  mutate(CS_ESCOL_N =
           case_when((CS_ESCOL_N == "0")|(CS_ESCOL_N == " 0")|(CS_ESCOL_N == "00") ~ "Analfabeto",
                     (CS_ESCOL_N == "1")|(CS_ESCOL_N == " 1")|(CS_ESCOL_N == "01") ~ "1 a 8 anos",
                     (CS_ESCOL_N == "2")|(CS_ESCOL_N == " 2")|(CS_ESCOL_N == "02") ~ "1 a 8 anos",
                     (CS_ESCOL_N == "3")|(CS_ESCOL_N == " 3")|(CS_ESCOL_N == "03") ~ "1 a 8 anos",
                     (CS_ESCOL_N == "4")|(CS_ESCOL_N == " 4")|(CS_ESCOL_N == "04") ~ "1 a 8 anos",
                     (CS_ESCOL_N == "5")|(CS_ESCOL_N == " 5")|(CS_ESCOL_N == "05") ~ "> 8 anos",
                     (CS_ESCOL_N == "6")|(CS_ESCOL_N == " 6")|(CS_ESCOL_N == "06") ~ "> 8 anos",
                     (CS_ESCOL_N == "7")|(CS_ESCOL_N == " 7")|(CS_ESCOL_N == "07") ~ "> 8 anos",
                     (CS_ESCOL_N == "8")|(CS_ESCOL_N == " 8")|(CS_ESCOL_N == "08") ~ "> 8 anos",
                     (CS_ESCOL_N == "9")|(CS_ESCOL_N == " 9")|(CS_ESCOL_N == "09") ~ "Ignorado",
                     (CS_ESCOL_N == "10") ~ "Não se aplica",
                     FALSE ~ NA))


## Corrigir variável beneficiário do governo
tuber_proc <- tuber_proc %>% 
  mutate(BENEF_GOV =
           case_when(BENEF_GOV == "1" ~ "Sim",
                     BENEF_GOV == "2" ~ "Não",
                     BENEF_GOV == "9" ~ "Ignorado",
                     FALSE ~ NA))


## Corrigir todos os agravaos
tuber_proc <- tuber_proc %>% 
  mutate_at(vars(contains("POP")),
            funs(case_when(. == "1" ~ "Sim",
                           . == "2" ~ "Não",
                           . == "9" ~ "Ignorado",
                           FALSE ~ NA)))


## Corrigir variável forma clinica
tuber_proc <- tuber_proc %>% 
  mutate(FORMA =
           case_when(FORMA == "1" ~ "Pulmonar",
                     FORMA == "2" ~ "Extrapulmonar",
                     FORMA == "3" ~ "Pulmonar + Extrapulmonar",
                     FALSE ~ NA))


## Corrigir variável extrapulmonar
tuber_proc <- tuber_proc %>% 
  mutate(EXTRAPU1_N =
           case_when(EXTRAPU1_N == "1" ~ "Pleural",
                     EXTRAPU1_N == "2" ~ "Gang. Perif.",
                     EXTRAPU1_N == "3" ~ "Geniturinária",
                     EXTRAPU1_N == "4" ~ "Óssea",
                     EXTRAPU1_N == "5" ~ "Ocular",
                     EXTRAPU1_N == "6" ~ "Miliar",
                     EXTRAPU1_N == "7" ~ "Meningoencefálico",
                     EXTRAPU1_N == "8" ~ "Cutânea",
                     EXTRAPU1_N == "9" ~ "Laringea",
                     EXTRAPU1_N == "10" ~ "Outra",
                     FALSE ~ NA))


## Corrigir variável tipo de entrada
tuber_proc <- tuber_proc %>% 
  mutate(TRATAMENTO =
           case_when(TRATAMENTO == "1" ~ "Caso Novo",
                     TRATAMENTO == "2" ~ "Recidiva",
                     TRATAMENTO == "3" ~ "Reingresso após Abandono",
                     TRATAMENTO == "4" ~ "Não Sabe",
                     TRATAMENTO == "5" ~ "Transferência",
                     TRATAMENTO == "6" ~ "Pós-óbito",
                     FALSE ~ NA))

table(tuber_proc$TRATAMENTO, useNA = "always")

## Corrigir variável RX torax
tuber_proc <- tuber_proc %>% 
  mutate(RAIOX_TORA =
           case_when(RAIOX_TORA == "1" ~ "Suspeito",
                     RAIOX_TORA == "2" ~ "Normal",
                     RAIOX_TORA == "3" ~ "Outra patologia",
                     RAIOX_TORA == "4" ~ "Não realizado",
                     FALSE ~ NA))

## Corrigir variável HIV
tuber_proc <- tuber_proc %>% 
  mutate(HIV =
           case_when(HIV == "1" ~ "Positivo",
                     HIV == "2" ~ "Negativo",
                     HIV == "3" ~ "Em andamento",
                     HIV == "4" ~ "Não realizado",
                     FALSE ~ NA))

## Corrigirvariável antirretroviral
tuber_proc <- tuber_proc %>% 
  mutate(ANT_RETRO =
           case_when(ANT_RETRO == "1" ~ "Sim",
                     ANT_RETRO == "2" ~ "Não",
                     ANT_RETRO == "9" ~ "Ignorado",
                     FALSE ~ NA))

## Corrigindo variável TDO
tuber_proc <- tuber_proc %>% 
  mutate(TRATSUP_AT =
           case_when(TRATSUP_AT == "1" ~ "Sim",
                     TRATSUP_AT == "2" ~ "Não",
                     TRATSUP_AT == "9" ~ "Ignorado",
                     FALSE ~ NA))
table(tuber_proc$TRATSUP_AT)


## Corrigir todos os agravaos
tuber_proc <- tuber_proc %>% 
  mutate_at(vars(contains("AGRAV")),
            funs(case_when(. == "1" ~ "Sim",
                            . == "2" ~ "Não",
                            . == "9" ~ "Ignorado",
                            FALSE ~ NA)))

## Corrigir todos exames baciloscopia
tuber_proc <- tuber_proc %>% 
  mutate_at(vars(contains("BACILOSC")),
            funs(case_when(. == "1" ~ "Positiva",
                           . == "2" ~ "Negativa",
                           . == "3" ~ "Não Realizada",
                           . == "4" ~ "Não se Aplica",
                           FALSE ~ NA)))

## Corrigir todos os encerramento
tuber_proc <- tuber_proc %>% 
  mutate(SITUA_ENCE =
           case_when((SITUA_ENCE == "1")|(SITUA_ENCE == " 1")|(SITUA_ENCE == "01") ~ "Cura",
                     (SITUA_ENCE == "2")|(SITUA_ENCE == " 2")|(SITUA_ENCE == "02") ~ "Abandono",
                     (SITUA_ENCE == "3")|(SITUA_ENCE == " 3")|(SITUA_ENCE == "03") ~ "Óbito por TB",
                     (SITUA_ENCE == "4")|(SITUA_ENCE == " 4")|(SITUA_ENCE == "04") ~ "Óbito por
outras causas",
                     (SITUA_ENCE == "5")|(SITUA_ENCE == " 5")|(SITUA_ENCE == "05") ~ "Transferência",
                     (SITUA_ENCE == "6")|(SITUA_ENCE == " 6")|(SITUA_ENCE == "06") ~ "Mudança de Diagnóstico",
                     (SITUA_ENCE == "7")|(SITUA_ENCE == " 7")|(SITUA_ENCE == "07") ~ "TB-DR",
                     (SITUA_ENCE == "8")|(SITUA_ENCE == " 8")|(SITUA_ENCE == "08") ~ "Mudança de
Esquema",
                     (SITUA_ENCE == "9")|(SITUA_ENCE == " 9")|(SITUA_ENCE == "09") ~ "Falência",
                     (SITUA_ENCE == "10") ~ "Abandono Primário",
                     FALSE ~ NA))


## Corrigir idade
tuber_proc$NU_IDADE_N<-as.numeric(str_sub(as.character(tuber_proc$NU_IDADE_N),
                                        start = 3L, end = -1L))


## Remover anos de notificação abaixo de 2015
tuber_proc <- tuber_proc %>% filter(NU_ANO > 2014 & NU_ANO < 2024)           

table(tuber_proc$NU_ANO, useNA = "always")

#### Salvar banco

write.csv(tuber_proc, "data/banco_tuber_proc.csv")

nrow(tuber_proc)


 
################################################################################
#                                                                              #
#                              Aplicando os filtros                            #
#                                                                              #
################################################################################

tuber_proc <- read.csv("data/banco_tuber_proc.csv")

str(tuber_proc)

table(tuber_proc$BENEF_GOV, useNA = "always")

glimpse(tuber_proc)

### Seleção dos grupos que iremos utilizar (Aplicar filtros)


#### Banco para descritivo (POP Geral)  (3929 individuos)
desc_pop_geral <- tuber_proc %>%
  filter(ID_MUNIC_A == "355030") %>%     ### Municipio SP
  filter(POP_RUA == "Não")       %>%      ### Não PSR
  filter(POP_LIBER == "Não")     %>%      ### Não PPL
  filter(POP_SAUDE == "Não")     %>%       ### não Profissional de saúde
  filter(POP_IMIG == "Não")      %>%      ### não Imigrante
  filter(HIV == "Positivo")      %>%      ### HIV Positivo
  filter(NU_IDADE_N > 17)        %>%    ### Maior que 17 anos
  filter((SITUA_ENCE %in% c("Cura", "Abandono", "Abandono Primário", "Óbito por TB", 
                            "TB-DR", "Falência")))

nrow(desc_pop_geral)

table(desc_pop_geral$BENEF_GOV, useNA = "always")  

write.csv(desc_pop_geral[,-1], "data/desc_pop_geral.csv")


#### Banco para descritivo (POP RUA)  (958 individuos)
desc_pop_rua <- tuber_proc %>%
  filter(ID_MUNIC_A == "355030") %>%    ### Municipio sp
  filter(POP_RUA == "Sim")       %>%     ### Sim pop rua
  filter(POP_LIBER == "Não")     %>%      ### Não pop privadada de liberdade
  filter(POP_SAUDE == "Não")     %>%      ### não pop proficional de saúde
  filter(POP_IMIG == "Não")      %>%     ### não pop imigrante
  filter(HIV == "Positivo")      %>%     ### HIV Positivo
  filter(NU_IDADE_N > 17)        %>%   ### Maiores do que 17 anos
  filter((SITUA_ENCE %in% c("Cura", "Abandono", "Abandono Primário", "Óbito por TB", 
                            "TB-DR", "Falência")))

nrow(desc_pop_rua)

table(desc_pop_rua$BENEF_GOV, useNA = "always")  

write.csv(desc_pop_rua[,-1], "data/desc_pop_rua.csv")


#### Banco para associação (POP Geral)  (3929 individuos)
asso_pop_geral <- tuber_proc %>%
  filter(ID_MUNIC_A == "355030") %>%     ### Municipio sp
  filter(POP_RUA == "Não")       %>%      ### Não pop rua
  filter(POP_LIBER == "Não")     %>%      ### Não pop privadada de liberdade
  filter(POP_SAUDE == "Não")     %>%      ### não pop profissional de saúde
  filter(POP_IMIG == "Não")      %>%      ### não pop imigrante
  filter(HIV == "Positivo")      %>%      ### HIV Positivo
  filter(NU_IDADE_N > 17)        %>%     ### Maiores do que 17 anos
  filter((SITUA_ENCE %in% c("Cura", "Abandono", "Abandono Primário", "Óbito por TB", 
                            "TB-DR", "Falência"))) %>% 
  mutate(SITUA_ENCE = case_when((SITUA_ENCE %in% "Cura") ~ "Favorável",
                                TRUE ~ "Desfavorável")) %>% 
  mutate(CS_RACA =
           case_when(CS_RACA == "Branca" ~ "Branca",
                     CS_RACA == "Preta" ~ "Não branca",
                     CS_RACA == "Outras" ~ "Não branca",
                     CS_RACA == "Parda" ~ "Não branca",
                     CS_RACA == "Outras" ~ "Não branca",
                     CS_RACA == "Ignorado" ~ "Ignorado",
                     CS_RACA == "Ignorado" ~ "Ignorado",
                     FALSE ~ NA))

nrow(asso_pop_geral)

table(asso_pop_geral$CS_RACA)

write.csv(asso_pop_geral[,-1], "data/asso_pop_geral.csv")


#### Banco para associação (POP RUA)  (958 individuos)
asso_pop_rua <- tuber_proc %>%
  filter(ID_MUNIC_A == "355030") %>%   ### Municipio sp
  filter(POP_RUA == "Sim")       %>%    ### Sim pop rua
  filter(POP_LIBER == "Não")     %>%     ### Não pop privadada de liberdade
  filter(POP_SAUDE == "Não")     %>%     ### não pop proficional de saúde
  filter(POP_IMIG == "Não")      %>%      ### não pop imigrante
  filter(HIV == "Positivo")      %>%      ### HIV Positivo
  filter(NU_IDADE_N > 17)        %>%     ### Maiores do que 17 anos
  filter((SITUA_ENCE %in% c("Cura", "Abandono", "Abandono Primário", "Óbito por TB", 
                            "TB-DR", "Falência"))) %>% 
  mutate(SITUA_ENCE = case_when((SITUA_ENCE %in% "Cura") ~ "Favorável",
                                TRUE ~ "Desfavorável")) %>% 
  mutate(CS_RACA =
           case_when(CS_RACA == "Branca" ~ "Branca",
                     CS_RACA == "Preta" ~ "Não branca",
                     CS_RACA == "Outras" ~ "Não branca",
                     CS_RACA == "Parda" ~ "Não branca",
                     CS_RACA == "Outras" ~ "Não branca",
                     CS_RACA == "Ignorado" ~ "Ignorado",
                     CS_RACA == "Ignorado" ~ "Ignorado",
                     FALSE ~ NA))

nrow(asso_pop_rua)

table(asso_pop_rua$CS_RACA)

write.csv(asso_pop_rua[,-1], "data/asso_pop_rua.csv")



