
############################################################################
#                                                                          #
#   DOWNLOAD DATAFRAME, DATA CLEANING AND CUTTOFF ON POPULATION OF STUDY   #
#                                                                          #
############################################################################


###   PACKAGES   ###

library("read.dbc")
library("tidyverse")


#####################################################
#         DOWNLOAD AND OPEN THE DATABASE            #
#    SET THE SINAN DATABASE LINK (FILE TRANSFER)    #
#####################################################


### Tuberculosis database Brazil year 2015
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/TUBEBR15.dbc"
download.file(url, destfile = "data/TUBEBR15.dbc", mode = "wb")
tuber_15 <- read.dbc("data/TUBEBR15.dbc")

### Tuberculosis database Brazil year 2016
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/TUBEBR16.dbc"
download.file(url, destfile = "data/TUBEBR16.dbc", mode = "wb")
tuber_16 <- read.dbc("data/TUBEBR16.dbc")

### Tuberculosis database Brazil year 2017
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/TUBEBR17.dbc"
download.file(url, destfile = "data/TUBEBR17.dbc", mode = "wb")
tuber_17 <- read.dbc("data/TUBEBR17.dbc")

### Tuberculosis database Brazil year 2018
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/TUBEBR18.dbc"
download.file(url, destfile = "data/TUBEBR18.dbc", mode = "wb")
tuber_18 <- read.dbc("data/TUBEBR18.dbc")

### Tuberculosis database Brazil year 2019
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR19.dbc"
download.file(url, destfile = "data/TUBEBR19.dbc", mode = "wb")
tuber_19 <- read.dbc("data/TUBEBR19.dbc")

### Tuberculosis database Brazil year 2020
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR20.dbc"
download.file(url, destfile = "data/TUBEBR20.dbc", mode = "wb")
tuber_20 <- read.dbc("data/TUBEBR20.dbc")

### Tuberculosis database Brazil year 2021
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR21.dbc"
download.file(url, destfile = "data/TUBEBR21.dbc", mode = "wb")
tuber_21 <- read.dbc("data/TUBEBR21.dbc")

### Tuberculosis database Brazil year 2022
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR22.dbc"
download.file(url, destfile = "data/TUBEBR22.dbc", mode = "wb")
tuber_22 <- read.dbc("data/TUBEBR22.dbc")

### Tuberculosis database Brazil year 2023
url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/TUBEBR23.dbc"
download.file(url, destfile = "data/TUBEBR23.dbc", mode = "wb")
tuber_23 <- read.dbc("data/TUBEBR23.dbc")



########       CHECK THE VARIABLES       ########

all.equal(colnames(tuber_15),colnames(tuber_16),
          colnames(tuber_17),colnames(tuber_18),
          colnames(tuber_19),colnames(tuber_20),
          colnames(tuber_21),colnames(tuber_22),
          colnames(tuber_23))


########       JOIN DATABASE ROWS       ########

tuber_15_23 <- rbind(tuber_15, tuber_16, tuber_17, tuber_18, tuber_19,
                     tuber_20, tuber_21, tuber_22, tuber_23)


########       REMOVE DUPLICATE NOTIFICATIONS       ########

tuber_15_23 <- tuber_15_23 %>% 
  distinct(DT_NOTIFIC, DT_INIC_TR, CS_SEXO, FORMA,
           SG_UF_NOT, ID_MUNICIP, TRATAMENTO, SITUA_ENCE,
           DT_ENCERRA, NU_ANO, .keep_all = TRUE)



#####################################################
#                                                   #
#         SELECTING AND CODING VARIABLES            #
#                                                   #
#####################################################

 
tuber_proc <- tuber_15_23 %>% 
  select(NU_ANO, ID_MUNIC_A,
         CS_SEXO, CS_RACA, CS_ESCOL_N, NU_IDADE_N, 
         POP_RUA, POP_LIBER, POP_SAUDE, POP_IMIG,
         FORMA, EXTRAPU1_N, TRATAMENTO,  
         RAIOX_TORA, HIV, ANT_RETRO, AGRAVAIDS, 
         AGRAVALCOO, AGRAVDIABE, AGRAVDOENC, 
         AGRAVDROGA, AGRAVTABAC,AGRAVOUTRA,
         BACILOSC_E, BACILOSC_1, BACILOSC_2, 
         BACILOSC_3, BACILOSC_4, BACILOSC_5, BACILOSC_6,
         CULTURA_ES, TEST_MOLEC, TEST_SENSI, TRATSUP_AT, SITUA_ENCE)



# VARIABLE: COLOR/RACE/ETHNICITY ###############################################

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


# VARIABLE: EDUCATION ##########################################################

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


# VARIABLE: POPUILATION ########################################################

tuber_proc <- tuber_proc %>% 
  mutate_at(vars(contains("POP")),
            funs(case_when(. == "1" ~ "Sim",
                           . == "2" ~ "Não",
                           . == "9" ~ "Ignorado",
                           FALSE ~ NA)))


# VARIABLE: TYPE OF TUBERCULOSIS  ##############################################

tuber_proc <- tuber_proc %>% 
  mutate(FORMA =
           case_when(FORMA == "1" ~ "Pulmonar",
                     FORMA == "2" ~ "Extrapulmonar",
                     FORMA == "3" ~ "Pulmonar + Extrapulmonar",
                     FALSE ~ NA))


# VARIABLE: EXTRAPULMONARY  ####################################################

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

# VARIABLE: TYPE OF CASE  ######################################################

tuber_proc <- tuber_proc %>% 
  mutate(TRATAMENTO =
           case_when(TRATAMENTO == "1" ~ "Caso Novo",
                     TRATAMENTO == "2" ~ "Recidiva",
                     TRATAMENTO == "3" ~ "Reingresso após Abandono",
                     TRATAMENTO == "4" ~ "Não Sabe",
                     TRATAMENTO == "5" ~ "Transferência",
                     TRATAMENTO == "6" ~ "Pós-óbito",
                     FALSE ~ NA))


# VARIABLE: CHEST X-RAY  ######################################################

tuber_proc <- tuber_proc %>% 
  mutate(RAIOX_TORA =
           case_when(RAIOX_TORA == "1" ~ "Suspeito",
                     RAIOX_TORA == "2" ~ "Normal",
                     RAIOX_TORA == "3" ~ "Outra patologia",
                     RAIOX_TORA == "4" ~ "Não realizado",
                     FALSE ~ NA))


# VARIABLE: HIV  ###############################################################

tuber_proc <- tuber_proc %>% 
  mutate(HIV =
           case_when(HIV == "1" ~ "Positivo",
                     HIV == "2" ~ "Negativo",
                     HIV == "3" ~ "Em andamento",
                     HIV == "4" ~ "Não realizado",
                     FALSE ~ NA))


# VARIABLE: ANTIRETROVIRAL THERAPY  ############################################

tuber_proc <- tuber_proc %>% 
  mutate(ANT_RETRO =
           case_when(ANT_RETRO == "1" ~ "Sim",
                     ANT_RETRO == "2" ~ "Não",
                     ANT_RETRO == "9" ~ "Ignorado",
                     FALSE ~ NA))


# VARIABLE: DIRECTLY OBSERVED TREATMENT  #######################################

tuber_proc <- tuber_proc %>% 
  mutate(TRATSUP_AT =
           case_when(TRATSUP_AT == "1" ~ "Sim",
                     TRATSUP_AT == "2" ~ "Não",
                     TRATSUP_AT == "9" ~ "Ignorado",
                     FALSE ~ NA))


# VARIABLES: DIABETES, MENTAL DISORDERS, ALCOHOL, TOBACCO AND DRUG #############

tuber_proc <- tuber_proc %>% 
  mutate_at(vars(contains("AGRAV")),
            funs(case_when(. == "1" ~ "Sim",
                           . == "2" ~ "Não",
                           . == "9" ~ "Ignorado",
                           FALSE ~ NA)))


# VARIABLE: SPUTUM SMEAR MICROSCOPY  ###########################################
 
tuber_proc <- tuber_proc %>% 
  mutate_at(vars(contains("BACILOSC")),
            funs(case_when(. == "1" ~ "Positiva",
                           . == "2" ~ "Negativa",
                           . == "3" ~ "Não Realizada",
                           . == "4" ~ "Não se Aplica",
                           FALSE ~ NA)))


# VARIABLE: SPUTUM CULTURE  ####################################################
 
tuber_proc <- tuber_proc %>% 
  mutate(CULTURA_ES =
           case_when(CULTURA_ES == "1" ~ "Positiva",
                     CULTURA_ES == "2" ~ "Negativa",
                     CULTURA_ES == "3" ~ "Em andamento",
                     CULTURA_ES == "4" ~ "Não Realizada",
                     FALSE ~ NA))


# VARIABLE: RAPID MOLECULAR TEST  ##############################################

tuber_proc <- tuber_proc %>% 
  mutate(TEST_MOLEC =
           case_when(TEST_MOLEC == "1" ~ "Detectável sensível à Rifampicina",
                     TEST_MOLEC == "2" ~ "Detectável resistente à Rifampicina",
                     TEST_MOLEC == "3" ~ "Não detectável",
                     TEST_MOLEC == "4" ~ "Inconclusivo",
                     TEST_MOLEC == "5" ~ "Não realizado",
                     FALSE ~ NA))


# VARIABLE: TUBERCULOSIS DRUG SENSITIVITY TEST  ################################

tuber_proc <- tuber_proc %>% 
  mutate(TEST_SENSI =
           case_when(TEST_SENSI == "1" ~ "Resistente somente à Isoniazida",
                     TEST_SENSI == "2" ~ "Resistente somente à Rifampicina",
                     TEST_SENSI == "3" ~ "Resistente à Isoniazida e Rifampicina",
                     TEST_SENSI == "4" ~ "Resistente a outras drogas de 1ª linha",
                     TEST_SENSI == "5" ~ "Sensível",
                     TEST_SENSI == "6" ~ "Em andamento",
                     TEST_SENSI == "7" ~ "Não realizado",
                     FALSE ~ NA))


# VARIABLE: OUTCOME  ###########################################################

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


# VARIABLE: AGE  ###############################################################

tuber_proc$NU_IDADE_N<-as.numeric(str_sub(as.character(tuber_proc$NU_IDADE_N),
                                          start = 3L, end = -1L))

# VARIABLE: AGE GROUP  #########################################################

tuber_proc$faixa_etaria <- cut(tuber_proc$NU_IDADE_N, 
                               breaks = c(-Inf, 14, 29, 59, Inf), 
                               labels = c("< 15 anos", "15 a 29 anos", "30 a 59 anos",
                                          "> 59 anos"), 
                               right = TRUE)


# REMOVE NOTIFICATION YEARS BELOW 2015 AND ABOVE 2023  #########################

tuber_proc <- tuber_proc %>% filter(NU_ANO > 2014 & NU_ANO < 2024)  



#####################################################
#                                                   #
#                APPLYING FILTERS                   #
#                                                   #
#####################################################


######   DATABASE FOR DESCRIPTIVE ANALYSIS   ######

################ GENERAL POPULATION   ### FILTERS FOR DESCRIPTIVE ANALYSIS
desc_pop_geral_1 <- tuber_proc                  %>%
        filter(ID_MUNIC_A == "355030")          %>%    
        filter(POP_RUA == "Não")                %>%     
        filter(POP_LIBER == "Não")              %>%      
        filter(POP_SAUDE == "Não")              %>%      
        filter(HIV == "Positivo")               %>%     
        filter(faixa_etaria %in% c("15 a 29 anos", "30 a 59 anos", "> 59 anos"))   

################ HOMELESS POPULATION  ### FILTERS FOR DESCRIPTIVE ANALYSIS
desc_pop_rua_1 <- tuber_proc                    %>%
        filter(ID_MUNIC_A == "355030")          %>%    
        filter(POP_RUA == "Sim")                %>%    
        filter(POP_LIBER == "Não")              %>%     
        filter(POP_SAUDE == "Não")              %>%      
        filter(HIV == "Positivo")               %>%     
        filter(faixa_etaria %in% c("15 a 29 anos", "30 a 59 anos", "> 59 anos"))   



######   DATABASE FOR INFERENTIAL ANALYSIS   ######

################ GENERAL POPULATION   ### FILTERS FOR INFERENTIAL ANALYSI
asso_pop_geral_1 <- tuber_proc                                                     %>%
              filter(ID_MUNIC_A == "355030")                                       %>%     
              filter(POP_RUA == "Não")                                             %>%      
              filter(POP_LIBER == "Não")                                           %>%      
              filter(POP_SAUDE == "Não")                                           %>%      
              filter(HIV == "Positivo")                                            %>%      
              filter(faixa_etaria %in% c("15 a 59 anos", "> 59 anos"))             %>%     
              filter((SITUA_ENCE %in% c("Cura", "Abandono", "Abandono Primário"))) %>% 
  mutate(SITUA_ENCE = case_when((SITUA_ENCE %in% "Cura") 
                                ~ "Favorável", TRUE ~ "Desfavorável"))             %>% 
  mutate(CS_RACA =
           case_when(CS_RACA == "Branca" ~ "Branca",
                     CS_RACA == "Preta" ~ "Não branca",
                     CS_RACA == "Outras" ~ "Não branca",
                     CS_RACA == "Parda" ~ "Não branca",
                     CS_RACA == "Outras" ~ "Não branca",
                     CS_RACA == "Ignorado" ~ "Ignorado",
                     CS_RACA == "Ignorado" ~ "Ignorado",
                     FALSE ~ NA))


asso_pop_geral_1 <- asso_pop_geral_1                                             %>%
  mutate(BACILOSC_1_num =
           case_when((BACILOSC_1 == "Positiva")| (BACILOSC_1 == "Negativa") ~ 1,
                     (BACILOSC_1 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_2_num =
           case_when((BACILOSC_2 == "Positiva")| (BACILOSC_2 == "Negativa") ~ 1,
                     (BACILOSC_2 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_3_num =
           case_when((BACILOSC_3 == "Positiva")| (BACILOSC_3 == "Negativa") ~ 1,
                     (BACILOSC_3 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_4_num =
           case_when((BACILOSC_4 == "Positiva")| (BACILOSC_4 == "Negativa") ~ 1,
                     (BACILOSC_4 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_5_num =
           case_when((BACILOSC_5 == "Positiva")| (BACILOSC_5 == "Negativa") ~ 1,
                     (BACILOSC_5 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_6_num =
           case_when((BACILOSC_6 == "Positiva")| (BACILOSC_6 == "Negativa") ~ 1,
                     (BACILOSC_6 == "Não Realizada") ~ 0,
                     FALSE ~ NA))

asso_pop_geral_1$BACILOSC_total <- asso_pop_geral_1$BACILOSC_1_num +
  asso_pop_geral_1$BACILOSC_2_num +
  asso_pop_geral_1$BACILOSC_3_num +
  asso_pop_geral_1$BACILOSC_4_num +
  asso_pop_geral_1$BACILOSC_5_num +
  asso_pop_geral_1$BACILOSC_6_num



################ HOMELESS POPULATION  ### FILTERS FOR INFERENTIAL ANALYSI

asso_pop_rua_1 <- tuber_proc                                                       %>%
              filter(ID_MUNIC_A == "355030")                                       %>% 
              filter(POP_RUA == "Sim")                                             %>%
              filter(POP_LIBER == "Não")                                           %>%
              filter(POP_SAUDE == "Não")                                           %>%
              filter(HIV == "Positivo")                                            %>% 
              filter(faixa_etaria %in% c("15 a 59 anos", "> 59 anos"))             %>% 
              filter((SITUA_ENCE %in% c("Cura", "Abandono", "Abandono Primário"))) %>% 
  mutate(SITUA_ENCE = case_when((SITUA_ENCE %in% "Cura") 
                                ~ "Favorável", TRUE ~ "Desfavorável"))             %>% 
  mutate(CS_RACA =
           case_when(CS_RACA == "Branca" ~ "Branca",
                     CS_RACA == "Preta" ~ "Não branca",
                     CS_RACA == "Outras" ~ "Não branca",
                     CS_RACA == "Parda" ~ "Não branca",
                     CS_RACA == "Outras" ~ "Não branca",
                     CS_RACA == "Ignorado" ~ "Ignorado",
                     CS_RACA == "Ignorado" ~ "Ignorado",
                     FALSE ~ NA))


asso_pop_rua_1 <- asso_pop_rua_1                                                 %>%
  mutate(BACILOSC_1_num =
           case_when((BACILOSC_1 == "Positiva")| (BACILOSC_1 == "Negativa") ~ 1,
                     (BACILOSC_1 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_2_num =
           case_when((BACILOSC_2 == "Positiva")| (BACILOSC_2 == "Negativa") ~ 1,
                     (BACILOSC_2 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_3_num =
           case_when((BACILOSC_3 == "Positiva")| (BACILOSC_3 == "Negativa") ~ 1,
                     (BACILOSC_3 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_4_num =
           case_when((BACILOSC_4 == "Positiva")| (BACILOSC_4 == "Negativa") ~ 1,
                     (BACILOSC_4 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_5_num =
           case_when((BACILOSC_5 == "Positiva")| (BACILOSC_5 == "Negativa") ~ 1,
                     (BACILOSC_5 == "Não Realizada") ~ 0,
                     FALSE ~ NA))                                                %>% 
  mutate(BACILOSC_6_num =
           case_when((BACILOSC_6 == "Positiva")| (BACILOSC_6 == "Negativa") ~ 1,
                     (BACILOSC_6 == "Não Realizada") ~ 0,
                     FALSE ~ NA))

asso_pop_rua_1$BACILOSC_total <- asso_pop_rua_1$BACILOSC_1_num +
  asso_pop_rua_1$BACILOSC_2_num +
  asso_pop_rua_1$BACILOSC_3_num +
  asso_pop_rua_1$BACILOSC_4_num +
  asso_pop_rua_1$BACILOSC_5_num +
  asso_pop_rua_1$BACILOSC_6_num

