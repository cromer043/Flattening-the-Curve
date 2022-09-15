# WORKING DIRECTORY SHOULD BE 'data' FOLDER

library(tidyverse)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(htmlwidgets)
library(ggplot2)
library(stringr)
library(cowplot)
library(ggspatial)
library(maps)
library(ggrepel)
library(googleway)
library(weights)
library(tinytex)
library(data.table)
library(stargazer)
library(pander)
library(rlang)
library(plotly)
library(ggplot2)
library(forcats)
library(data.table)
library(knitr)
library(grid)
library(gridExtra)
library(questionr)
library(Hmisc)
library(lmtest)
library(sandwich)
library(stringr)
library(cowplot)
library(tinytex)
library(readxl)
library(devtools)
library(RColorBrewer)
library(kableExtra)
library(cellranger)
library(raster)
library(haven)
library(tidyverse)
library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(dplyr)

#Import data
statescr <- read.csv("acs_microdata.csv")
state_namecr <- read.csv("name.csv")
health_datacr<- read_xpt("LLCP2018.xpt")

#Get percent population 
state_racecr <- statescr %>% 
  mutate(RACE = factor(ifelse(HISPAN== 1 | HISPAN ==2 | HISPAN ==3 |HISPAN==4, "Hispanic",
                              ifelse(RACE ==1, "White",
                                     ifelse(RACE==2, "Black",
                                            ifelse(RACE==3, "American Indian or Alaska Native",
                                                   ifelse(RACE==4 | RACE==5, "Asian",
                                                          ifelse(RACE==6,"Pacific Islander",
                                                                 ifelse(RACE==7|RACE==8|RACE==9, "Other",NA)))))))),
         RACE = reorder.factor(RACE, new.order = c("White", "Black", "American Indian or Alaska Native", "Asian", "Pacific Islander", "Hispanic", "Other"))) %>%
  group_by(STATEFIP) %>% 
  mutate(
    white_percent = wpct(RACE, weight = PERWT)[1]*100,
    black_percent =  wpct(RACE, weight = PERWT)[2]*100,
    asian_percent =  wpct(RACE, weight = PERWT)[4]*100,
    indian_percent = wpct(RACE, weight = PERWT)[3]*100,
    pi_percent = wpct(RACE, weight = PERWT)[5]*100,
    hispanic_percent =  wpct(RACE, weight = PERWT)[6]*100
  ) %>% 
  dplyr::select(white_percent, black_percent, asian_percent,hispanic_percent, indian_percent, pi_percent, STATEFIP) %>% 
  distinct()

#Get percent senior
state_agecr <- statescr %>% 
  mutate(Senior = ifelse(as.numeric(as.character(AGE))>65,1,0),
         RACE = factor(ifelse(HISPAN== 1 | HISPAN ==2 | HISPAN ==3 |HISPAN==4, "Hispanic",
                              ifelse(RACE ==1, "White",
                                     ifelse(RACE==2, "Black",
                                            ifelse(RACE==3, "American Indian or Alaska Native",
                                                   ifelse(RACE==4 | RACE==5, "Asian",
                                                          ifelse(RACE==6,"Pacific Islander",
                                                                 ifelse(RACE==7|RACE==8|RACE==9, "Other",NA)))))))),
         RACE = reorder.factor(RACE, new.order = c("White", "Black", "American Indian or Alaska Native", "Asian", "Pacific Islander", "Hispanic", "Other")),
         RACE = reorder.factor(RACE, new.order = c("White", 
                                                   "Black", 
                                                   "American Indian or Alaska Native", 
                                                   "Asian", 
                                                   "Pacific Islander", 
                                                   "Hispanic", 
                                                   "Other")),
         Senior_white = ifelse(RACE=="White" & Senior == 1,1,0),
         Senior_black = ifelse(RACE=="Black" & Senior == 1,1,0),
         Senior_hispanic = ifelse(RACE=="Hispanic" & Senior == 1,1,0),
         Senior_indian = ifelse(RACE=="American Indian or Alaska Native" & Senior == 1,1,0),
         Senior_asian= ifelse(RACE=="Asian" & Senior == 1,1,0),
         Senior_pi = ifelse(RACE=="Pacific Islander" & Senior == 1,1,0)
  ) %>% 
  dplyr::filter(Senior == 1) %>% 
  group_by(STATEFIP) %>%
  mutate(
    percent_white_Senior = wpct(Senior_white, weight = PERWT)[2]*100,
    percent_black_Senior = wpct(Senior_black, weight = PERWT)[2]*100,
    percent_hispanic_Senior = wpct(Senior_hispanic, weight = PERWT)[2]*100,
    percent_asian_Senior = wpct(Senior_asian, weight = PERWT)[2]*100,
    percent_indian_Senior = wpct(Senior_indian, weight = PERWT)[2]*100,
    percent_pi_Senior = wpct(Senior_pi, weight = PERWT)[2]*100,
  ) %>% 
  dplyr::select(percent_white_Senior, 
                percent_black_Senior, 
                percent_hispanic_Senior,
                percent_asian_Senior,
                percent_indian_Senior,
                percent_pi_Senior) %>% 
  distinct() %>% 
  inner_join(.,inner_join(state_racecr,state_namecr))

#mutate new variables for each race and illness

health_data_workingcr <- health_datacr%>%
  mutate(RACE = ifelse(`_RACE` ==1, "White",
                       ifelse(`_RACE`==2,"Black",
                              ifelse(`_RACE`==3,"American Indian or Alaska Native",
                                     ifelse(`_RACE`==4, "Asian",
                                            ifelse(`_RACE`==5, "Pacific Islander",
                                                   ifelse(`_RACE`==6 | `_RACE`==7, "Other",
                                                          ifelse(`_RACE`==8, "Hispanic", NA))))))),
         white_diabetes = ifelse(RACE=="White" &DIABETE3 ==1,1,0),
         black_diabetes = ifelse(RACE=="Black" &DIABETE3 ==1,1,0),
         hispanic_diabetes = ifelse(RACE=="Hispanic" &DIABETE3 ==1,1,0),
         asian_diabetes = ifelse(RACE=="Asian" &DIABETE3 ==1,1,0),
         pi_diabetes = ifelse(RACE=="Pacific Islander" &DIABETE3 ==1,1,0),
         indian_diabetes = ifelse(RACE=="American Indian or Alaska Native" &DIABETE3 ==1,1,0),
         white_smoker = ifelse(RACE=="White" &`_RFSMOK3` ==2,1,0),
         black_smoker = ifelse(RACE=="Black" &`_RFSMOK3` ==2,1,0),
         hispanic_smoker = ifelse(RACE=="Hispanic" &`_RFSMOK3` ==2,1,0),
         asian_smoker = ifelse(RACE=="Asian" &`_RFSMOK3` ==2,1,0),
         pi_smoker = ifelse(RACE=="Pacific Islander" &`_RFSMOK3` ==2,1,0),
         indian_smoker = ifelse(RACE=="American Indian or Alaska Native" &`_RFSMOK3` ==2,1,0),
         white_copd = ifelse(RACE=="White" &`CHCCOPD1` ==1,1,0),
         black_copd = ifelse(RACE=="Black" &`CHCCOPD1` ==1,1,0),
         hispanic_copd = ifelse(RACE=="Hispanic" &`CHCCOPD1` ==1,1,0),
         asian_copd = ifelse(RACE=="Asian" &`CHCCOPD1` ==1,1,0),
         pi_copd = ifelse(RACE=="Pacific Islander" &`CHCCOPD1` ==1,1,0),
         indian_copd = ifelse(RACE=="American Indian or Alaska Native" &`CHCCOPD1` ==1,1,0),
         white_asthma = ifelse(RACE=="White" &`ASTHMA3` ==1,1,0),
         black_asthma = ifelse(RACE=="Black" &`ASTHMA3` ==1,1,0),
         hispanic_asthma = ifelse(RACE=="Hispanic" &`ASTHMA3` ==1,1,0),
         asian_asthma = ifelse(RACE=="Asian" &`ASTHMA3` ==1,1,0),
         pi_asthma = ifelse(RACE=="Pacific Islander" &`ASTHMA3` ==1,1,0),
         indian_asthma = ifelse(RACE=="American Indian or Alaska Native" &`ASTHMA3` ==1,1,0),
         white_obese = ifelse(RACE=="White" &`_BMI5CAT` ==4,1,0),
         black_obese = ifelse(RACE=="Black" &`_BMI5CAT` ==4,1,0),
         hispanic_obese = ifelse(RACE=="Hispanic" &`_BMI5CAT` ==4,1,0),
         asian_obese = ifelse(RACE=="Asian" &`_BMI5CAT` ==4,1,0),
         indian_obese = ifelse(RACE=="American Indian or Alaska Native" &`_BMI5CAT` ==4,1,0),
         pi_obese = ifelse(RACE=="Pacific Islander" &`_BMI5CAT` ==4,1,0),
         white_kidney = ifelse(RACE=="White" &`CHCKDNY1` ==1,1,0),
         black_kidney = ifelse(RACE=="Black" &`CHCKDNY1` ==1,1,0),
         hispanic_kidney = ifelse(RACE=="Hispanic" &`CHCKDNY1` ==1,1,0),
         asian_kidney = ifelse(RACE=="Asian" &`CHCKDNY1` ==1,1,0),
         indian_kidney = ifelse(RACE=="American Indian or Alaska Native" &`CHCKDNY1` ==1,1,0),
         pi_kidney = ifelse(RACE=="Pacific Islander" &`CHCKDNY1` ==1,1,0),
         STATEFIP = as.numeric(as.character(`_STATE`))
  ) 

#Get percentage of each race of each condition
diabetescr <- health_data_workingcr %>% 
  filter(DIABETE3==1) %>% 
  group_by(STATEFIP) %>% 
  mutate(
    white_diabetes_percent = wpct(white_diabetes, weight = `_LLCPWT`)[2]*100,
    black_diabetes_percent = wpct(black_diabetes, weight = `_LLCPWT`)[2]*100,
    hispanic_diabetes_percent = wpct(hispanic_diabetes, weight = `_LLCPWT`)[2]*100,
    asian_diabetes_percent = wpct(asian_diabetes, weight = `_LLCPWT`)[2]*100,
    pi_diabetes_percent = wpct(pi_diabetes, weight = `_LLCPWT`)[2]*100,
    indian_diabetes_percent = wpct(indian_diabetes, weight = `_LLCPWT`)[2]*100
  ) %>% 
  dplyr::select(white_diabetes_percent, black_diabetes_percent, hispanic_diabetes_percent, 
                asian_diabetes_percent, pi_diabetes_percent,indian_diabetes_percent) %>% 
  ungroup() %>% 
  distinct()

smokerscr <- health_data_workingcr %>% 
  filter(`_RFSMOK3`==2) %>% 
  group_by(STATEFIP) %>% 
  mutate(
    white_smoker_percent = wpct(white_smoker, weight = `_LLCPWT`)[2]*100,
    black_smoker_percent = wpct(black_smoker, weight = `_LLCPWT`)[2]*100,
    hispanic_smoker_percent = wpct(hispanic_smoker, weight = `_LLCPWT`)[2]*100,
    asian_smoker_percent = wpct(asian_smoker, weight = `_LLCPWT`)[2]*100,
    pi_smoker_percent = wpct(pi_smoker, weight = `_LLCPWT`)[2]*100,
    indian_smoker_percent = wpct(indian_smoker, weight = `_LLCPWT`)[2]*100
  ) %>% 
  dplyr::select(
    white_smoker_percent, black_smoker_percent,hispanic_smoker_percent, 
    asian_smoker_percent, pi_smoker_percent,indian_smoker_percent
  ) %>% 
  ungroup() %>% 
  distinct()
copdcr <- health_data_workingcr %>% 
  filter(CHCCOPD1==1) %>% 
  group_by(STATEFIP) %>% 
  mutate(
    white_copd_percent = wpct(white_copd, weight = `_LLCPWT`)[2]*100,
    black_copd_percent = wpct(black_copd, weight = `_LLCPWT`)[2]*100,
    hispanic_copd_percent = wpct(hispanic_copd, weight = `_LLCPWT`)[2]*100,
    asian_copd_percent = wpct(asian_copd, weight = `_LLCPWT`)[2]*100,
    pi_copd_percent = wpct(pi_copd, weight = `_LLCPWT`)[2]*100,
    indian_copd_percent = wpct(indian_copd, weight = `_LLCPWT`)[2]*100
  ) %>% 
  dplyr::select(
    white_copd_percent, 
    black_copd_percent, hispanic_copd_percent,asian_copd_percent, pi_copd_percent, 
    indian_copd_percent
  ) %>% 
  ungroup() %>% 
  distinct()
asthmacr <- health_data_workingcr %>% 
  filter(ASTHMA3 ==1) %>% 
  group_by(STATEFIP) %>% 
  mutate(
    white_asthma_percent = wpct(white_asthma, weight = `_LLCPWT`)[2]*100,
    black_asthma_percent = wpct(black_asthma, weight = `_LLCPWT`)[2]*100,
    hispanic_asthma_percent = wpct(hispanic_asthma, weight = `_LLCPWT`)[2]*100,
    asian_asthma_percent = wpct(asian_asthma, weight = `_LLCPWT`)[2]*100,
    pi_asthma_percent = wpct(pi_asthma, weight = `_LLCPWT`)[2]*100,
    indian_asthma_percent = wpct(indian_asthma, weight = `_LLCPWT`)[2]*100
  ) %>% 
  dplyr::select(white_asthma_percent,black_asthma_percent, 
                hispanic_asthma_percent, asian_asthma_percent, pi_asthma_percent, 
                indian_asthma_percent) %>% 
  ungroup() %>% 
  distinct()

obesecr <- health_data_workingcr %>% 
  filter(`_BMI5CAT`==4) %>% 
  group_by(STATEFIP) %>% 
  mutate(
    white_obese_percent = wpct(white_obese, weight = `_LLCPWT`)[2]*100,
    black_obese_percent = wpct(black_obese, weight = `_LLCPWT`)[2]*100,
    hispanic_obese_percent = wpct(hispanic_obese, weight = `_LLCPWT`)[2]*100,
    asian_obese_percent = wpct(asian_obese, weight = `_LLCPWT`)[2]*100,
    indian_obese_percent = wpct(indian_obese, weight = `_LLCPWT`)[2]*100,
    pi_obese_percent = wpct(pi_obese, weight = `_LLCPWT`)[2]*100
  ) %>% 
  dplyr::select(
    white_obese_percent ,
    black_obese_percent , hispanic_obese_percent,  asian_obese_percent ,
    indian_obese_percent,   pi_obese_percent   
  ) %>% 
  ungroup() %>% 
  distinct()

kidneycr <- health_data_workingcr %>% 
  filter(CHCKDNY1 ==1) %>% 
  group_by(STATEFIP) %>% 
  mutate(
    white_kidney_percent = wpct(white_kidney, weight = `_LLCPWT`)[2]*100,
    black_kidney_percent = wpct(black_kidney, weight = `_LLCPWT`)[2]*100,
    hispanic_kidney_percent = wpct(hispanic_kidney, weight = `_LLCPWT`)[2]*100,
    asian_kidney_percent = wpct(asian_kidney, weight = `_LLCPWT`)[2]*100,
    indian_kidney_percent = wpct(indian_kidney, weight = `_LLCPWT`)[2]*100,
    pi_kidney_percent = wpct(pi_kidney, weight = `_LLCPWT`)[2]*100
  ) %>% 
  dplyr::select( white_kidney_percent ,
                 black_kidney_percent , hispanic_kidney_percent, 
                 asian_kidney_percent ,  indian_kidney_percent , pi_kidney_percent 
  ) %>% 
  ungroup() %>% 
  distinct() 
#Get ratios and join datasets
health_data_workingcr <- inner_join(inner_join(asthmacr,copdcr),inner_join(diabetescr,inner_join(kidneycr,inner_join(obesecr,smokerscr))))     
datacr<- inner_join(state_agecr,health_data_workingcr, by = c("STATEFIP")) %>%
  ungroup() %>% 
  dplyr::select( -STATEFIP) %>% 
  mutate(
    white_diabetes_percent	=	white_diabetes_percent	/	white_percent	,
    black_diabetes_percent	=	black_diabetes_percent	/	black_percent	,
    hispanic_diabetes_percent	=	hispanic_diabetes_percent	/	hispanic_percent	,
    asian_diabetes_percent	=	asian_diabetes_percent	/	asian_percent	,
    pi_diabetes_percent	=	pi_diabetes_percent	/	pi_percent	,
    indian_diabetes_percent	=	indian_diabetes_percent	/	indian_percent	,
    white_smoker_percent	=	white_smoker_percent	/	white_percent	,
    black_smoker_percent	=	black_smoker_percent	/	black_percent	,
    hispanic_smoker_percent	=	hispanic_smoker_percent	/	hispanic_percent	,
    asian_smoker_percent	=	asian_smoker_percent	/	asian_percent	,
    pi_smoker_percent	=	pi_smoker_percent	/	pi_percent	,
    indian_smoker_percent	=	indian_smoker_percent	/	indian_percent	,
    white_copd_percent	=	white_copd_percent	/	white_percent	,
    black_copd_percent	=	black_copd_percent	/	black_percent	,
    hispanic_copd_percent	=	hispanic_copd_percent	/	hispanic_percent	,
    asian_copd_percent	=	asian_copd_percent	/	asian_percent	,
    pi_copd_percent	=	pi_copd_percent	/	pi_percent	,
    indian_copd_percent	=	indian_copd_percent	/	indian_percent	,
    white_asthma_percent	=	white_asthma_percent	/	white_percent	,
    black_asthma_percent	=	black_asthma_percent	/	black_percent	,
    hispanic_asthma_percent	=	hispanic_asthma_percent	/	hispanic_percent	,
    asian_asthma_percent	=	asian_asthma_percent	/	asian_percent	,
    pi_asthma_percent	=	pi_asthma_percent	/	pi_percent	,
    indian_asthma_percent	=	indian_asthma_percent	/	indian_percent	,
    white_obese_percent	=	white_obese_percent	/	white_percent	,
    black_obese_percent	=	black_obese_percent	/	black_percent	,
    hispanic_obese_percent	=	hispanic_obese_percent	/	hispanic_percent	,
    asian_obese_percent	=	asian_obese_percent	/	asian_percent	,
    indian_obese_percent	=	indian_obese_percent	/	indian_percent	,
    pi_obese_percent	=	pi_obese_percent	/	pi_percent	,
    white_kidney_percent	=	white_kidney_percent	/	white_percent	,
    black_kidney_percent	=	black_kidney_percent	/	black_percent	,
    hispanic_kidney_percent	=	hispanic_kidney_percent	/	hispanic_percent	,
    asian_kidney_percent	=	asian_kidney_percent	/	asian_percent	,
    indian_kidney_percent	=	indian_kidney_percent	/	indian_percent	,
    pi_kidney_percent	=	pi_kidney_percent	/	pi_percent	,
    percent_white_Senior	=	percent_white_Senior	/	white_percent	,
    percent_black_Senior	=	percent_black_Senior	/	black_percent	,
    percent_hispanic_Senior	=	percent_hispanic_Senior	/	hispanic_percent	,
    percent_asian_Senior	=	percent_asian_Senior	/	asian_percent	,
    percent_indian_Senior	=	percent_indian_Senior	/	indian_percent	,
    percent_pi_Senior	=	percent_pi_Senior	/	pi_percent
  ) %>% 
  dplyr::select(-white_percent,
                -  hispanic_percent,
                - black_percent,
                -indian_percent,
                - pi_percent,
                - asian_percent,
                -State.Name) %>% 
  pivot_longer(.,-`STATE.`,names_to = "condition", values_to = "Ratio") %>% 
  mutate(Race = ifelse(str_detect(condition,"white")==T,"White",
                       ifelse(str_detect(condition,"black")==T,"Black",
                              ifelse(str_detect(condition,"hispanic")==T,"Hispanic",
                                     ifelse(str_detect(condition,"pi")==T,"Native Hawaian or Pacific Islander",
                                            ifelse(str_detect(condition,"indian")==T,"American Indian or Alaska Native",
                                                   ifelse(str_detect(condition,"asian")==T,"Asian",NA))))))) %>% 
  mutate(Condition = ifelse(str_detect(condition,"kidney")==T,"Kidney Disease",
                            ifelse(str_detect(condition,"diabetes")==T,"Diabetes",
                                   ifelse(str_detect(condition,"copd")==T,"COPD",
                                          ifelse(str_detect(condition,"obese")==T,"Obesity",
                                                 ifelse(str_detect(condition,"Senior")==T,"Senior",
                                                        ifelse(str_detect(condition,"smoker")==T,"Smoking",
                                                               ifelse(str_detect(condition,"asthma")==T,"Asthma",NA)))))))) %>% 
  dplyr::select(-condition) %>% 
  inner_join(state_namecr)

mergercr <- inner_join(state_racecr,state_namecr) %>% 
  ungroup() %>% 
  dplyr::select(-STATEFIP, -State.Name) %>% 
  pivot_longer(.,-`STATE.`,names_to = "condition", values_to = "Percent") %>% 
  mutate(Race = ifelse(str_detect(condition,"white")==T,"White",
                       ifelse(str_detect(condition,"black")==T,"Black",
                              ifelse(str_detect(condition,"hispanic")==T,"Hispanic",
                                     ifelse(str_detect(condition,"pi")==T,"Native Hawaian or Pacific Islander",
                                            ifelse(str_detect(condition,"indian")==T,"American Indian or Alaska Native",
                                                   ifelse(str_detect(condition,"asian")==T,"Asian",NA))))))) %>% 
  mutate(Overall = Percent) %>% 
  dplyr::select(-condition, -Percent)

data_2cr <- inner_join(state_agecr,health_data_workingcr, by = c("STATEFIP")) %>%
  ungroup() %>% 
  dplyr::select(-white_percent,
                -  hispanic_percent,
                - black_percent,
                -indian_percent,
                - pi_percent,
                - asian_percent,
                -STATEFIP, -State.Name) %>% 
  pivot_longer(.,-`STATE.`,names_to = "condition", values_to = "Percent") %>% 
  mutate(Race = ifelse(str_detect(condition,"white")==T,"White",
                       ifelse(str_detect(condition,"black")==T,"Black",
                              ifelse(str_detect(condition,"hispanic")==T,"Hispanic",
                                     ifelse(str_detect(condition,"pi")==T,"Native Hawaian or Pacific Islander",
                                            ifelse(str_detect(condition,"indian")==T,"American Indian or Alaska Native",
                                                   ifelse(str_detect(condition,"asian")==T,"Asian",NA))))))) %>% 
  mutate(Condition = ifelse(str_detect(condition,"kidney")==T,"Kidney Disease",
                            ifelse(str_detect(condition,"diabetes")==T,"Diabetes",
                                   ifelse(str_detect(condition,"copd")==T,"COPD",
                                          ifelse(str_detect(condition,"obese")==T,"Obesity",
                                                 ifelse(str_detect(condition,"Senior")==T,"Senior",
                                                        ifelse(str_detect(condition,"smoker")==T,"Smoking",
                                                               ifelse(str_detect(condition,"asthma")==T,"Asthma",NA)))))))) %>% 
  dplyr::select(-condition) %>% 
  inner_join(mergercr) %>%
  ungroup() %>% 
  inner_join(state_namecr)
write.csv(datacr, file = "datacr.csv")
write.csv(data_2cr, file = "data_2cr.csv")