# PLEASE!! use Rstudio PROJECT feature. The code looks for the `.Rproj` file in 
# the project root directory in order to setup relative paths.

# This file edits the HLFS dataset between 2014 and 2019 and produces 
# the final.csv and final.dta files.

# 
library(tidyverse)
library(rio)
library(here)
library(readxl)
#
# I use `here::` package to make the code portable.
# This sets the folder where .Rproj file resides as the project root folder
here::here() 
# 
#Variable List
var = c("REFERANS_YIL","BIRIMNO","CINSIYET","YAS","IBBS_1","IBBS_2",
        "OKUL_BITEN_K","OKUR_YAZAR","EGITIM_DEVAM_K","OKUL_DEVAM_K","KURS",
        "MEDENI_DURUM","ISTEKI_DURUM_K","CALISMA","NACE2_ESAS_K","OZEL_KAMU",  
        "CALISAN_SAYI_HH","ISCO08_ESAS_K","KAYITLILIK","ESAS_FIILI","EKIS_FIILI", "ESAS_HAFSAAT_GENEL",
        "GELIR_GECENAY_K","DURUM","IDO_NEDEN","AZCAL_NEDEN","AGIRLIK_KATSAYISI","ENF",
        "FAAL_DURUM_GECENYIL","NACE2_GECENYIL_K","ISTEKI_DURUM_GECENYIL")

# Read each year separately. 
hia14 <- read.csv2(here("data", "raw","hia2014.csv"), dec = ".") %>% mutate(ENF = 0.082 ) %>% subset(select=c(var))
#
hia15 <- read.csv2(here("data", "raw","hia2015.csv"), dec = ".") %>% mutate(ENF = 0.088 ) %>% subset(select=c(var))
#
hia16 <- read.csv2(here("data", "raw","hia2016.csv"), dec = ".") %>% mutate(ENF = 0.085 ) %>% subset(select=c(var))
#
hia17 <- read.csv2(here("data", "raw","hia2017.csv"), dec = ".") %>% mutate(ENF = 0.1192 ) %>% subset(select=c(var))
# 
hia18 <- read.csv2(here("data", "raw","hia2018.csv"), dec = ".") %>% mutate(ENF = 0.2030 ) %>% subset(select=c(var))
#
hia19 <- read.csv2(here("data", "raw","hia2019.csv"), dec = ".") %>% mutate(ENF = 0.1184 ) %>% subset(select=c(var))


# Bind all years
data <- rbind(hia14,hia15,hia16,hia17,hia18,hia19) 

data <- data %>% rename(year = REFERANS_YIL, age = YAS, educ = OKUL_BITEN_K, firmsize = CALISAN_SAYI_HH,
                        workhour = ESAS_FIILI, addwhour = EKIS_FIILI, income = GELIR_GECENAY_K, 
                        sector = NACE2_ESAS_K, occupation = ISCO08_ESAS_K)

# Exclude workers in agriculture
data <- data %>% mutate(agworker = ifelse(sector %in% c(1,2,3),1,0)) %>% subset(agworker != 1)

#Restrict the data by age 
data <- data %>% filter(age %in% c(18:55))

#Create the variables that will use in the analysis.
data <- data %>% mutate(female = ifelse(CINSIYET==2,1,0),
                        married = ifelse(MEDENI_DURUM==2,1,0), 
                        wageworker = ifelse(DURUM==1 & ISTEKI_DURUM_K==1,1,0),
                        employed = ifelse(DURUM==1,1,0),
                        unemployed = ifelse(DURUM==2,1,0),
                        notlabforce = ifelse(DURUM==3,1,0), 
                        formal = ifelse(DURUM==1 & KAYITLILIK==1,1,0),
                        informal = ifelse(DURUM==1 & KAYITLILIK==2,1,0),
                        privatesec = ifelse(OZEL_KAMU==1,1,0),
                        forww = ifelse(wageworker==1 & KAYITLILIK==1,1,0),
                        infww = ifelse(wageworker==1 & KAYITLILIK==2,1,0))

#
data <- data %>% mutate(male1824 = ifelse(female==0 & (age %in% c(18:24)),1,0),
                        male2528 = ifelse(female==0 & (age %in% c(25:28)),1,0),
                        target  = ifelse(female==1 | (female==0 & age<25),1,0),
                        male1820 = ifelse(female==0 & (age %in% c(18:20)),1,0),
                        male1920 = ifelse(female==0 & (age %in% c(19:20)),1,0),
                        male2122 = ifelse(female==0 & (age %in% c(21:22)),1,0),
                        male2124 = ifelse(female==0 & (age %in% c(21:24)),1,0),
                        male2324 = ifelse(female==0 & (age %in% c(23:24)),1,0),
                        male2526 = ifelse(female==0 & (age %in% c(25:26)),1,0),
                        male1921 = ifelse(female==0 & (age %in% c(19:21)),1,0),
                        male2224 = ifelse(female==0 & (age %in% c(22:24)),1,0),
                        male2527 = ifelse(female==0 & (age %in% c(25:27)),1,0),
                        post18 = ifelse(year %in% c(2018:2019),1,0)) 

data <- data %>% mutate(region = as.numeric(as.factor(IBBS_2))) 

#Create the logarithmic hourly wage variable 
data <- data %>% mutate(hwage = income/(workhour*4.2), lhwage = log(hwage), lhwage = ifelse(is.finite(lhwage),lhwage,NA)) 

#Create the logarithmic reel hourly wage variable 
data <- data %>% mutate(rwage = income/ENF, rhwage = rwage/(workhour*4.2), lrhwage = log(rhwage),lrhwage = ifelse(is.finite(lrhwage),lrhwage,NA))

#Define a variable for individuals whose income and work hours are not zero 
data <- data %>% mutate(x = ifelse(income>0 & workhour>0,1,0)) 
  
#Define again education groups 
data <- data %>% mutate(educ1 = case_when(educ==0 ~ 0,
                                          educ %in% c(1,2) ~ 1,
                                          educ %in% c(31,32) ~ 2,
                                          educ %in% c(4,5) ~ 3),
                        educ1_en = case_when(educ1==0 ~ "No School",
                                             educ1==1 ~ "Primary or Middle School",
                                             educ1==2 ~ "High School",
                                             educ1==3 ~ "College or University"),
                        educ1_tr = case_when(educ1==0 ~ "Bir Okul Bitirmeyen",
                                             educ1==1 ~ "İlkokul veya Ortaokul",
                                             educ1==2 ~ "Lise",
                                             educ1==3 ~ "Üniversite"))

# Write to file
export(data, here("data","clean","final.csv")) # Save csv 
export(data, here("data","clean","final.dta")) # Save dta
