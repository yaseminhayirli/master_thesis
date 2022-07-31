# PLEASE!! use Rstudio PROJECT feature. The code looks for the `.Rproj` file in 
# the project root directory in order to setup relative paths.

# This file uses the final.csv file.

# This file produces the output of Table 2 in the thesis.

# The codes that make up the final version of the table showing the descriptive 
# statistics results are not included.

#
library(tidyverse)
library(here)
library(survey)

here::here() 
options(scipen=999)

data <- read.csv(here("data", "clean","final.csv")) 
#
data <- data %>% filter(year %in% c(2016:2019))
data <- data %>% filter(age %in% c(18:28)) 
male <- data %>% filter(female==0) 
#
male <- male %>% mutate(group = case_when(male1824==1 ~ "male1824",
                                          male2528==1 ~ "male2528"), one=1)
#
malesvy <- svydesign(ids = ~1, weights = ~AGIRLIK_KATSAYISI, data = male) 
#

# Descriptive statistics for treatment group (18-24 Aged Male)
dt_tr_exp <- svyby(~educ1_tr+married, ~year, subset(malesvy, male1824==1), svymean)
dt_tr_dep1 <- svyby(~forww+infww, ~year, subset(malesvy, male1824==1), svymean)
dt_tr_dep2 <- svyby(~rhwage, ~year, subset(malesvy, male1824==1 & x==1 & formal==1), svymean)
dt_tr_dep3 <- svyby(~rhwage, ~year, subset(malesvy, male1824==1 & x==1 & formal==0), svymean)

dt_obs_tr1 <- svyby(~one, ~year, subset(malesvy, male1824==1), svytotal) 
dt_obs_trf <- svyby(~one, ~year, subset(malesvy, male1824==1 & x==1 & formal==1), svytotal) 
dt_obs_trif <- svyby(~one, ~year, subset(malesvy, male1824==1 & x==1 & formal==0), svytotal) 
################################################################################
# Descriptive statistics for control group (25-28 Aged Male)
dt_cn_exp <- svyby(~educ1_tr+married, ~year, subset(malesvy, male1824==0), svymean)
dt_cn_dep1 <- svyby(~forww+infww, ~year, subset(malesvy, male1824==0), svymean)
dt_cn_dep2 <- svyby(~rhwage, ~year, subset(malesvy, male1824==0 & x==1 & formal==1), svymean)
dt_cn_dep3 <- svyby(~rhwage, ~year, subset(malesvy, male1824==0 & x==1 & formal==0), svymean)

dt_obs_cn1 <- svyby(~one, ~year, subset(malesvy, male1824==0), svytotal) 
dt_obs_cnf <- svyby(~one, ~year, subset(malesvy, male1824==0 & x==1 & formal==1), svytotal) 
dt_obs_cnif <- svyby(~one, ~year, subset(malesvy, male1824==0 & x==1 & formal==0), svytotal) 
################################################################################
# Unemployment Rate by year for treatment and control groups
tr_unemp <- svyby(~unemployed, ~year, subset(malesvy, male1824==1), svymean) %>% round(digits = 3)
cn_unemp <- svyby(~unemployed, ~year, subset(malesvy, male1824==0), svymean) %>% round(digits = 3)

################################################################################




