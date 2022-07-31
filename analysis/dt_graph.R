# PLEASE!! use Rstudio PROJECT feature. The code looks for the `.Rproj` file in 
# the project root directory in order to setup relative paths.

# This file uses the final.csv file. 

# This file produces a table showing the annual values of labor output for men 
# aged 18-24 and 25-28.

library(tidyverse)
library(here)
library(survey)

here::here() 
options(scipen=999)

data <- read.csv(here("data", "clean","final.csv")) 
var <- c("year","forww","infww","forwage","infwage","unemployed") 

data <- data %>% filter(age %in% c(18:28)) 
male <- data %>% filter(female==0) 

male <- male %>% mutate(group = case_when(male1824==1 ~ "male1824",
                                          male2528==1 ~ "male2528"))
#
malesvy <- svydesign(ids = ~1, weights = ~AGIRLIK_KATSAYISI, data = male) 
#

# Descriptive statistics for treatment group (18-24 Aged Male)
tr_dep1 <- svyby(~forww+infww, ~year, subset(malesvy, male2124==1), svymean) 
tr_dep2 <- svyby(~rhwage, ~year, subset(malesvy, male2124==1 & x==1 & formal==1), svymean) %>% rename(forwage = rhwage) 
tr_dep3 <- svyby(~rhwage, ~year, subset(malesvy, male2124==1 & x==1 & formal==0), svymean) %>% rename(infwage = rhwage) 
tr_unemp <- svyby(~unemployed, ~year, subset(malesvy, male2124==1), svymean) 

# Descriptive statistics for control group (25-28 Aged Male)
cn_dep1 <- svyby(~forww+infww, ~year, subset(malesvy, male2124==0), svymean)
cn_dep2 <- svyby(~rhwage, ~year, subset(malesvy, male2124==0 & x==1 & formal==1), svymean) %>% rename(forwage = rhwage)
cn_dep3 <- svyby(~rhwage, ~year, subset(malesvy, male2124==0 & x==1 & formal==0), svymean) %>% rename(infwage = rhwage)
cn_unemp <- svyby(~unemployed, ~year, subset(malesvy, male2124==0), svymean) 

################################################################################
# Create table for descriptive statistics 
# Treatment Group
male2124 <- cbind(tr_dep1,tr_dep2,tr_dep3,tr_unemp) %>% select(c(var)) %>% round(digits = 3) %>% mutate(group="male1824")
# Control Group
male2528 <- cbind(cn_dep1,cn_dep2,cn_dep3,cn_unemp) %>% select(c(var)) %>% round(digits = 3) %>% mutate(group="male2528")
# Combine datasets
dt_graph <- rbind(male2124,male2528) 
# Write to file
write.csv2(dt_graph, here("data","clean","dt_graph.csv"), row.names = F) 



