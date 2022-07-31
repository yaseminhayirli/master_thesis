# PLEASE!! use Rstudio PROJECT feature. The code looks for the `.Rproj` file in 
# the project root directory in order to setup relative paths.

# This file uses the final.csv file.

# This file produces the output of Table 3 in the thesis.

# The codes that make up the final version of the table showing the descriptive 
# statistics results are not included.


library(tidyverse)
library(here)
library(survey)

here::here() 

data <- read.csv(here("data", "clean","final.csv")) 

data <- data %>% filter(year %in% c(2016:2019))
data <- data %>% filter(age %in% c(18:28)) 
male <- data %>% filter(female==0) 
#
malesvy <- svydesign(ids = ~1, weights = ~AGIRLIK_KATSAYISI, data = male) 
#
# Descriptive statistics for explanatory variables
data_exp <- svyby(~educ1_tr+as.factor(EGITIM_DEVAM_K)+married, ~group+post18, malesvy, svymean) 
# Descriptive statistics for dependent variables
data_dep <- svyby(~forww+infww+employed+unemployed+notlabforce+formal+informal, ~group+post18, malesvy, svymean) 


# Create a table for desc. statistics 
male_exp <- as.data.frame(t(data_exp), stringsAsFactors = F)
male_exp[] <- lapply(male_exp, type.convert, as.is = T)
male_exp <- male_exp[-1,]  


male_exp$male1824.0 <- as.numeric(as.character(male_exp$male1824.0))
male_exp$male1824.1 <- as.numeric(as.character(male_exp$male1824.1))
male_exp$male2528.0 <- as.numeric(as.character(male_exp$male2528.0))
male_exp$male2528.1 <- as.numeric(as.character(male_exp$male2528.1))

male_exp <- male_exp %>% mutate(diff_pre = male1824.0-male2528.0, diff_post = male1824.1-male2528.1)
male_exp <- male_exp %>% round(digits = 3)
male_exp <- male_exp %>% select(c("male1824.0","male1824.1","male2528.0","male2528.1","diff_pre","diff_post")) 
#







