# PLEASE!! use Rstudio PROJECT feature. The code looks for the `.Rproj` file in 
# the project root directory in order to setup relative paths. 

# This file estimates the Regression Discontinuity Model for men aged 23-26.

# This file uses final.csv dataset. 

# This file does not produce the final version of tables and graphs.


library(tidyverse) 
library(rio)
library(dplyr)
library(here)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(Hmisc) 
library(magrittr)
library(dplyr)
library(rddtools)
library(rdrobust)
library(stargazer)

here::here() 

options(scipen=999) 

data <- read.csv(here("data", "clean","final.csv")) 
data <- data %>% filter(year %in% c(2016:2019)) 
data <- data %>% filter(female!=1) 
data <- data %>% mutate(threshold = ifelse(age>24,0,1)) 
#
dt17 <- data %>% filter(year=="2017") %>% filter(age %in% c(23:26)) 
dt18 <- data %>% filter(year=="2018") %>% filter(age %in% c(23:26)) 
dt19 <- data %>% filter(year=="2019") %>% filter(age %in% c(23:26)) 
################################################################################
# RD Estimation by year for 23-26 aged males - Bandwidth Size = 2 
# RD Estimation 
# Linear RD Estimation (No controls)
# 2017
a1 <- lm(forww ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), dt17)  
b1 <- lm(lrhwage ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), subset(dt17, forww==1 & x==1))  
c1 <- lm(infww ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), dt17)  
d1 <- lm(lrhwage ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), subset(dt17, infww==1 & x==1))  

stargazer(a1,b1,c1,d1, type = "text", digits = 3, keep = "threshold", keep.stat = "n")

# 2018
a2 <- lm(forww ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), dt18)
b2 <- lm(lrhwage ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), subset(dt18, forww==1 & x==1)) 
c2 <- lm(infww ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), dt18)
d2 <- lm(lrhwage ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), subset(dt18, infww==1 & x==1)) 

stargazer(a2,b2,c2,d2, type = "text", digits = 3, keep = "threshold", keep.stat = "n") 

# 2019
a3 <- lm(forww ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), dt19)
b3 <- lm(lrhwage ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), subset(dt19, forww==1 & x==1)) 
c3 <- lm(infww ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), dt19)
d3 <- lm(lrhwage ~ I(age-25) + threshold + I(age-25):threshold + as.factor(region), subset(dt19, infww==1 & x==1)) 

stargazer(a3,b3,c3,d3, type = "text", digits = 3, keep = "threshold", keep.stat = "n") 

#####
# RD Linear Model Graphics for 2018  
dt18_2128 <- data %>% filter(age %in% c(21:28)) %>% filter(year=="2018") 
# Formal Wage Worker 
rdplot(dt18_2128$forww, dt18_2128$age, c=25, p=1, y.lab="Kayıtlı Ücretli Çalışan", x.lab="Yaş", title = "", y.lim = c(0.3,0.65), 
       col.dots = "black", col.lines = "blue", ci=95)
# Real Wage in Formal Sector 
dt18_2128_frw <- dt18_2128 %>% filter(forww==1 & x==1)
rdplot(dt18_2128_frw$lrhwage, dt18_2128_frw$age, c=25, p=1, y.lab="Kayıtlı S. Reel Ücret", x.lab="Yaş", title = "", y.lim = c(3.5,4.1),  
       col.dots = "black", col.lines = "blue", ci=95)
# Formal Wage Worker 
rdplot(dt18_2128$infww, dt18_2128$age, c=25, p=1, y.lab="Kayıtsız Ücretli Çalışan", x.lab="Yaş", title = "", y.lim = c(0.05,0.14), 
       col.dots = "black", col.lines = "blue", ci=95)
# Real Wage in Formal Sector 
dt18_2128_irw <- dt18_2128 %>% filter(infww==1 & x==1)
rdplot(dt18_2128_irw$lrhwage, dt18_2128_irw$age, c=25, p=1, y.lab="Kayıtsız S. Reel Ücret", x.lab="Yaş", title = "", y.lim = c(3.3,3.7), 
       col.dots = "black", col.lines = "blue", ci=95)
#####
# RD Quadratic Model Graphics for 2018  
# Formal Wage Worker 
rdplot(dt18_2128$forww, dt18_2128$age, c=25, p=2, y.lab="Kayıtlı Ücretli Çalışan", x.lab="Yaş", title = "", y.lim = c(0.3,0.65), 
       col.dots = "black", col.lines = "blue", ci=95)
# Real Wage in Formal Sector 
rdplot(dt18_2128_frw$lrhwage, dt18_2128_frw$age, c=25, p=2, y.lab="Kayıtlı S. Reel Ücret", x.lab="Yaş", title = "", y.lim = c(3.5,4.1),  
       col.dots = "black", col.lines = "blue", ci=95)
# Formal Wage Worker 
rdplot(dt18_2128$infww, dt18_2128$age, c=25, p=2, y.lab="Kayıtsız Ücretli Çalışan", x.lab="Yaş", title = "", y.lim = c(0.05,0.14), 
       col.dots = "black", col.lines = "blue", ci=95)
# Real Wage in Formal Sector 
rdplot(dt18_2128_irw$lrhwage, dt18_2128_irw$age, c=25, p=2, y.lab="Kayıtsız S. Reel Ücret", x.lab="Yaş", title = "", y.lim = c(3.3,3.7), 
       col.dots = "black", col.lines = "blue", ci=95)
#####

