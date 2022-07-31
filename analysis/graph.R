# PLEASE!! use Rstudio PROJECT feature. The code looks for the `.Rproj` file in 
# the project root directory in order to setup relative paths.

# This file uses the final.csv file. 

# This file generates graphs showing the change in intervention and control 
# groups over the years. 

# These graphs are given in Figure 5 in the thesis.

#
library(tidyverse)
library(rio)
library(here)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(survey)
library(Hmisc) 

here::here() 
options(scipen=999)

dt <- read.csv2(here("data","clean","dt_graph.csv")) 
dt <- dt %>% mutate(label = case_when(group=="male1824" & year==2019 ~ "18-24 Yaş Arası Erkek",
                                      group=="male2528" & year==2019 ~ "25-28 Yaş Arası Erkek",
                                      TRUE ~ NA_character_))

# Formal wage worker rate by year between treatment and control groups 
ggplot(dt, aes(x= as.factor(year))) + geom_line(aes(y = forww , group = as.factor(group), colour = as.factor(group)), size= 1.2) +
  geom_point(aes(y = forww, color = as.factor(group), size= 0.5)) + 
  theme_bw() + scale_color_economist()+
  geom_label_repel(aes(y = forww, label = label, color = group), point.padding = unit(8, "points"), na.rm = TRUE, size=8) +
  labs(title= "", x="", y="Kayıtlı İstihdam Oranı") +
  theme(plot.title = element_text(size=30,  colour= "black" ),
        axis.title.x = element_text(size=30, colour = "black"),    
        axis.title.y = element_text(size=30, colour = "black"),    
        axis.text.x = element_text(size=30, colour = "black"), 
        axis.text.y = element_text(size=30, colour = "black"), 
        strip.text.x = element_text(size = 30, colour = "black" ),
        strip.text.y = element_text(size = 30, colour = "black"),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        plot.caption = element_text(color = "black", size=16)) + theme(legend.position = "none") +
  labs(caption = "Kaynak: HİA")+ theme(plot.caption = element_text(hjust = 0))



# Informal wage worker rate by year between treatment and control groups 
ggplot(dt, aes(x= as.factor(year))) + geom_line(aes(y = infww , group = as.factor(group), colour = as.factor(group)), size= 1.2) +
  geom_point(aes(y = infww, color = as.factor(group), size= 0.5)) + 
  theme_bw() + scale_color_economist()+
  geom_label_repel(aes(y = infww, label = label, color = group), point.padding = unit(8, "points"), na.rm = TRUE, size=8) +
  labs(title= "", x="", y="Kayıtsız İstihdam Oranı") +
  theme(plot.title = element_text(size=30,  colour= "black" ),
        axis.title.x = element_text(size=30, colour = "black"),    
        axis.title.y = element_text(size=30, colour = "black"),    
        axis.text.x = element_text(size=30, colour = "black"), 
        axis.text.y = element_text(size=30, colour = "black"), 
        strip.text.x = element_text(size = 30, colour = "black" ),
        strip.text.y = element_text(size = 30, colour = "black"),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        plot.caption = element_text(color = "black", size = 16)) + theme(legend.position = "none") +
  labs(caption = "Kaynak: HİA")+ theme(plot.caption = element_text(hjust = 0))



# Hourly real wages in formal sector rate by year between treatment and control groups 
ggplot(dt, aes(x= as.factor(year))) + geom_line(aes(y = forwage , group = as.factor(group), colour = as.factor(group)), size= 1.2) +
  geom_point(aes(y = forwage, color = as.factor(group), size= 0.5)) + 
  theme_bw() + scale_color_economist()+
  geom_label_repel(aes(y = forwage, label = label, color = group), point.padding = unit(8, "points"), na.rm = TRUE, size=8) +
  labs(title= "", x="", y="Kayıtlı Sektör Saatlik Reel Ücret (TL)") +
  theme(plot.title = element_text(size=30,  colour= "black" ),
        axis.title.x = element_text(size=30, colour = "black"),    
        axis.title.y = element_text(size=30, colour = "black"),    
        axis.text.x = element_text(size=30, colour = "black"), 
        axis.text.y = element_text(size=30, colour = "black"), 
        strip.text.x = element_text(size = 30, colour = "black" ),
        strip.text.y = element_text(size = 30, colour = "black"),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        plot.caption = element_text(color = "black", size = 16)) + theme(legend.position = "none") +
  labs(caption = "Kaynak: HİA")+ theme(plot.caption = element_text(hjust = 0))



# Hourly real wages in informal sector by year between treatment and control groups 
ggplot(dt, aes(x= as.factor(year))) + geom_line(aes(y = infwage , group = as.factor(group), colour = as.factor(group)), size= 1.2) +
  geom_point(aes(y = infwage, color = as.factor(group), size= 0.5)) + 
  theme_bw() + scale_color_economist()+
  geom_label_repel(aes(y = infwage, label = label, color = group), point.padding = unit(8, "points"), na.rm = TRUE, size=8) +
  labs(title= "", x="", y="Kayıtsız Sektör Saatlik Reel Ücret (TL)") +
  theme(plot.title = element_text(size=30,  colour= "black" ),
        axis.title.x = element_text(size=30, colour = "black"),    
        axis.title.y = element_text(size=30, colour = "black"),    
        axis.text.x = element_text(size=30, colour = "black"), 
        axis.text.y = element_text(size=30, colour = "black"), 
        strip.text.x = element_text(size = 30, colour = "black" ),
        strip.text.y = element_text(size = 30, colour = "black"),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        plot.caption = element_text(color = "black", size = 16)) + theme(legend.position = "none") +
  labs(caption = "Kaynak: HİA")+ theme(plot.caption = element_text(hjust = 0))



# Unemployment rate by year between treatment and control groups 
ggplot(dt, aes(x= as.factor(year))) + geom_line(aes(y = unemployed , group = as.factor(group), colour = as.factor(group)), size= 1.2) +
  geom_point(aes(y = unemployed, color = as.factor(group), size= 0.5)) + 
  theme_bw() + scale_color_economist()+
  geom_label_repel(aes(y = unemployed, label = label, color = group), point.padding = unit(8, "points"), na.rm = TRUE, size=8) +
  labs(title= "", x="", y="İşsizlik Oranı") +
  theme(plot.title = element_text(size=30,  colour= "black" ), 
        axis.title.x = element_text(size=30, colour = "black"),    
        axis.title.y = element_text(size=30, colour = "black"),    
        axis.text.x = element_text(size=30, colour = "black"), 
        axis.text.y = element_text(size=30, colour = "black"), 
        strip.text.x = element_text(size = 30, colour = "black" ),
        strip.text.y = element_text(size = 30, colour = "black"),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        plot.caption = element_text(color = "black", size = 16)) + theme(legend.position = "none") +
  labs(caption = "Kaynak: HİA")+ theme(plot.caption = element_text(hjust = 0))










