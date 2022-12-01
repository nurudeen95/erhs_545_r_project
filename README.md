# erhs_545_r_project


##setup and library
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forcats)
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

##data entry and cleaning
setwd("C:/Users/nurud/OneDrive/Desktop/Ph.D. Classes/ERHS581A3 R Programming for Research I/EHRS 545 R Project/EHRS 545 R Project/data")
homicide <- read.csv("homicide-data.csv")
baltimore_homicide <- homicide %>% 
  filter(city == "Baltimore") %>% 
  mutate(reported_date = ymd(reported_date),
         year = year(reported_date),
         month = month(reported_date))

##summarizing data       
baltimore_homicide_summ <- baltimore_homicide %>% 
  group_by(year, month) %>% 
  summarize(count = n()) %>% 
  unite(col = date, c(year, month), sep = "-",
        remove = FALSE) %>% 
  mutate(date = ym(date),
         season = case_when(month >= 5 & month <= 10 ~ "Summer",
                            month < 5 ~ "Winter",
                            month > 10 ~ "Winter"),
         season = fct_relevel(season, c("Summer", "Winter")))

##freddie gray         
freddie_grey <- baltimore_homicide %>% 
  filter(victim_last == "GREY") %>% 
  unite(col = date, c(year, month), sep = "-",
        remove = FALSE) %>% 
  mutate(date = ym(date))
  
ggplot()+
  geom_bar(data = baltimore_homicide_summ,
                 aes(x = date, weight = count, fill = season), 
                 bins = 132, binwidth = 17)+
  geom_smooth(data = baltimore_homicide_summ,aes(x = date, y = count), colour="dodgerblue1", 
              span = 0.15, se = FALSE, size = 2)+
  theme_dark(base_size = 15)+
  scale_fill_manual(name = " ", values = c("Winter" = "slategray1",
                               "Summer" = "gray87"))+
  annotate("text", x = freddie_grey$date, y = 40, label = "Arrest of \n Freddie Gray",
           col = "white", hjust = 1.1, size = 6)+
  geom_vline(xintercept = freddie_grey$date, color = "red", linetype = "longdash",
             size = 2)+
  theme(legend.position = "bottom")+
  labs(title = "Homicides in Baltimore, MD")+
  xlab("Date")+
  ylab("Monthly Homicides")
  
  ##plot
  ggplot()+
  geom_bar(data = baltimore_homicide_summ,
                 aes(x = date, weight = count, fill = season), 
                 bins = 132, binwidth = 17)+
  geom_smooth(data = baltimore_homicide_summ,aes(x = date, y = count), colour="dodgerblue1", 
              span = 0.15, se = FALSE, size = 2)+
  theme_dark(base_size = 15)+
  scale_fill_manual(name = " ", values = c("Winter" = "slategray1",
                               "Summer" = "gray87"))+
  annotate("text", x = freddie_grey$date, y = 40, label = "Arrest of \n Freddie Gray",
           col = "white", hjust = 1.1, size = 6)+
  geom_vline(xintercept = freddie_grey$date, color = "red", linetype = "longdash",
             size = 2)+
  theme(legend.position = "bottom")+
  labs(title = "Homicides in Baltimore, MD")+
  xlab("Date")+
  ylab("Monthly Homicides")