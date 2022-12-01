# erhs_545_r_project


##setup and library
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forcats)
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

setwd("C:/Users/nurud/OneDrive/Desktop/Ph.D. Classes/ERHS581A3 R Programming for Research I/EHRS 545 R Project/EHRS 545 R Project/data")
homicide <- read.csv("homicide-data.csv")
baltimore_homicide <- homicide %>% 
  filter(city == "Baltimore") %>% 
  mutate(reported_date = ymd(reported_date),
         year = year(reported_date),
         month = month(reported_date))