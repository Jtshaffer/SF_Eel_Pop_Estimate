
# Scrip to produce the population abundance estimates for the south fork eel DIDSON station at Meyers flat - JTS 2023

require(tidyverse)
require(lubridate)
require(dataRetrieval)
source("Missing_Hours_fxn.R")
source("Size_correction_fxn.R")

Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/Review Data/Review_Data-01-13-23.xlsx"))
Data<- Data %>% 
  mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>% 
  mutate(Date = ymd_h(Date))


# Remove the counts that are less than 40cm and correcting the net counts
Data<-Size_correction(Data)


## Accounting for single hours down ----
Data<- Missing_Hours(Data)


#Adding in correction factor and daily net movement and then correcting the daily net movement
day.adj<- Data %>% 
  group_by(Year,Month,Day) %>% 
  summarise(Dailynet= sum(hnet,na.rm = T), missinghrs=sum(is.na(hnet))) %>% 
  mutate(daycorrectionfactor= missinghrs/24, corrected.day.net = Dailynet + (Dailynet*daycorrectionfactor)) %>% 
  ungroup()


#Adding in the monthly correction factor and monthly net movement then correcting the monthly movement
days.per.month<- day.adj %>% 
  group_by(Month) %>% 
  summarise(length(Day))


month.adj<- day.adj %>% 
  group_by(Month) %>% 
  summarise(monthlynet= sum(corrected.day.net,na.rm = T), missing_day_vals= n_distinct(which(corrected.day.net == 0.00))) %>% 
  mutate(monthcorrectionfactor= missing_day_vals/days.per.month[2], cor.month.net= round(monthlynet + (monthlynet*monthcorrectionfactor ))) %>% 
  ungroup()


