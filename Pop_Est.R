
# Scrip to produce the population abundance estimates for the south fork eel DIDSON station at Meyers flat - JTS 2023

require(tidyverse)
require(lubridate)
require(dataRetrieval)
source("Missing_Hours_fxn.R")
source("Size_correction_fxn.R")
source("Missing_Day_fxn.R")
source("Passage_plot_fxn.R")
source("Flow_Data_Fxn.R")


Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/2022-23 SF Eel sonar counts.xlsx"))
Data<- Data %>% 
  filter(!Hour %in% seq(0.5,23.5,1)) %>%  #Currently this code only supports one 10 minute count per hour
  mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>% 
  mutate(Date = ymd_h(Date))

# Obtain USGS data for site of interest. URL to helpful page: https://waterdata.usgs.gov/blog/dataretrieval/
flowdata<- Flow_data_fxn(siteNo = "11476500",pCode= "00060",start.date= "2022-10-31",end.date = as.character(today()))

#Join the Review data and USGS flow data
Data<- left_join(Data,flowdata, by = c("Date" = "dateTime"))

# Remove the counts that are less than 40 cm and correct the net counts. Optional* 
Data<-Size_correction(Data)

## Accounting for hours down ----
Data<- Missing_Hours(Data)

#Adding in a correction for missed hours and then estimating daily net movement
day_adj<- day.adj(Data = Data, twenty_min_file = F)

#Adding in the missing days correction factor and calculating monthly net movement
days.per.month<- day_adj %>% 
  group_by(Month) %>% 
  summarise(Days= length(Day))

month_adj<- day_adj %>% 
  group_by(Month) %>% 
  summarise(Monthlynet= sum(corrected.daily.net,na.rm = T),
            missing_day_vals= n_distinct(which(corrected.daily.net == 0.00))) %>% 
  mutate(monthcorrectionfactor = missing_day_vals/days.per.month$Days,
         corrected.monthly.net = round(Monthlynet + (Monthlynet*monthcorrectionfactor ))) %>% 
  ungroup()


#Viewing daily passage 
day_adj

#Viewing monthly passage 
month_adj
