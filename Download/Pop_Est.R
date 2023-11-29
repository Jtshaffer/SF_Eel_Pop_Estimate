# Script to obtain estimates of salmonid passage from SONAR data 
#Developed by JTS 2023

require(tidyverse)
require(lubridate)
require(dataRetrieval)
source("Download/Missing_Hours_fxn.R")
source("Download/Size_correction_fxn.R")
source("Download/Missing_Day_fxn.R")
source("Download/Flow_Data_Fxn.R")


Data<- as.data.frame(readxl::read_xlsx("Data/Example_Data.xlsx"))
Data$Minute<- ifelse(grepl(".5",Data[["Hour"]],fixed = T) == T,30,0)
Data$Hour<- as.numeric(gsub(".5","",Data[["Hour"]],fixed = T))
Data<- Data %>% 
  # filter(!Minute == 30) %>%  #Currently this script only supports one 10 minute count per hour
  mutate(Date = paste(Year,Month,Day,Hour,Minute, sep = "-")) %>% 
  mutate(Date = ymd_hm(Date))


# Obtain USGS data for site of interest. URL to helpful page: https://waterdata.usgs.gov/blog/dataretrieval/
flowdata<- Flow_data_fxn(siteNo = "11476500",pCode= "00060",start.date= "2022-10-31",end.date = as.character(today()))

#Join the reviewed data and USGS flow data
Data<- left_join(Data,flowdata, by = c("Date" = "dateTime"))

# Remove the counts that are less than 40 cm to correct the net counts. Optional* 
# Data<-Size_correction(Data)


## Hourly counts *Includes option to interpolate data to missing intervals ----
Hourly_Data<- Hourly_counts(Data,interpolate= F,twenty_min_file = T)

#Estimating daily net movement *Includes option to correct for missing intervals
Daily_Data<- Daily_counts(Data = Hourly_Data, correction = T, twenty_min_file = F)


#Adding in the missing days correction factor and calculating monthly net movement- Not operating properly 11/28/2023
# days.per.month<- Daily_Data %>% 
#   group_by(Month) %>% 
#   summarise(Days= length(Day))
# 
# Monthly_passage<- Daily_Data %>% 
#   group_by(Month) %>% 
#   summarise(Monthlynet= sum(Corrected.Daily.Net,na.rm = T),
#             missing_day_vals= n_distinct(which(Corrected.Daily.Net == 0.00))) %>% 
#   mutate(monthcorrectionfactor = missing_day_vals/days.per.month$Days,
#          corrected.monthly.net = round(Monthlynet + (Monthlynet*monthcorrectionfactor ))) %>% 
#   ungroup()


#Viewing daily passage 
Daily_Data

#Viewing monthly passage 
Monthly_passage

