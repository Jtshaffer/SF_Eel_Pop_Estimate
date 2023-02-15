
# Scrip to produce the population abundance estimates for the south fork eel DIDSON station at Meyers flat - JTS 2023

require(tidyverse)
require(lubridate)
require(dataRetrieval)
source("Missing_Hours_fxn.R")
source("Size_correction_fxn.R")
source("Missing_Day_fxn.R")


Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/Review Data/Review_Data-01-13-23.xlsx"))
Data<- Data %>% 
  mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>% 
  mutate(Date = ymd_h(Date))


# Obtain USGS data for site of interest. URL to helpful page: https://waterdata.usgs.gov/blog/dataretrieval/
siteNo<- "11476500" # location code for Miranda
pCode <- "00060"  # Data type: Discharge 
start.date <- "2022-10-31"
end.date <- as.character(today())

Miranda <- readNWISuv(siteNumbers = siteNo,
                      parameterCd = pCode,
                      startDate = start.date,
                      endDate = end.date)
Miranda<- renameNWISColumns(Miranda)



#Join the Review data and USGS flow data
names(Data)
names(Miranda)
Data<- left_join(Data,Miranda, by = c("Date" = "dateTime"))





# Remove the counts that are less than 40cm and correcting the net counts
Data<-Size_correction(Data)


## Accounting for single hours down ----
Data<- Missing_Hours(Data)


#Adding in correction factor and daily net movement and then correcting the daily net movement
day_adj<- day.adj(Data = Data, twenty_min_file = F)


#Adding in the monthly correction factor and monthly net movement then correcting the monthly movement
days.per.month<- day_adj %>% 
  group_by(Month) %>% 
  summarise(length(Day))


month.adj<- day_adj %>% 
  group_by(Month) %>% 
  summarise(monthlynet= sum(corrected.day.net,na.rm = T), missing_day_vals= n_distinct(which(corrected.day.net == 0.00))) %>% 
  mutate(monthcorrectionfactor = missing_day_vals/days.per.month[2], cor.month.net = round(monthlynet + (monthlynet*monthcorrectionfactor ))) %>% 
  ungroup()


