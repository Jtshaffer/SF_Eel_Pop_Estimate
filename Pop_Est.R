
# Scrip to produce the population abundance estimates for the south fork eel DIDSON station at Meyers flat - JTS 2023

require(tidyverse)
require(lubridate)
require(dataRetrieval)
source("Missing_Hours_fxn.R")
source("Size_correction_fxn.R")
source("Missing_Day_fxn.R")
source("Passage_plot_fxn.R")


Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/2022-23 SF Eel sonar counts.xlsx"))

Data<-Data %>% 
  filter(!Hour %in% seq(0.5,23.5,1))

Data<- Data %>%  # Error in parsing out dates for the half hour marks 3-03-23
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
  summarise(Monthlynet= sum(corrected.daily.net,na.rm = T), missing_day_vals= n_distinct(which(corrected.daily.net == 0.00))) %>% 
  mutate(monthcorrectionfactor = missing_day_vals/days.per.month$`length(Day)`,
         corrected.monthly.net = round(Monthlynet + (Monthlynet*monthcorrectionfactor ))) %>% 
  ungroup()


## Join the daily and monthly counts to the orignial dataset so that counts and the flows can be plotted together----
Data<- left_join(Data,Day_data,by = c('Year','Month','Day'))
Data<- left_join(Data,Month_data,by = c('Month'))
Data$Date<- as.character(Data$Date)
Data<-separate(Data,Date,into = c("USGS_Date","USGS_Hour"), sep = " ",remove = F)
Data$USGS_Date<- as.POSIXct(Data$USGS_Date)
Data$Date<- as.POSIXct(Data$Date)


# Plot the data
Passage_plot(Data)


# To plot only a single or set of months

Passage_plot(filter(Data,Month == 11))
