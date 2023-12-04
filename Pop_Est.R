# Script to obtain estimates of salmonid passage from SONAR data 
#Developed by JTS 2023

require(tidyverse)
require(lubridate)
require(dataRetrieval)
require(patchwork)
source("Download/Missing_Hours_fxn.R")
source("Download/Size_correction_fxn.R")
source("Download/Missing_Day_fxn.R")
source("Download/Flow_Data_Fxn.R")
source("Download/Daily_passage_plot_fxn.R")


Data<- as.data.frame(readxl::read_xlsx("Data/Example_Data.xlsx"))
Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SONAR/SFER_2023/BOX_SONAR_DATA11302023.xlsx"))
Data$Minute<- ifelse(grepl(".5",Data[["Hour"]],fixed = T) == T,30,0)
Data$Hour<- as.numeric(gsub(".5","",Data[["Hour"]],fixed = T))
Data<- Data %>% 
  # filter(!Minute == 30) %>%  
  mutate(Date = paste(Year,Month,Day,Hour,Minute, sep = "-")) %>% 
  mutate(Date = ymd_hm(Date))

No_sept<- Data %>% 
  filter(!Month==9)

# Obtain USGS data for site of interest. URL to helpful page: https://waterdata.usgs.gov/blog/dataretrieval/
flowdata<- Flow_data_fxn(siteNo = "11476500",pCode= "00060",start.date= "2023-10-9",end.date = as.character(today()))


# Remove the counts that are less than 40 cm to correct the net counts. Optional* 
# Data<-Size_correction(Data)


## Hourly counts *Includes option to interpolate data to missing intervals ----
# The current interpolation procedure: for any missing hourly passage estimate, the passage counts 24hrs prior to and after the missing hour are averaged and applied to the missing period. 
Hourly_Data<- Hourly_counts(No_sept,interpolate= T,twenty_min_file = T)

#Estimating daily net movement *Includes a correction for missing intervals.
# The current correction procedure: the total number of missing intervals is computed and then divided by the total number of possible intervals (48 for 20min files) to obtain a correction factor. The daily net movement is then multiplied by the correction factor to obtain the number of fish that would have been expected to move during the missing time intervals. These fish are then added to the daily net movement to obtain the total daily movement. 
Daily_Data<- Daily_counts(Data = Hourly_Data, correction = T, twenty_min_file = T)


#Monthly net movement
Monthly_Data<- Daily_Data %>%
  group_by(Month) %>%
  summarise(Monthlynet= sum(Corrected.Daily.Net,na.rm = T))



#Viewing daily passage 
Daily_Data

#Viewing monthly passage 
Monthly_Data


# Daily passage estimate figure
# This function produces a plot showing the daily passage estimates plotted against the USGS flow gauge at Miranda
daily_passage_plot(flowdata,Daily_Data,DateBreaks = "1 day",dual_axis = T)
daily_passage_plot(flowdata,Daily_Data,DateBreaks = "1 day",dual_axis = F,startdate = "2023-10-10",enddate = "2023-11-24")

