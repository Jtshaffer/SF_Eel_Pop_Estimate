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
source("Download/V5Estimator.R")


Data<- as.data.frame(readxl::read_xlsx("Data/Example_Data.xlsx"))
Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SONAR/SFER_2023/2023-24 SF Eel sonar counts.xlsx"))
Data$Minute<- ifelse(grepl(".5",Data[["Hour"]],fixed = T) == T,30,0)
Data$Hour<- as.numeric(gsub(".5","",Data[["Hour"]],fixed = T))
Data<- Data %>% 
  # filter(!Minute == 30) %>%  
  mutate(Date = paste(Year,Month,Day,Hour,Minute, sep = "-")) %>% 
  mutate(Date = ymd_hm(Date))

# Obtain USGS data for site of interest. URL to helpful page: https://waterdata.usgs.gov/blog/dataretrieval/
flowdata<- Flow_data_fxn(siteNo = "11476500",pCode= "00060",start.date= "2023-10-10",end.date = as.character(today()))

 Max_flowdata<-flowdata %>% 
  mutate(Date = as.Date(dateTime)) %>% 
  group_by(Date) %>% 
  summarise(Flow_Inst = max(Flow_Inst))
  

# Remove the counts that are less than 40 cm to correct the net counts. Optional* 
# Data<-Size_correction(Data)


## Hourly counts *Includes option to interpolate data to missing intervals ----
# The current interpolation procedure: for any missing hourly passage estimate, the passage counts 24hrs prior to and after the missing hour are averaged and applied to the missing period. 
Hourly_Data<- Hourly_counts(Data,interpolate= T,twenty_min_file = T)


##Estimating daily net movement *Includes a correction for missing intervals ----
# The current correction procedure: the total number of missing intervals is computed and then divided by the total number of possible intervals (48 for 20 min files) to obtain a correction factor. The daily net movement is then multiplied by the correction factor to obtain the number of fish that would have been expected to move during the missing time intervals. These fish are then added to the daily net movement to obtain the total daily movement. 
Daily_Data<- Daily_counts(Data = Hourly_Data, correction = T, twenty_min_file = T)



#Monthly net movement
Monthly_Data<- Daily_Data %>%
  group_by(Month) %>%
  summarise(Monthlynet= sum(Corrected.Daily.Net,na.rm = T))



#Viewing daily passage 
Daily_Data

#Viewing monthly passage 
Monthly_Data

sum(Monthly_Data$Monthlynet)

# Daily passage estimate figure
# This function produces a plot showing the daily passage estimates plotted against the maximum daily flow at Miranda according to the USGS guage
daily_passage_plot(Max_flowdata,Daily_Data,DateBreaks = "2 day",dual_axis = T,startdate = "2023-10-10",enddate = today(),first.y.adj = 1000,sec.y.adj = 1000) # dual-axis
daily_passage_plot(Max_flowdata,Daily_Data,DateBreaks = "2 day",dual_axis = F,startdate = "2023-10-10",enddate = today())# Single axis, two plots



# Estimated variance 

Var_data<- Hourly_Data %>% 
  ungroup() %>% 
  select(Date,Hourly.Net) %>% 
  arrange(Date) 

Var_data<- Var_data %>% 
  filter(!Date %in% Var_data$Date[1:10] )

splt_var_data<- split(na.omit(Var_data$Hourly.Net), cumsum(is.na(Var_data$Hourly.Net))[!is.na(Var_data$Hourly.Net)]) # Split the data into lists of footage periods that are divided by the missing intervals. 

workable<- which( lengths(splt_var_data) >5)
unworkable<- which( lengths(splt_var_data) <5)

splt_var_data[workable]
splt_var_data[unworkable]

out<- data.frame(sapply(splt_var_data[workable], SystematicVar5,FPC = T,SamplingFrac = 0.33))

tmp<- out %>% 
  pivot_longer(cols = starts_with('X'),names_to = "period",values_to = c("data"))


cols = c('VarEst','DF')






