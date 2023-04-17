
library(tidyverse)
library(lubridate)
library(dataRetrieval)



# Randomly interject NA values to a data frame
#source("NA_Value_Creator.R")

# Randomly interject NA values to a vector
# ind <- which(foo %in% sample(foo, 15))
# #now replace those indices in foo with NA
# foo[ind]<-NA

# 
# #Read in review data and add Date
# Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/Review Data/Review_Data-01-13-23.xlsx"))
# Data<- Data %>% 
#   #mutate(Hnet = Net * 6) %>% 
#   mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>% 
#   mutate(Date = ymd_h(Date))
# 
# 
# Data$lessthan40<- apply(Data[8:27],1,function(x){  # Function to remove the counts that are less than 40cm and correct the net counts
#   sum(x < 40,na.rm = T)
#   }
# )    
# 
# Data<- Data %>% 
#   mutate(Net.corrected = Net - lessthan40)
# 
# 
# 
# 
# # Correcting the NA values
# # Scenario 1: if there is an NA then take the average of the net movement 24 hours prior and 24 hours after  # Step 1
# 
# #  Error: Currently the function is pasting the same value for all missing NAs. Need this to treat things on a case by case basis
# # The problem is that I am calling Dat[...][1] which is making the answer the same for all values 
# #  Need to find a way to reference the cell location of x 
# # Solved in section 1a
# 
# # Dat$new.net<- sapply(Dat[,3],function(x)  # original
# #   if_else(is.na(x), mean(c(Dat[which(is.na(Dat),arr.ind = T)[1]-24,3],Dat[which(is.na(Dat),arr.ind = T)[1]+24,3])),x)) 
# 
# 
# #Senario 1a: If there are multiple NA values and missing days  ----  # If there is 1 missing value have the test fail
# #Data Creation
# Day1<- rep(1,24)
# Day2<- rep(2,24)
# Day3<- rep(3,24)
# Day4<- rep(4,24)
# Day5<- rep(5,24)
# Day6<- rep(6,24)
# Day<- c(Day1,Day2,Day3,Day4,Day5,Day6)
# Hour<- rep(0:23,3)
# Net <- round(rnorm(length(Day),mean = 2))
# Dat<- data.frame(Day= Day,Hour= Hour,Net= Net)
# 
# #Populate missing observations
# Dat[27,3]<- NA
# Dat[31,3]<- NA
# Dat[49:120,3]<- NA
# 
# Dat
# 
# Dat$New.net <- Dat[,3]
# 
# 
# #Solution
# 
# ind <- which(is.na(Dat[[3]]))
# ind_minus <- ind - 24
# ind_minus[ind_minus < 1] <- NA
# ind_plus <- ind + 24
# ind_plus[ind_plus > nrow(Dat)] <- NA
# 
# Dat[[4]][ind] <- rowMeans(cbind(Dat[[4]][ind_minus], Dat[[4]][ind_plus]),
#                           na.rm = F)
# 
# 
# 
# #Scenario 2: if there are NA values in 24hrs before or after the missing hour.  # No longer  needed as it is in scenario 3 ----
# #             take the un-adjusted daily count and add in an adjustement for the missing hours E.adj = E.unadj + (E.unadj * z/24)     # Step 2
# # set.seed(1)
# # 
# # #Data Creation
# # Day1<- rep(1,24)
# # Day2<- rep(2,24)
# # Day3<- rep(3,24)
# # Day<- c(Day1,Day2,Day3)
# # 
# # Hour<- rep(0:23,3)
# # Net <- round(rnorm(length(Day),mean = 2))
# # 
# # Dat<- data.frame(Day= Day,Hour= Hour,Net= Net)
# # Dat[27,3]<- NA
# # Dat[3,3]<- NA
# # Dat[51,3]<- NA
# # Dat[31,3]<- NA
# # Dat
# # #Scaling up the observations 
# # Dat<- Dat %>% 
# #   mutate(hnet= Net*6) 
# # 
# # #Adding in correction factor and daily net movement
# # pre.adj<- Dat %>% 
# #   group_by(Day) %>% 
# #   summarise(Dailynet= sum(hnet,na.rm = T),missingvals=sum(is.na(Net))) %>% 
# #   mutate(correctionfactor= missingvals/24)
# # 
# # # The daily counts adjusted for missing hours 
# # post.adj<- pre.adj %>% 
# #   mutate(corected.dailynet = Dailynet + (Dailynet*correctionfactor))
# 
# #Scenario 3: build in the adjustment for missing consecutive days in a month ----
# # Building in a variance estimator ----
# set.seed(1)
# #Data Creation
# Day1<- rep(1,24)
# Day2<- rep(2,24)
# Day3<- rep(3,24)
# Day4<- rep(4,24)
# Day5<- rep(5,24)
# Day6<- rep(6,24)
# Day7<- rep(7,24)
# Day8<- rep(8,24)
# Day9<- rep(9,24)
# Day10<- rep(10,24)
# Day11<- rep(11,24)
# Day<- c(Day1,Day2,Day3,Day4,Day5,Day6,Day7,Day8,Day9,Day10,Day11)
# 
# Hour<- rep(0:23,11)
# Month<- rep(1,length(Day))
# Net <- round(rnorm(length(Day),mean = 10,sd = 3.5))
# 
# Dat<- data.frame(Month= Month, Day= Day,Hour= Hour,Net= Net)
# Dat[27,4]<- NA
# Dat[3,4]<- NA
# Dat[49:120,4]<- NA
# ind <- which(Dat$Net %in% sample(Dat$Net, replace = F,1))
# Dat$Net[ind]<-NA
# 
# Dat<- Dat %>% 
#   mutate(hnet= Net*6) 
# 
# #Adding in correction factor and daily net movement and then correcting the daily net movement
# day.adj<- Dat %>% 
#   group_by(Month,Day) %>% 
#   summarise(Dailynet= sum(hnet,na.rm = T),missinghrs=sum(is.na(Net))) %>% 
#   mutate(daycorrectionfactor= missinghrs/24, cor.day.net = Dailynet + (Dailynet*daycorrectionfactor))
# 
# #Adding in the monthly correction factor and monthly net movement then correcting the monthly movement
# month.adj<- day.adj %>% 
#   group_by(Month) %>% 
#   summarise(monthlynet= sum(cor.day.net,na.rm = T), missing_day_vals= n_distinct(which(cor.day.net == 0.00))) %>% 
#   mutate(monthcorrectionfactor= missing_day_vals/length(Day), cor.month.net= round(monthlynet + (monthlynet*monthcorrectionfactor ) ))
# 
# 
# 
# #Variance estimator ----
# #day.adj$cor.day.net<- sapply(day.adj$cor.day.net, function(x)ifelse(x == 0,NA,x))
# 
# #I need to make a better simulatated dataset that includes Every hour over 5 days and then pull out only the first observation of each hour to compare the estimates. 
# 
# set.seed(1)
# #Data Creation
# Day1<- rep(1,144)
# Day2<- rep(2,144)
# Day3<- rep(3,144)
# Day4<- rep(4,144)
# Day5<- rep(5,144)
# Day6<- rep(6,144)
# Day7<- rep(7,144)
# Day8<- rep(8,144)
# Day9<- rep(9,144)
# Day10<- rep(10,144)
# Day11<- rep(11,144)
# Day<- c(Day1,Day2,Day3,Day4,Day5,Day6,Day7,Day8,Day9,Day10,Day11)
# obs<- round(rnorm(length(Day),mean = 5,sd = 2.5))
# period <- rep(seq(from= 1 ,to =6),264)
# data<- data.frame(Day,period ,obs)
# 
# 
# total_passage<- sum(obs)
# Standarddev<- sd(obs)
# variance<- var(obs)
# 
# 
# 
# #Converting the data to the survey format
# samp_data<- filter(data,period == 1) # Sampling only the first 10 minutes of each hour
# samp_data$Hour<- rep(0:23,11)
# samp_data<- samp_data %>% 
#   select(!period)
# samp_data$Month<- rep(1,nrow(samp_data))
# Dat<- samp_data
# 
# Dat<- Dat %>% 
#   mutate(hnet= obs*6) 
# 
# 
# 
# #Getting the variables of the V5 estimator 
# 
# f<- 1/6 # The proportion of possible observations that were actually collected
# n<- nrow(filter(Dat,!is.na(hnet))) # number of hours that were sampled 
# 
# y_bar<- round(sum(Dat$hnet/n,na.rm = T)) #Daily mean escapement (sum(y_sub_i)/n)
# 
# tmp<-sapply(5:length(Dat$hnet), function(x){
#   (Dat[x,]/2 - Dat[x-1,]+ Dat[x-2,] - Dat[x-3,] + Dat[x-4,]/2)^2/(3.5*(n-4))
# })
# 
# Cj<- unlist(tmp['hnet',])
# x<- sum(Cj,na.rm = T)
# 
# Vhat_ybar<- (1-f)*(1/n)*x
# 
# Vhat_Yhat <- Vhat_ybar * (6*24*11)
# 
# V_Y<- var(Dat$hnet,na.rm = T)
# 
# 
# 
# 
# #Var estimator - Hourly 
# 
# 
# f<- 1/6 # The proportion of possible observations that were collected
# n<- Dat %>% 
#   filter(!is.na(hnet)) %>% 
#   summarise(length(hnet))
# n<- n$`length(hnet)`
# 
# 
# N<- length(Dat$hnet)
# 
# y_bar<- round(sum(day.adj$cor.day.net/length(filter(day.adj,!is.na(cor.day.net)) ),na.rm = T)) #Daily mean escapement (sum(y_sub_i)/n)
# 
# tmp<-sapply(5:length(Dat$hnet), function(x){
#   (Dat[x,]/2 - Dat[x-1,]+ Dat[x-2,] - Dat[x-3,] + Dat[x-4,]/2)^2 / (3.5*(n-4))
# })
# 
# Cj<- unlist(tmp["hnet",])
# sum(Cj,na.rm = T)
# 
# V5<- (1-f)*(1/n)*sum(Cj,na.rm = T)
# 
# Vhat_Yhat <- V5 * (6*24*11)^2
# 
# V_Y<- var(Dat$hnet,na.rm = T)
# sd(Dat$hnet,na.rm = T)
# 
# 
# 
# 
# 
# 
# 
# # Combine the three scenarios ----
# 
# #Example data 
# # set.seed(1)
# # #Data Creation
# # Day1<- rep(1,24)
# # Day2<- rep(2,24)
# # Day3<- rep(3,24)
# # Day4<- rep(4,24)
# # Day5<- rep(5,24)
# # Day6<- rep(6,24)
# # Day<- c(Day1,Day2,Day3,Day4,Day5,Day6)
# # 
# # Hour<- rep(1:24,6)
# # Month<- rep(1,length(Day))
# # Net <- round(rnorm(length(Day),mean = 2))
# # 
# # Dat<- data.frame(Month= Month, Day= Day,Hour= Hour,Net= Net)
# # Dat[27,4]<- NA
# # Dat[3,4]<- NA
# # Dat[49:120,4]<- NA
# # 
# # #Insert sample of random missing values
# # ind <- which(Dat$Net %in% sample(Dat$Net, 5))
# # Dat$Net[ind]<-NA
# # 
# # Data<-Dat
# # Data$Net.corrected<-Data$Net
# 
# #Trial on all currently available data ----
# 
# Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/Review Data/Review_Data-01-13-23.xlsx"))
# Data<- Data %>%
#   #mutate(Hnet = Net * 6) %>%
#   mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>%
#   mutate(Date = ymd_h(Date))
# 
# 
# Data$lessthan40<- apply(Data[8:27],1,function(x){  # Function to remove the counts that are less than 40cm and correct the net counts
#     sum(x < 40,na.rm = T)
#   }
#   )
# 
# Data<- Data %>%
#     mutate(Net.corrected = Net - lessthan40)
# 
# ## Accounting for single hours down ----
# 
# Data$New.Net<- Data[["Net.corrected"]]
# ind <- which(is.na(Data[["Net.corrected"]]))
# ind_minus <- ind - 24
# ind_minus[ind_minus < 1] <- NA
# ind_plus <- ind + 24
# ind_plus[ind_plus > nrow(Data)] <- NA
# 
# Data[["New.Net"]][ind] <- rowMeans(cbind(Data[["New.Net"]][ind_minus], Data[["New.Net"]][ind_plus]),
#                           na.rm = TRUE)
# 
# 
# Data<- Data %>% 
#   mutate(hnet= New.Net*6) 
# 
# 
# #Adding in correction factor and daily net movement and then correcting the daily net movement
# #Building in a function to note if one ten-minute file or two ten-minute files have been reviewed. 
# 
# day.adj<-function(Data= X, twenty_min_file = T){
#   if(twenty_min_file == T){ 
#           Data %>% 
#             group_by(Year,Month,Day) %>% 
#             summarise(Dailynet= sum(hnet,na.rm = T), missinghrs=sum(is.na(hnet))) %>% 
#             mutate(daycorrectionfactor= missinghrs/48, corrected.day.net = Dailynet + (Dailynet*daycorrectionfactor)) %>% 
#             ungroup()}
#   else{
#           Data %>% 
#             group_by(Year,Month,Day) %>% 
#             summarise(Dailynet= sum(hnet,na.rm = T), missinghrs=sum(is.na(hnet))) %>% 
#             mutate(daycorrectionfactor= missinghrs/24, corrected.day.net = Dailynet + (Dailynet*daycorrectionfactor)) %>% 
#             ungroup()}
#   
# }
# 
# 
# day.adj()
# 
# day_adj<- day.adj(Data = Data,F)
# 
# #Adding in the monthly correction factor and monthly net movement then correcting the monthly movement
# days.per.month<- day_adj %>% 
#   group_by(Month) %>% 
#   summarise(length(Day))
# 
# 
# month_adj<- day_adj %>% 
#   group_by(Month) %>% 
#   summarise(monthlynet= sum(corrected.day.net,na.rm = T), missing_day_vals= n_distinct(which(corrected.day.net == 0.00))) %>% 
#   mutate(monthcorrectionfactor= missing_day_vals/days.per.month[2], cor.month.net= round(monthlynet + (monthlynet*monthcorrectionfactor ))) %>% 
#   ungroup()
# 
# 
# 
# 
# # Obtain USGS data for site of interest. URL to helpful page: https://waterdata.usgs.gov/blog/dataretrieval/
# Flow_data_fxn<- function(siteNo,pCode,start.date,end.date){
# siteNo<- "11476500" # location code for Miranda
# pCode <- "00060"  # Data type: Discharge 
# start.date <- "2022-10-31"
# end.date <- as.character(today())
# 
# tmp <- readNWISuv(siteNumbers = siteNo,
#                        parameterCd = pCode,
#                        startDate = start.date,
#                        endDate = end.date)
# tmp<- renameNWISColumns(tmp) 
# }
# 
# 
# flowdata<- Flow_data_fxn("11476500","00060","2022-10-31",as.character(today()))
# 
# 
# Data<- left_join(Data,flowdata, by = c("Date" = "dateTime"))
# 
# 
# Data<-Data %>% 
#   group_by(Month,Day) %>% 
#   mutate(Daily.count = sum(Hnet,na.rm = T), Monthly.count = sum(Daily.count,na.rm = T))
# 
# 
# # Plotting ---- 
# 
# Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/2022-23 SF Eel sonar counts.xlsx"))
# Data<-Data %>% 
#   filter(!Hour %in% seq(0.5,23.5,1)) # Error in parsing out dates for the half hour marks 3-03-23
# Data<- Data %>%  
#   mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>% 
#   mutate(Date = ymd_h(Date))
# 
# # Obtain USGS data for site of interest. URL to helpful page: https://waterdata.usgs.gov/blog/dataretrieval/
# siteNo<- "11476500" # location code for Miranda
# pCode <- "00060"  # Data type: Discharge 
# start.date <- "2022-10-31"
# end.date <- as.character(today())
# Miranda <- readNWISuv(siteNumbers = siteNo,
#                       parameterCd = pCode,
#                       startDate = start.date,
#                       endDate = end.date)
# Miranda<- renameNWISColumns(Miranda)
# 
# #Join the Review data and USGS flow data
# names(Data)
# names(Miranda)
# Data<- left_join(Data,Miranda, by = c("Date" = "dateTime"))
# 
# # Remove the counts that are less than 40cm and correcting the net counts
# Data<-Size_correction(Data)
# 
# ## Accounting for single hours down 
# Data<- Missing_Hours(Data)
# 
# #Adding in correction factor and daily net movement and then correcting the daily net movement
# day_adj<- day.adj(Data = Data, twenty_min_file = F)
# names(day_adj)<- c("Year", "Month", "Day", "Dailynet", "missinghrs", "daycorrectionfactor", "corrected.daily.net"  )
# 
# Day_data<- day_adj %>% 
#   select(!"Dailynet":"daycorrectionfactor")
# 
# #Adding in the monthly correction factor and monthly net movement then correcting the monthly movement
# days.per.month<- day_adj %>% 
#   group_by(Month) %>% 
#   summarise(length(Day))
# 
# month.adj<- day_adj %>% 
#   group_by(Month) %>% 
#   summarise(Monthlynet= sum(corrected.daily.net,na.rm = T), missing_day_vals= n_distinct(which(corrected.daily.net == 0.00))) %>% 
#   mutate(monthcorrectionfactor = missing_day_vals/days.per.month$`length(Day)`, corrected.monthly.net = round(Monthlynet + (Monthlynet*monthcorrectionfactor ))) %>% 
#   ungroup()
# 
# names(month.adj)<- c("Month","Monthlynet","missing_day_vals","monthcorrectionfactor", "corrected.monthly.net" )  
# 
# Month_data<- month.adj %>% 
#   select("Month", "corrected.monthly.net")
# names(Month_data)
# 
# 
# ## Join the daily and monthly counts to the original data set so that counts and the flows can be plotted together----
# 
# Data<- left_join(Data,Day_data,by = c('Year','Month','Day'))
# Data<- left_join(Data,Month_data,by = c('Month'))
# 
# Data$Date<- as.character(Data$Date)
# Data<-separate(Data,Date,into = c("USGS_Date","USGS_Hour"), sep = " ",remove = F)
# Data$USGS_Date<- as.POSIXct(Data$USGS_Date)
# Data$Date<- as.POSIXct(Data$Date)
# 
# ##Plot the two variables ----
# 
# 
# Passage_plot<- function(Data){
# 
# # function to scale the axis automatically 
# max_first  <- max(Data$corrected.daily.net,na.rm = T)   # Specify max of first y axis
# max_second <- max(Data$Flow_Inst,na.rm = T) # Specify max of second y axis
# min_first  <- min(day_adj$corrected.daily.net,na.rm = T)   # Specify min of first y axis
# min_second <- min(Data$Flow_Inst,na.rm = T) # Specify min of second y axis
# 
# # scale and shift variables calculated based on desired mins and maxes
# scale = (max_second - min_second)/(max_first - min_first)
# shift = min_first - min_second
# 
# # Function to scale secondary axis
# scale_function <- function(x, scale, shift){
#   return ((x)*scale - shift)
# }
# 
# # Function to scale secondary variable values
# inv_scale_function <- function(x, scale, shift){
#   return ((x + shift)/scale)
# }
# 
# plot <- Data %>%
#   ggplot(aes(x = Date, y = corrected.daily.net)) +
#   geom_point(shape = 20,size = 2) +
#   geom_line(aes(y = inv_scale_function(Flow_Inst, scale, shift)),col = 'blue') +
#   scale_y_continuous(limits = c(min_first, max_first), 
#                      sec.axis = sec_axis(~scale_function(., scale, shift),
#                      name = "USGS Miranda gage flow (cfs)"))+  # Find a way to make this work for each unique site
#   ylab("Daily passage estimate")+
#   theme_classic()
# 
# plot
# 
# }
# 
# 
# tmp<- Data %>% 
#   filter(Month ==11 )
# 
# 
# Passage_plot(Data= Data)
# Passage_plot(Data= tmp)






# Working Script ----

require(tidyverse)
require(lubridate)
require(dataRetrieval)
source("Download/Missing_Hours_fxn.R")
source("Download/Size_correction_fxn.R")
source("Download/Missing_Day_fxn.R")
source("Download/Passage_plot_fxn.R")
source("Download/Flow_Data_Fxn.R")


Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/2022-23 SF Eel sonar counts.xlsx"))
Data$Minute<- ifelse(grepl(".5",Data[["Hour"]],fixed = T) == T,print(30),print(0)) # Minutes need to be added to the fall 2023 version of the data for record keeping purposes
Data$Hour<- gsub(".5","",Data[["Hour"]],fixed = T)# Hours should be whole numbers in the fall 2023 data
Data<- Data %>% 
  #filter(!Hour %in% seq(0.5,23.5,1)) %>%  #Currently this code only supports one 10 minute count per hour
  mutate(Date = paste(Year,Month,Day,Hour,Minute, sep = "-")) %>% 
  mutate(Date = ymd_hm(Date))
  

# Obtain USGS data for site of interest. URL to helpful page: https://waterdata.usgs.gov/blog/dataretrieval/
flowdata<- Flow_data_fxn(siteNo = "11476500",pCode= "00060",start.date= "2022-10-31",end.date = as.character(today()))

#Join the Review data and USGS flow data
Data<- left_join(Data,flowdata, by = c("Date" = "dateTime"))

# Remove the counts that are less than 40 cm and correct the net counts. Optional* 
Data<-Size_correction(Data)

## Accounting for hours down 
Data<- Missing_Hours(Data)

#Adding in a correction for missed hours and then estimating daily net movement
day_adj<- day.adj(Data = Data, twenty_min_file = T)

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


