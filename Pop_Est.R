
library(tidyverse)
library(lubridate)
library(dataRetrieval)

# Means to randomly interject NA values to a data frame
#source("NA_Value_Creator.R")

#Way to randomly interject NA values to a vector
# ind <- which(foo %in% sample(foo, 15))
# #now replace those indices in foo with NA
# foo[ind]<-NA


#Read in review data and add Date
Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/Review Data/Review_Data-01-13-23.xlsx"))
Data<- Data %>% 
  mutate(Hnet = Net * 6) %>% 
  mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>% 
  mutate(Date = ymd_h(Date))


Data$lessthan40<- apply(Data[8:27],1,function(x){  # Function to remove the counts that are less than 40cm and correct the net counts
  sum(x < 40,na.rm = T)
  }
)    

Data<- Data %>% 
  mutate(Net.corrected = Net - lessthan40)




# Correcting the NA values
# Scenario 1: if there is an NA then take the average of the net movement 24 hours prior and 24 hours after  # Step 1

#  Error: Currently the function is pasting the same value for all missing NAs. Need this to treat things on a case by case basis
# The problem is that I am calling Dat[...][1] which is making the answer the same for all values 
#  Need to find a way to reference the cell location of x 
# Solved in section 1a

# Dat$new.net<- sapply(Dat[,3],function(x)  # original
#   if_else(is.na(x), mean(c(Dat[which(is.na(Dat),arr.ind = T)[1]-24,3],Dat[which(is.na(Dat),arr.ind = T)[1]+24,3])),x)) 


#Senario 1a: If there are multiple NA values
#Data Creation
Day1<- rep(1,24)
Day2<- rep(2,24)
Day3<- rep(3,24)
Day<- c(Day1,Day2,Day3)
Hour<- rep(0:23,3)
Net <- round(rnorm(length(Day),mean = 2))
Dat<- data.frame(Day= Day,Hour= Hour,Net= Net)

#Populate missing observations
Dat[27,3]<- NA
Dat[31,3]<- NA
Dat

Dat$New.net <- Dat[,3]


#Solution

ind <- which(is.na(Dat[[3]]))
ind_minus <- ind - 24
ind_minus[ind_minus < 1] <- NA
ind_plus <- ind + 24
ind_plus[ind_plus > nrow(Dat)] <- NA

Dat[[4]][ind] <- rowMeans(cbind(Dat[[4]][ind_minus], Dat[[4]][ind_plus]),
                          na.rm = TRUE)



#Scenario 2: if there are NA values in 24hrs before or after the missing hour. 
#             take the un-adjusted daily count and add in an adjustement for the missing hours E.adj = E.unadj + (E.unadj * z/24hrs)     # Step 2
set.seed(1)

#Data Creation
Day1<- rep(1,24)
Day2<- rep(2,24)
Day3<- rep(3,24)
Day<- c(Day1,Day2,Day3)

Hour<- rep(0:23,3)
Net <- round(rnorm(length(Day),mean = 2))

Dat<- data.frame(Day= Day,Hour= Hour,Net= Net)
Dat[27,3]<- NA
Dat[3,3]<- NA
Dat[51,3]<- NA
Dat[31,3]<- NA
Dat
#Scaling up the observations 
Dat<- Dat %>% 
  mutate(hnet= Net*6) 

#Adding in correction factor and daily net movement
pre.adj<- Dat %>% 
  group_by(Day) %>% 
  summarise(Dailynet= sum(hnet,na.rm = T),missingvals=sum(is.na(Net))) %>% 
  mutate(correctionfactor= missingvals/24)

# The daily counts adjusted for missing hours 
post.adj<- pre.adj %>% 
  mutate(corected.dailynet = Dailynet + (Dailynet*correctionfactor))

#Scenario 3: build in the adjustment for missing consecutive days in a month
set.seed(1)
#Data Creation
Day1<- rep(1,24)
Day2<- rep(2,24)
Day3<- rep(3,24)
Day4<- rep(4,24)
Day5<- rep(5,24)
Day6<- rep(6,24)
Day<- c(Day1,Day2,Day3,Day4,Day5,Day6)

Hour<- rep(1:24,6)
Month<- rep(1,length(Day))
Net <- round(rnorm(length(Day),mean = 2))

Dat<- data.frame(Month= Month, Day= Day,Hour= Hour,Net= Net)
Dat[27,4]<- NA
Dat[3,4]<- NA
Dat[49:96,4]<- NA

Dat<- Dat %>% 
  mutate(hnet= Net*6) 

#Adding in correction factor and daily net movement and then correcting the daily net movement
day.adj<- Dat %>% 
  group_by(Month,Day) %>% 
  summarise(Dailynet= sum(hnet,na.rm = T),missinghrs=sum(is.na(Net))) %>% 
  mutate(daycorrectionfactor= missinghrs/24, cor.day.net = Dailynet + (Dailynet*daycorrectionfactor))

#Adding in the monthly correction factor and monthly net movement then correcting the monthly movement
month.adj<- day.adj %>% 
  group_by(Month) %>% 
  summarise(monthlynet= sum(cor.day.net,na.rm = T), missing_day_vals= n_distinct(which(cor.day.net == 0.00))) %>% 
  mutate(monthcorrectionfactor= missing_day_vals/length(Day), cor.month.net= round(monthlynet + (monthlynet*monthcorrectionfactor ) ))



# Combine the three scenarios 

set.seed(1)
#Data Creation
Day1<- rep(1,24)
Day2<- rep(2,24)
Day3<- rep(3,24)
Day4<- rep(4,24)
Day5<- rep(5,24)
Day6<- rep(6,24)
Day<- c(Day1,Day2,Day3,Day4,Day5,Day6)

Hour<- rep(1:24,6)
Month<- rep(1,length(Day))
Net <- round(rnorm(length(Day),mean = 2))

Dat<- data.frame(Month= Month, Day= Day,Hour= Hour,Net= Net)
Dat[27,4]<- NA
Dat[3,4]<- NA
Dat[49:96,4]<- NA

# Accounting for single hours down
Dat$new.Net<- sapply(Dat[,4],function(x)
  if_else(is.na(x), mean(c(Dat[which(is.na(Dat),arr.ind = T)[1]-24,3],Dat[which(is.na(Dat),arr.ind = T)[1]+24,3])),x)) 


Dat<- Dat %>% 
  mutate(hnet= Net*6) 











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


Data<-Data %>% 
  group_by(Month,Day) %>% 
  mutate(Daily.count = sum(Hnet,na.rm = T), Monthly.count = sum(Daily.count,na.rm = T))



#Plot the two variables 
scale.factor<- 10


# function to scale the axis automatically 
max_first  <- max(Data$Daily.count)   # Specify max of first y axis
max_second <- max(Data$Flow_Inst) # Specify max of second y axis
min_first  <- min(Data$Daily.count)   # Specify min of first y axis
min_second <- min(Data$Flow_Inst) # Specify min of second y axis

# scale and shift variables calculated based on desired mins and maxes
scale = (max_second - min_second)/(max_first - min_first)
shift = min_first - min_second

# Function to scale secondary axis
scale_function <- function(x, scale, shift){
  return ((x)*scale - shift)
}

# Function to scale secondary variable values
inv_scale_function <- function(x, scale, shift){
  return ((x + shift)/scale)
}


plot <- Data %>%
  #filter(Month == 11) %>% 
  ggplot(aes(x = Date, y = Daily.count)) +
  geom_point() +
  geom_line(aes(y = inv_scale_function(Flow_Inst, scale, shift))) +
  scale_y_continuous(limits = c(min_first, max_first), sec.axis = sec_axis(~scale_function(., scale, shift)))
plot





Daily.movement<-Data %>% 
  group_by(Year,Month,Day) %>% 
  summarise(Net = sum(Hnet,na.rm = T))

Daily.movement %>% 
  filter(Month == 12) %>% 
  ggplot(aes(Day,Net))+
  geom_point() +
  # geom_line(data = Data, aes(y = Flow_Inst/scale.factor))+ 
  # scale_y_continuous(sec.axis = sec_axis(~.*scale.factor, name="Discharge (m^3/s)")) +
  theme_classic()

Monthly.movement<-Data %>% 
  group_by(Year,Month) %>% 
  summarise(Net = sum(Hnet,na.rm = T))




