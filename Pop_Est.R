
library(tidyverse)
library(lubridate)
library(dataRetrieval)
?tidyverse

#Read in review data and add Date
Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/Review Data/Review_Data-01-13-23.xlsx"))
Data<- Data %>% 
  mutate(Hnet = Net * 6) %>% 
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


Data<-Data %>% 
  group_by(Month,Day) %>% 
  mutate(Daily.count = sum(Hnet,na.rm = T), Monthly.count = sum(Daily.count,na.rm = T))



#Plot the two variables 

scale.factor<- 10


# unction to scale the axis automatically 
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


# Working on developing an inline scaling function that responds to filtered datasets
# inline_scale_fnct<- function(){
#   max_first  <- max(Data$Daily.count)   # Specify max of first y axis
#   max_second <- max(Data$Flow_Inst) # Specify max of second y axis
#   min_first  <- min(Data$Daily.count)   # Specify min of first y axis
#   min_second <- min(Data$Flow_Inst) # Specify min of second y axis
#   scale = (max_second - min_second)/(max_first - min_first)
#   shift = min_first - min_second
#   Flow_Inst = Data$Flow_Inst 
#   inv_scale_function <- function (Flow_Inst, scale, shift) {
#     return ((Flow_Inst + shift)/scale)
#   }
#   inv_scale_function()
# }

# plot <- Data %>%
#   filter(Month == 11) %>% 
#   ggplot(aes(x = Date, y = Daily.count)) +
#   geom_point() +
#   geom_line(aes(y = inline_scale_fnct())) +
#   scale_y_continuous(limits = c(min_first, max_first), sec.axis = sec_axis(~scale_function(., scale, shift)))
# plot
# 




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



