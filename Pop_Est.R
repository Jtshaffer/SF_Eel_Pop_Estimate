
library(tidyverse)
library(lubridate)
?tidyverse


Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/Review Data/Review_Data-12-27-22.xlsx"))

names(Data)
str(Data)

summary(Data)


Data<- Data %>% 
  mutate(Hnet = Net * 6) %>% 
  mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>% 
  mutate(Date = ymd_h(Date))



Data %>% 
  filter(Month == 12) %>% 
  ggplot(aes(Date,Hnet))+
  geom_point() +
  theme_classic()



Daily.movement<-Data %>% 
  group_by(Year,Month,Day) %>% 
  summarise(Net = sum(Hnet,na.rm = T))

Daily.movement %>% 
  filter(Month == 12) %>% 
  ggplot(aes(Day,Net))+
  geom_point() +
  theme_classic()

Monthly.movement<-Data %>% 
  group_by(Year,Month) %>% 
  summarise(Net = sum(Hnet,na.rm = T))




