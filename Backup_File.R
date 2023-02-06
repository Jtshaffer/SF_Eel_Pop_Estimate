
# Backup of the scrip to produce the population abundance estimates for the south fork eel didson station. JTS 2023
Data<- as.data.frame(readxl::read_xlsx("E:/CalTrout/SF_Eel_Didson/Review Data/Review_Data-01-13-23.xlsx"))
Data<- Data %>% 
  #mutate(Hnet = Net * 6) %>% 
  mutate(Date = paste(Year,Month,Day,Hour, sep = "-")) %>% 
  mutate(Date = ymd_h(Date))


Data$lessthan40<- apply(Data[8:27],1,function(x){  # Function to remove the counts that are less than 40cm and correct the net counts
  sum(x < 40,na.rm = T)
}
)    

Data<- Data %>% 
  mutate(Net.corrected = Net - lessthan40)  

## Accounting for single hours down ----

Data$New.Net<- Data[["Net.corrected"]]
ind <- which(is.na(Data[["Net.corrected"]]))
ind_minus <- ind - 24
ind_minus[ind_minus < 1] <- NA
ind_plus <- ind + 24
ind_plus[ind_plus > nrow(Data)] <- NA

Data[["New.Net"]][ind] <- rowMeans(cbind(Data[["New.Net"]][ind_minus], Data[["New.Net"]][ind_plus]),
                                   na.rm = TRUE)


Data<- Data %>% 
  mutate(hnet= New.Net*6) 


#Adding in correction factor and daily net movement and then correcting the daily net movement
day.adj<- Data %>% 
  group_by(Year,Month,Day) %>% 
  summarise(Dailynet= sum(hnet,na.rm = T), missinghrs=sum(is.na(hnet))) %>% 
  mutate(daycorrectionfactor= missinghrs/24, corrected.day.net = Dailynet + (Dailynet*daycorrectionfactor)) %>% 
  ungroup()


#Adding in the monthly correction factor and monthly net movement then correcting the monthly movement
days.per.month<- day.adj %>% 
  group_by(Month) %>% 
  summarise(length(Day))


month.adj<- day.adj %>% 
  group_by(Month) %>% 
  summarise(monthlynet= sum(corrected.day.net,na.rm = T), missing_day_vals= n_distinct(which(corrected.day.net == 0.00))) %>% 
  mutate(monthcorrectionfactor= missing_day_vals/days.per.month[2], cor.month.net= round(monthlynet + (monthlynet*monthcorrectionfactor ))) %>% 
  ungroup()


