day.adj<-function(Data= X, twenty_min_file = T){
  if(twenty_min_file == T){ 
    Data %>% 
      group_by(Year,Month,Day) %>% 
      summarise(Dailynet= sum(hnet,na.rm = T), missinghrs=sum(is.na(hnet))) %>% 
      mutate(daycorrectionfactor= missinghrs/48, corrected.daily.net = round(Dailynet + (Dailynet*daycorrectionfactor))) %>% 
      ungroup()}
  else{
    Data %>% 
      group_by(Year,Month,Day) %>% 
      summarise(Dailynet= sum(hnet,na.rm = T), missinghrs=sum(is.na(hnet))) %>% 
      mutate(daycorrectionfactor= missinghrs/24, corrected.daily.net = round(Dailynet + (Dailynet*daycorrectionfactor))) %>% 
      ungroup()}

}
