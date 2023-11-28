day.adj<-function(Data= X, twenty_min_file = F, census = F){
  if(twenty_min_file == T){ 
    Data %>% 
      group_by(Year,Month,Day) %>% 
      summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.hrs=sum(is.na(Hourly.Net))) %>% 
      mutate(day.correction.factor= Missing.hrs/48, Corrected.Daily.Net = round(Daily.Net + (Daily.Net*day.correction.factor))) %>% 
      ungroup()}
  else if (census == T){
    Data %>% 
      group_by(Year,Month,Day) %>% 
      summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.hrs=sum(is.na(Hourly.Net))) %>% 
      ungroup()}
  else{
    Data %>% 
      group_by(Year,Month,Day) %>% 
      summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.hrs=sum(is.na(Hourly.Net))) %>% 
      mutate(day.correction.factor= Missing.hrs/24, Corrected.Daily.Net = round(Daily.Net + (Daily.Net*day.correction.factor))) %>% 
      ungroup()}

}
