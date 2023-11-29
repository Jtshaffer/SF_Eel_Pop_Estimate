Daily_counts<-function(Data= X, twenty_min_file = F, correction = T){
  if(correction == T){
    if(twenty_min_file == T){ 
      Data %>% 
        group_by(Year,Month,Day) %>% 
        summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.intervals=sum(is.na(Hourly.Net))) %>% 
        mutate(day.correction.factor= Missing.intervals/48, Corrected.Daily.Net = round(Daily.Net + (Daily.Net*day.correction.factor))) %>% 
        ungroup()}
    else{
      Data %>% 
        group_by(Year,Month,Day) %>% 
        summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.intervals=sum(is.na(Hourly.Net))) %>% 
        mutate(day.correction.factor= Missing.intervals/24, Corrected.Daily.Net = round(Daily.Net + (Daily.Net*day.correction.factor))) %>% 
        ungroup()}

  }else{
    if(twenty_min_file == T){ 
      Data %>% 
        group_by(Year,Month,Day) %>% 
        summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.intervals=sum(is.na(Hourly.Net)))}
    else{
      Data %>% 
        group_by(Year,Month,Day) %>% 
        summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.intervals=sum(is.na(Hourly.Net)))}
    }
}
