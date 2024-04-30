Daily_counts<-function(Data= X, twenty_min_file = F, correction = T){
    if(correction == T){
    if(twenty_min_file == T){ 
      Data %>% 
        group_by(Year,Month,Day) %>% 
        summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.intervals=sum(is.na(Hourly.Net))) %>% 
        mutate(day.correction.factor= Missing.intervals/48, Corrected.Daily.Net = round(Daily.Net + (Daily.Net*day.correction.factor))) %>% 
        mutate(Date = paste(Year,Month,Day, sep = "-")) %>% 
        mutate(Date = as.POSIXct(strptime(Date,format = "%Y-%m-%d"))) %>% 
        ungroup()}
    else{
      Data %>% 
        group_by(Year,Month,Day) %>% 
        summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.intervals=sum(is.na(Hourly.Net))) %>% 
        mutate(day.correction.factor= Missing.intervals/24, Corrected.Daily.Net = round(Daily.Net + (Daily.Net*day.correction.factor))) %>% 
        mutate(Date = paste(Year,Month,Day, sep = "-")) %>% 
        mutate(Date = as.POSIXct(strptime(Date,format = "%Y-%m-%d"))) %>% 
        ungroup()}

  }else{
    if(twenty_min_file == T){ 
      Data %>% 
        group_by(Year,Month,Day) %>% 
        summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.intervals=sum(is.na(Hourly.Net))) %>% 
        mutate(Date = paste(Year,Month,Day, sep = "-")) %>% 
        mutate(Date = as.POSIXct(strptime(Date,format = "%Y-%m-%d")))}
    else{
      Data %>% 
        group_by(Year,Month,Day) %>% 
        summarise(Daily.Net= sum(Hourly.Net,na.rm = T), Missing.intervals=sum(is.na(Hourly.Net))) %>% 
        mutate(Date = paste(Year,Month,Day, sep = "-")) %>% 
        mutate(Date = as.POSIXct(strptime(Date,format = "%Y-%m-%d")))}
  }
  
  
}
