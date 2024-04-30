Hourly_counts<-  function(Data= Data,interpolate= F,twenty_min_file =F){
 Data$New.Net<- Data[[ifelse("Net.size.corrected" %in% colnames(Data),"Net.size.corrected","Net")]]
  if(interpolate == T) {
    ind <- which(is.na(Data[[ifelse("Net.corrected" %in% colnames(Data),"Net.size.corrected","Net")]]))
    ind_minus <- ind - 24
    ind_minus[ind_minus < 1] <- NA
    ind_plus <- ind + 24
    ind_plus[ind_plus > nrow(Data)] <- NA
    Data[["New.Net"]][ind] <- rowMeans(cbind(Data[["New.Net"]][ind_minus], Data[["New.Net"]][ind_plus]),na.rm = F)
    if(twenty_min_file == T){
      Data %>% 
        group_by(Month,Day,Year,Hour,Date) %>% 
        summarise(Hourly.Net= floor(sum(New.Net)*3))
    }else{
      Data %>% 
        group_by(Month,Day,Year,Hour,Date) %>% 
        summarise(Hourly.Net= floor(sum(New.Net)*6))
    }
  }else{
     if(twenty_min_file == T){
       Data %>% 
       group_by(Month,Day,Year,Hour,Date) %>% 
       summarise(Hourly.Net= floor(sum(New.Net)*3))
     }else{
       Data %>% 
         group_by(Month,Day,Year,Hour,Date) %>% 
         summarise(Hourly.Net= floor(sum(New.Net)*6))
     }
   }
 }
