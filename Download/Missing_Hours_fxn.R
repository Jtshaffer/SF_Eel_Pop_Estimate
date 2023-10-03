Missing_Hours<- function(Data= Data){
  Data$New.Net<- Data[[ifelse("Net.size.corrected" %in% colnames(Data),"Net.size.corrected","Net")]]
  ind <- which(is.na(Data[[ifelse("Net.corrected" %in% colnames(Data),"Net.size.corrected","Net")]]))
  ind_minus <- ind - 24
  ind_minus[ind_minus < 1] <- NA
  ind_plus <- ind + 24
  ind_plus[ind_plus > nrow(Data)] <- NA
  
  Data[["New.Net"]][ind] <- rowMeans(cbind(Data[["New.Net"]][ind_minus], Data[["New.Net"]][ind_plus]),na.rm = F)
  Data<- Data %>% 
    mutate(Hourly.Net= New.Net*6)
    #select(!New.Net)
}

