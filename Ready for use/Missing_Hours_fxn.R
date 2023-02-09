Missing_Hours<- function(Data= Data){
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
}