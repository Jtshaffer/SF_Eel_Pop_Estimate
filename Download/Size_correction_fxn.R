Size_correction<- function(Data = Data){
  Data$lessthan40<- apply(Data[8:27],1,function(x){  
    sum(x < 40,na.rm = T)
  })
  
  Data<- Data %>% 
    mutate(Net.size.corrected = Net - lessthan40)  
}
