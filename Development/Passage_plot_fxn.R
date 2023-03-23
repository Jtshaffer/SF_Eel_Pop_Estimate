Passage_plot<- function(Data){
  
  # function to scale the axis automatically 
  max_first  <- max(Data$corrected.daily.net,na.rm = T)   # Specify max of first y axis
  max_second <- max(Data$Flow_Inst,na.rm = T) # Specify max of second y axis
  min_first  <- min(day_adj$corrected.daily.net,na.rm = T)   # Specify min of first y axis
  min_second <- min(Data$Flow_Inst,na.rm = T) # Specify min of second y axis
  
  # scale and shift variables calculated based on desired mins and maxes
  scale = (max_second - min_second)/(max_first - min_first)
  shift = min_first - min_second
  
  # Function to scale secondary axis
  scale_function <- function(x, scale, shift){
    return ((x)*scale - shift)
  }
  
  # Function to scale secondary variable values
  inv_scale_function <- function(x, scale, shift){
    return ((x + shift)/scale)
  }
  
  plot <- Data %>%
    ggplot(aes(x = Date, y = corrected.daily.net)) +
    geom_point(shape = 20,size = 2) +
    geom_line(aes(y = inv_scale_function(Flow_Inst, scale, shift)),col = 'blue') +
    scale_y_continuous(limits = c(min_first, max_first), 
                       sec.axis = sec_axis(~scale_function(., scale, shift),
                                           name = "USGS Miranda gage flow (cfs)"))+
    ylab("Daily passage estimate")
  
  plot
  
}
