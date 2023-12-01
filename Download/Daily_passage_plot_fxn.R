#Daily Passage Plot

daily_passage_plot<- function(Data1= XYZ, Data2= ZYX, DateBreaks = "1 day" ){
  Data2<-Data2 %>% 
    mutate(Date= as.POSIXct(Date))
  
  # function to scale the axis automatically 
  max_first  <- max(Data2$Daily.Net,na.rm = T) +1   # Specify max of first y axis
  max_second <- max(Data1$Flow_Inst,na.rm = T) +5 # Specify max of second y axis
  min_first  <- min(Data2$Daily.Net,na.rm = T) -1  # Specify min of first y axis
  min_second <- min(Data1$Flow_Inst,na.rm = T)  -15 # Specify min of second y axis
  
  
  # scale and shift variables calculated based on desired mins and maxes
  scale = (max_second - min_second)/(max_first - min_first)
  shift = (min_first) - (min_second) 
  
  # Function to scale secondary axis
  scale_function <- function(x, scale, shift){
    return ((x)*scale - shift)
  }
  
  # Function to scale secondary variable values
  inv_scale_function <- function(x, scale, shift){
    return ((x + shift)/scale)
  }
  
  
  plot <- ggplot() +
    #geom_point(shape = 20,size = 2) +
    #stat_summary_bin( fun = "median", colour = "red", size = 2, geom = "point",bins = 4)+
    geom_line(aes(x =Data1$dateTime, y = inv_scale_function(Data1$Flow_Inst, scale, shift), color ='blue')) +
    scale_y_continuous(limits = c(min_first, max_first),
                       sec.axis = sec_axis(~scale_function(., scale, shift),
                                           name = "USGS Miranda gage flow (cfs)"))+
    scale_x_datetime(date_breaks = DateBreaks ,date_labels = "%b %d" )+
    # geom_point(data= Data2, aes(x=Date , y = Daily.Net), shape = 0,size = 2) +
    geom_point(aes(x=Data2$Date , y = Data2$Daily.Net), shape = 18,size = 1.75) +
    # geom_text(data = Data2, aes(x = Date, y = Daily.Net, label = Daily.Net),nudge_y = 25, ) +
    # geom_text_repel(data = Data2, aes(x = Date, y = Daily.Net, label = Daily.Net) ) +
    ylab("Daily passage estimate (fish > 40 cm)")+
    xlab("Date")+
    scale_color_manual(name = "",
                       breaks = c("blue"),
                       labels = c("USGS Miranda gage flow (cfs)"),
                       values = c("blue" = "blue"))+
    theme(legend.position = "bottom",
          plot.background = element_rect(inherit.blank = T),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linewidth = 1,linetype = 1,
                                      fill = NA),
          axis.text.x = element_text(angle = 45, vjust = .5))
  
  plot
  
}
