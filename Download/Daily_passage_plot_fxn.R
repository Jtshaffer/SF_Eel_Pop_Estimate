#Daily Passage Plot
daily_passage_plot<- function(Data1= XYZ, Data2= ZYX, DateBreaks = "1 day", dual_axis = T,startdate="2023-10-10", enddate=today(),first.y.adj = 100, sec.y.adj = 100){
  if (dual_axis==T) {
  
  Data1<-Data1 %>% 
    mutate(Date= as.Date(Date,format = "%Y-%m-%d")) %>% 
    select(Date,Flow_Inst)
  
  Data2<-Data2 %>% 
    mutate(Date= as.Date(Date,format = "%Y-%m-%d")) 
    
  
  Data<- left_join(Data1,Data2) %>% 
    mutate(size = 1.75) 
  
  Data$Corrected.Daily.Net <- ifelse(Data$Missing.intervals == 48,NA,Data$Corrected.Daily.Net)

  max <- as.Date(startdate,format = "%Y-%m-%d")
  min <- as.Date(enddate,format = "%Y-%m-%d")
  
  # function to scale the axis automatically 
  max_first  <- max(Data$Daily.Net,na.rm = T) + first.y.adj# Specify max of first y axis
  max_second <- max(Data$Flow_Inst,na.rm = T) + sec.y.adj # Specify max of second y axis
  min_first  <- min(Data$Daily.Net,na.rm = T) -1  # Specify min of first y axis
  min_second <- min(Data$Flow_Inst,na.rm = T)  -15 # Specify min of second y axis
  
  
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
  
  
  plot <- Data %>% 
    ggplot() +
    geom_point(aes(x=Date, y = Corrected.Daily.Net,size=size)) +
    geom_line(aes(x =Date, y = inv_scale_function(Flow_Inst, scale, shift),color ='blue')) +
    # geom_smooth(aes(x =Date, y = inv_scale_function(Flow_Inst, scale, shift),color ='blue'),se = F, span = 0.1) +
    scale_y_continuous(limits = c(min_first, max_first),
                       sec.axis = sec_axis(~scale_function(., scale, shift),
                                           name = "USGS Miranda Gage Flow (cfs)"))+
    scale_x_date(date_breaks =  DateBreaks ,date_labels = "%b %d",limits = c(max, min))+
    # geom_point(data= Data2, aes(x=Date , y = Daily.Net), shape = 0,size = 2) +
    # geom_text(data = Data2, aes(x = Date, y = Daily.Net, label = Daily.Net),nudge_y = 25, ) +
    # geom_text_repel(data = Data2, aes(x = Date, y = Daily.Net, label = Daily.Net) ) +
    ylab("Daily Passage Estimate (fish > 40 cm)")+
    xlab("Date")+
    scale_color_manual(name = "",
                       breaks = c("blue"),
                       labels = c("USGS Miranda Gage Flow (cfs)"),
                       values = c("blue" = "blue"))+
    scale_size(name = "",
               range = c(1.75),
               labels = c("Daily Passage Estimate (fish > 40 cm)"))+
    theme(legend.position = "bottom",
          plot.background = element_rect(inherit.blank = T),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linewidth = 1,linetype = 1,
                                      fill = NA),
          axis.text.x = element_text(angle = 45,size = 14,vjust = 1.25,hjust = 1.2),
          plot.title = element_text( size=16, face="bold.italic"),
          axis.text = element_text( size=10),
          axis.title.x = element_text( size=12, face="bold"),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.title.y = element_text( size=14, face="bold"),
          strip.text.x = element_text(size = 10, face="bold"),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
  
  plot
  
  
  }else{
    Data1<-Data1 %>% 
      mutate(Date= as.Date(Date,format = "%Y-%m-%d")) %>% 
      select(Date,Flow_Inst)
    Data2<-Data2 %>% 
      mutate(Date= as.Date(Date,format = "%Y-%m-%d")) 
    max <- as.Date(startdate,format = "%Y-%m-%d")
    min <- as.Date(enddate,format = "%Y-%m-%d")
    
    plot1 <- ggplot() +
      geom_line(data = Data1, aes(x =Date, y = Flow_Inst), color ='blue')+
      # scale_x_datetime(date_breaks = DateBreaks ,date_labels = "%b %d",limits = c(max, min))+
      scale_x_date(date_breaks = DateBreaks ,date_labels = "%b %d",limits = c(max, min))+
      xlab("")+
      ylab("USGS Miranda gage flow (cfs)")+
       theme(legend.position = "bottom",
          plot.background = element_rect(inherit.blank = T),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linewidth = 1,linetype = 1,
                                      fill = NA),
          axis.text.x = element_text(angle = 55, vjust = .5,size = 14),
          plot.title = element_text( size=16, face="bold.italic"),
          axis.text = element_text( size=10),
          axis.title.x = element_text( size=12, face="bold"),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.title.y = element_text( size=14, face="bold"),
          strip.text.x = element_text(size = 10, face="bold"))
    
    plot2<- ggplot()+
      geom_point(data = Data2, aes(x=Date, y = Corrected.Daily.Net ),size = 1.75)+
      scale_x_date(date_breaks = DateBreaks ,date_labels = "%b %d", limits = c(max, min))+
      xlab("Date")+
      ylab("Daily passage estimate (fish > 40 cm)")+
      theme(legend.position = "bottom",
            plot.background = element_rect(inherit.blank = T),
            panel.background = element_rect(fill = NA),
            panel.border = element_rect(linewidth = 1,linetype = 1,
                                        fill = NA),
            axis.text.x = element_text(angle = 55, vjust = .5,size = 14),
            plot.title = element_text( size=16, face="bold.italic"),
            axis.text = element_text( size=10),
            axis.title.x = element_text( size=12, face="bold"),
            axis.text.y = element_text(size = 14, angle = 0, hjust = 0.5),
            axis.title.y = element_text( size=14, face="bold"),
            strip.text.x = element_text(size = 10, face="bold"))
      
    fin_plot<- plot1/plot2
    
    fin_plot
 }
}



