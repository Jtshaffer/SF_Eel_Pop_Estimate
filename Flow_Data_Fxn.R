Flow_data_fxn<- function(siteNo,pCode,start.date,end.date){
  siteNo<- "11476500" # location code for Miranda
  pCode <- "00060"  # Data type: Discharge 
  start.date <- "2022-10-31"
  end.date <- as.character(today())
  
  tmp <- readNWISuv(siteNumbers = siteNo,
                    parameterCd = pCode,
                    startDate = start.date,
                    endDate = end.date)
  tmp<- renameNWISColumns(tmp) 
}
