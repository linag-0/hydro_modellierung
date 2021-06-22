#### Calculate potential evapotranspiration based on temperature data
#rm(list=ls())
#load climate time series

#climatedata = read.table('D:/test_climatedata_hydrologicalmodelling.txt',header=TRUE,sep=',')  ### read your file in here

#Date1=as.character(climatedata$Mess_Datum)#
#Date2=as.Date(Date1,format='%Y%m%d')

#months=format(Date2,format='%m')      ### we need monthly mean temperatures in the thornthwaite equation
#years=format(Date2,format='%Y')

#Tempdata=data.frame(Date=Date2,month=months,year=years,Temp=climatedata$LUFTTEMPERATUR)  ###creates table with temperature data 
Tempmonthly <-  data_m %>% 
  mutate(month = month(Date), year = year(Date)) %>% 
  select(Date, month, year, Temp) %>% 
  group_by(month, year) %>% 
  summarise(Temp = mean(Temp))
  
#Tempmonthly=aggregate(Temp~month+year,Tempdata,mean)   ####calculate monthly mean temperatures

################################################################
################################################################
################################################################

#install.packages("SPEI")
require(SPEI)
#install.packages("dplyr")
require(dplyr)

#calculate ETP as monthly time series and take the mean of each month

ETP <- Tempmonthly %>% 
  dplyr::mutate(tho = thornthwaite(Temp, lat = 49.1296)) %>% 
  dplyr::group_by(month) %>% 
  dplyr::summarize(ETP = mean(tho))

#### now it is in mm/month --> we need mm/d for each month

#divide by days per month

days <- c(31,28,31,30,31,30,31,31,30,31,30,31)

ETP$ETPday <- ETP$ETP/days

#export

write.table(ETP, paste0(pfad1,'ETPdata.txt'),row.names=F)

EVAP <- ETP %>% 
  select(Nopex_evap = ETPday)

write_tsv(EVAP, file = paste0(pfad1, "EVAP.txt"))

