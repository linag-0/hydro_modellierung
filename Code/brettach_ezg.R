#-------------------------------------------------------------------------------
#hier alle grundsätzlichen datensatzanpassungen und funktionsausführungen
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#libraries
#-------------------------------------------------------------------------------
library(dygraphs)
library(xts)
library(ggplot2)
library(tidyverse)
library(lubridate)


#-------------------------------------------------------------------------------
#pfade
#-------------------------------------------------------------------------------
pfad1 <- "C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/Data/"
q_daten <- "04415_Neuenstadt-Brettach_Qh.txt"

pfad_erg <- paste0(pfad1, "/Ergebnisse/")


#-------------------------------------------------------------------------------
#q-Daten einlesen
#-------------------------------------------------------------------------------
setwd(pfad1)
data_raw<-read.csv(q_daten,skip=2,sep=";",dec=",",header=F)
colnames(data_raw) <- c("Date", "Q", "Flag") 

data_raw$Q <- as.numeric(data_raw$Q)
datah <- dplyr::select(data_raw, Date, Q)
datah$Date <- as.POSIXct(datah$Date, format = "%d.%m.%Y %H:%M")

#head(datah)

#-------------------------------------------------------------------------------
# convert to daily
#-------------------------------------------------------------------------------

data_raw$day <- substr(data_raw$Date, 1,10)
data_raw$Q <- as.numeric(data_raw$Q)
datad <- aggregate(data_raw$Q, list(day = data_raw$day), mean)
head(datad)
datad$day <-  strptime(datad$day, format="%d.%m.%Y")


  #-------------------------------------------------------------------------------
  #change q<0 to NA
  #-------------------------------------------------------------------------------
#  wo <- which(datad$x < 0)
#  datad$x[wo] <- NA

  
  #-------------------------------------------------------------------------------
  # change m3/s to mm/day
  #-------------------------------------------------------------------------------
  
  area <- 141.472    # EZG Area in km2
  
  datad$x <- datad$x*60*60*24/area/1000
  
  write.csv(datad, file = paste0(pfad1, "Abfluss_Neuenstadt_Brettach_taeglich.csv"))
  
#datad: tägliche abflussdaten, falsche daten zu NAs, Q in mm/d  
  
#######
#hier was zum Herausfinden �ber Qualit�t der Daten
########
  max(data_m$N)

  
#-------------------------------------------
#make the table with n t and q  
  
  head(left_join(data_m, datad, by = c('Date'='day')))

ptq <- left_join(data_m, datad, by = c('Date'='day')) %>% 
  mutate(Date = str_replace_all(Date, '-', '')) %>% 
  select(date = Date, 'Prec.'=N, Temp, Qobs = x)
  
head(data_m)

write_tsv(ptq, file = paste0(pfad1, "ptq.txt"))

#-------------------------------------------------------------------------------
#monatlicher mittelwert der et �ber alle jahre
#-------------------------------------------------------------------------------
EVAP_akt <- data_m %>% 
  mutate(year = year(Date), month = month(Date)) %>% 
  group_by(month) %>% 
  summarise(Nopex_evap = mean(ET)) %>% 
  select(!month)

#head(EVAP)

write_tsv(EVAP_akt, file = paste0(pfad1, "EVAP_akt.txt"))

#-------------------------------------------------------------------------------
#wasserbilanz checken
#-------------------------------------------------------------------------------
library(lubridate)
yearly_wabi <- inner_join(data_m, datad, by = c('Date'='day'))  %>% 
  group_by(year(Date)) %>% #diese Zeile auskommentieren f�r gesamte WaBi
  summarise(ETa = sum(ET, na.rm = T), N = sum(N, na.rm = T), Q = sum(x, na.rm=T)) %>% 
  mutate(dS = N-ETa-Q ,Error = ((N-ETa-Q)/N)*100)
  


#-------------------------------------------------------------------------------
#dygraph über alle jahre
#-------------------------------------------------------------------------------
t_q <- xts(datad$x, order.by=datad$day)

temp <-order(datad$day)
datad <- datad[temp,]


t_n <- xts(data_m$N, order.by=data_m$Date)
t_e <- xts(data_m$ET, order.by=data_m$Date)

t1 <- merge(t_n, t_q, all=FALSE)
t1 <- merge(t_e, t1, all=FALSE)

dygraph(t1)  %>% dyRangeSelector()   %>%   dyOptions(stepPlot = TRUE, fillGraph = TRUE, fillAlpha = 0.2)


#-------------------------------------------------------------------------------
#dauerlinie, summenkurve etc. (gruppe 1)
#-------------------------------------------------------------------------------

datad$q <- datad$x


fdc(datad, pfad_erg, "Dauerlinie 2006-2018")


fdc_years(datad, pfad_erg, "Dauerlinie Jahresweise 2006-2018")

sumcum_Q <- sumcumQ(datad_day)
head(sumcum_Q)

plot_sumcum(sumcum_Q, pfad_erg, "Kumulative Summe 2006-2018")

cov_Line(sumcum_Q, 2015, pfad_erg, "Abflussschwerpunkt 2015")


cov_Line(sumcum_Q, 2003, pfad_erg, "Abflussschwerpunkt 2003")
cov_Line(sumcum_Q, 1990, pfad_erg, "Abflussschwerpunkt 1990")
cov_Line(sumcum_Q, 2001, pfad_erg, "Abflussschwerpunkt 2001")
#-------------------------------------------------------------------------------
#speicheränderung (gruppe 2)
#-------------------------------------------------------------------------------
#innerhalb von deren dokument gemacht

#-------------------------------------------------------------------------------
#basisabflussabtrennung (gruppe 3)
#-------------------------------------------------------------------------------
#Ausführen der Datei Gruppe3...
#basisabfluss heißt: baseline$qbf (!NA Werte!) bzw stundenwenrte: data_baseline_h$bfih

data_baseline_h$bfih[which(is.na(data_baseline_h$bfih))] <- minNA(data_baseline_h$bfih)
#Tabellen: data_nh hat stuendl Nschlag, data_baseline_h hat stuendl base flow, datah hat stuendl Q

datah$Qd <- datah$Q-data_baseline_h$bfih


yeardatah <- filter(datah, year(Date) == 2015)
yeardata_nh <- filter(data_nh, year(Date) == 2015)
yeardata_baseline_h <- filter(data_baseline_h, year(date) == 2015)
yeardata_baseline_h <- yeardata_baseline_h[-8760,]

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#FEHLT: Umrechnung Q in mm/h
yeardatah$Q <- ((yeardatah$Q*60*60*1000) / (area*1000000) )
yeardatah$Qd <- ((yeardatah$Qd*60*60*1000) / (area*1000000) )

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
head(yeardatah)

yeardatah$date <- yeardatah$Date
yeardata_nh$date <- yeardata_nh$Date

events <- eventsep(yeardatah$Q, yeardatah$Qd, yeardata_baseline_h$bfih, area)

koeffiz <- koeff(events$Start, events$Ende, yeardatah$Q, yeardata_nh$N)

pdf(paste0(ergebnispfad, "Abflussereignisse.pdf"))

plot_events(events, yeardatah, yeardata_nh )

dev.off()

pdf(paste0(ergebnispfad, "Abflusskoeffizient_Jahreszeit.pdf"))

boxplot_season(yeardatah$Date, koeffiz )

dev.off()

#--------------------------------------------
#für 2012-2017
#--------------------------------------------

yeardatah <- filter(datah, year(Date) %in% 2012:2017)
yeardata_nh <- filter(data_nh, year(Date) %in% 2012:2017)
yeardata_baseline_h <- filter(data_baseline_h, year(Date) %in% 2012:2017)
#yeardata_baseline_h <- yeardata_baseline_h[-8760,]

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#FEHLT: Umrechnung Q in mm/h
yeardatah$Q <- ((yeardatah$Q*60*60*1000) / (area*1000000) )
yeardatah$Qd <- ((yeardatah$Qd*60*60*1000) / (area*1000000) )

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
head(yeardatah)

yeardatah$date <- yeardatah$Date
yeardata_nh$date <- yeardata_nh$Date

events <- eventsep(yeardatah$Q, yeardatah$Qd, yeardata_baseline_h$bfih, area)

koeffiz <- koeff(events$Start, events$Ende, yeardatah$Q, yeardata_nh$N)

pdf(paste0(ergebnispfad, "Abflussereignisse_12-16.pdf"))

plot_events(events, yeardatah, yeardata_nh )

dev.off()

pdf(paste0(ergebnispfad, "Abflusskoeffizient_Jahreszeit12-16.pdf"))

boxplot_season(yeardatah$Date, koeffiz )

dev.off()


#-------------------------------------------------------------------------------
#Gruppe 5
#-------------------------------------------------------------------------------
#in deren dokument

#gruppe 6 und 7 auch


#-------------------------------------------------------------------------------
#abschließende berechnungen
#-------------------------------------------------------------------------------
#durchschnittlicher jahresniederschlag
head(data_m)
data_m$year <- year(data_m$Date)
meteo_yearly <- data_m %>%
       group_by( year) %>% 
       filter(year != 2006 && year != 2018) %>% 
       summarize(N_Jahr =sum(N))
tail(meteo_yearly)

meteo_avg <- summarize(meteo_yearly, mean(N_Jahr))

head(datad)

datad$year <- year(datad$day)
flow_yearly <- datad %>%
       group_by( year) %>% 
       filter(year != 1982 && year != 2018) %>% 
       summarize(Flow =sum(q))

flow_avg <- summarize(flow_yearly, mean(Flow))
