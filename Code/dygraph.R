################################################################################
#
# Read and plot Time series with dygraphs
#
################################################################################


library(dygraphs)
library(xts)

pfad1 <- "C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/"
q_daten <- "Data/04415_Neuenstadt-Brettach_Qh.txt"
meteo <- "C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/Data/S_086/" # Verzeichnis mit N_hour_2006-2018.txt

setwd(pfad1)
data<-read.csv(q_daten,skip=2,sep=";",dec=",",header=F) #für gemessene Daten
colnames(data) <- c("Date", "Q", "Flag") #für gemessene Daten
head(data)

#convert to daily (only with measured data)
data$day <- substr(data$Date, 1,10)
data$Q <- as.numeric(data$Q)
datad <- aggregate(data$Q, list(day = data$day), mean)
head(datad)
datad$day <-  strptime(datad$day, format="%d.%m.%Y")
wo <- which(datad$x < 0)
datad$x[wo] <- NA




# change m3/s to mm/day

area <- 141.472    # EZG Area in km2

datad$x <- datad$x*60*60*24/area/1000

Q <- xts(datad$x, order.by=datad$day)

temp <-order(datad$day)
datad <- datad[temp,]

data_m <- read.csv(file = paste(meteo,"ET_T_N_daily_2006-2019.txt",sep=""), skip=2, header=T, sep=";")
data_m$Date <-  strptime(data_m$Date, format="%Y-%m-%d")

N <- xts(data_m$N, order.by=data_m$Date)
ET <- xts(data_m$ET, order.by=data_m$Date)

t1 <- merge(N, Q, all=FALSE)
t1 <- merge(ET, t1, all=FALSE)

max(N,na.rm = TRUE)

dygraph(t1)  %>% dyRangeSelector()   %>%   dyOptions(stepPlot = TRUE, fillGraph = TRUE, fillAlpha = 0.2) %>% 
  dyAxis("y", label = "Abfluss", valueRange = c(0, 25), independentTicks = TRUE) %>% 
  dyAxis("y2", label = "Niederschlag", valueRange = c(0, 75), independentTicks = TRUE) %>% 
  dySeries('N', axis =('y2'))


#hbv stuff
q_sim_daten <- "HBV-light_2018/Brettach/Results/Results_best_GAP_full.txt"

data <- read_tsv(q_sim_daten)#, skip= 1, dec=".", header = T) #für HBV Output

data$Date <- strptime(data$Date, format = "%Y%m%d") #für HBV Output



datad <- dplyr::select(data,Date : Precipitation)

wo <- which(datad$Qobs < 0)
datad$Qobs[wo] <- NA


Q_sim <- xts(datad$Qsim, order.by = datad$Date)
Q_obs <- xts(datad$Qobs, order.by = datad$Date)
N <- xts(datad$Precipitation, order.by = datad$Date)

t1 <- merge(N, Q_obs, all= FALSE)
t1 <- merge(Q_sim, t1, all = FALSE)

dygraph(t1) %>% dyRangeSelector() %>%   dyOptions(stepPlot = TRUE, fillGraph = TRUE, fillAlpha = 0.2) %>% 
  dyAxis("y", label = "Abfluss", valueRange = c(0, 20), independentTicks = TRUE) %>% 
  dyAxis("y2", label = "Niederschlag", valueRange = c(0, 65), independentTicks = TRUE) %>% 
  dySeries('N', axis =('y2'))

