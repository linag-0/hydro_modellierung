#mitteln und gewichten der Meteo-Daten

pfad1 <- "C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/Data/"

pfad86 <- "/S_086/"
pfad56 <- "/S_056/"
pfad57 <- "/S_057/"


#einlesen der daten der drei unterschiedlichen stationen
data_m_86 <- read.csv(file = paste0(pfad1, pfad86 ,"ET_T_N_daily_2006-2019.txt"), skip=2, header=T, sep=";")
data_m_86$Date <-  strptime(data_m_86$Date, format="%Y-%m-%d")

data_m_56 <- read.csv(file = paste0(pfad1, pfad56 ,"ET_T_N_daily_2006-2019.txt"), skip=2, header=T, sep=";")
data_m_56$Date <-  strptime(data_m_56$Date, format="%Y-%m-%d")

data_m_57 <- read.csv(file = paste0(pfad1, pfad57 ,"ET_T_N_daily_2006-2019.txt"), skip=2, header=T, sep=";")
data_m_57$Date <-  strptime(data_m_57$Date, format="%Y-%m-%d")

#erstellen der gewichteten Tabelle, Gewichte wurden per Hand berechnet
data_m <- data_m_56
data_m$ET <- (0.32*data_m_56$ET) + (0.13 * data_m_57$ET) + (0.55 * data_m_86$ET)
data_m$Temp <- (0.32*data_m_56$Temp) + (0.13 * data_m_57$Temp) + (0.55 * data_m_86$Temp)
data_m$N <- (0.32*data_m_56$N) + (0.13 * data_m_57$N) + (0.55 * data_m_86$N)

write.csv(data_m, file = paste0(pfad1, "ET_T_N_daily_2006-2018_brettach_ezg.txt"), row.names = F)


#für die stundenweisen werte
####################
data_nh_86 <- read.csv(file = paste0(pfad1, pfad86 ,"N_hour_2006-2018.txt"), skip=2, header=T, sep=";")
#data_nh_86$Date <-  strptime(data_nh_86$Date, format="%Y-%m-%d")
data_nh_86$Date <-  as.POSIXct(data_nh_86$Date, format= "%Y-%m-%d %H")

data_nh_56 <- read.csv(file = paste0(pfad1, pfad56 ,"N_hour_2006-2018.txt"), skip=2, header=T, sep=";")
#data_nh_56$Date <-  strptime(data_nh_56$Date, format="%Y-%m-%d")
data_nh_56$Date <-  as.POSIXct(data_nh_56$Date, format= "%Y-%m-%d %H")

data_nh_57 <- read.csv(file = paste0(pfad1, pfad57 ,"N_hour_2006-2018.txt"), skip=2, header=T, sep=";")
#data_nh_57$Date <-  strptime(data_nh_57$Date, format="%Y-%m-%d")
data_nh_57$Date <-  as.POSIXct(data_nh_57$Date, format= "%Y-%m-%d %H")

#erstellen der gewichteten Tabelle, Gewichte wurden per Hand berechnet
data_nh <- data_nh_56

data_nh$N <- (0.32*data_nh_56$N) + (0.13 * data_nh_57$N) + (0.55 * data_nh_86$N)

write.csv(data_nh, file = paste0(pfad1, "N_hour_2006-2018_brettach_ezg.txt"), row.names = F)

print(head(data_nh))
