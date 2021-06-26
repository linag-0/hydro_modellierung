#
#===================================================================================================
#Dotty plots aus Monte-Carlo-Laeufen fuer alle Parameter mit gewaehlten Likelihood > threshold.y
#===================================================================================================
#rm(list = ls())
#----- Einstellungen  -----


#Arbeitsverzeichnis
setwd("C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/HBV-light_2018/Brettach") #In diesem Verzeichnis muss ein "Multi.txt" liegen

#Pfade festlegen
path <- paste0(getwd(),"/") #Genereller Pfad (== wd)
path.file <- paste0(path,"Results/Multi.txt") #Pfad wo Multi-File liegt
path.plot <- paste0(path,"dottyplots_",format(Sys.time(), "%H%M"),".pdf") #Pfad wohin geplottet wird

#Guetemasse waehlen (muessen exakt so geschrieben sein wie im Header der Multi.txt)
ObjFun <- c("Reff")#,"R2","LogReff")
#Schwellwert setzen, oberhalb dem geplottet werden soll (muss gleiche Laenge wie ObjFun haben)
threshold.y <- c(0.3)#,0.3,0.3)

#Anzahl Zeilen + Spalten im Plotfile festlegen
zeilen <- 4
#spalten <- length(ObjFun)
spalten <- 3

#----- Dotty Plots  -----


#Daten laden
data <- read.table(path.file, sep = "\t", dec = ".", header = T)

data <- select(data, !c('Pelev','Telev','PCALT','TCALT', 'SP_1', 'CFR_1', 'CWH_1'))

#Welche Parameter sind in dem MC-File?
paras <- names(data)[2:(grep("Contribution", names(data))[1]-1)]

#Pdf-Einstellungen
pdf(path.plot, height = 3.33*spalten, width = 2.22*zeilen)
#Plotting Parameter
par(mfrow = c(zeilen,spalten), mar = c(5,4,3,1), oma = c(1,1,1,1))
#Plotting Schleife
for(i in 1:length(paras)){
      for(j in 1:length(ObjFun)){
            #Mache Subset
            dummy <- data.frame(data[,paras[i]], data[,ObjFun[j]])
            names(dummy) <- c(paras[i], ObjFun[j])
            #Diskriminiere mit Schwellwert
            dummy <- dummy[dummy[ObjFun[j]] > threshold.y[j],]
            #Plot
            plot(dummy, cex.lab = 1.25)
            points(dummy[which(dummy[,2] == max(dummy[,2])),], pch = 16, col = "red", cex = 2)
      }
}
dev.off()

