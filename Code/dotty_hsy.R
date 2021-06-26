
#===================================================================================================
# Erstellt Simultan Dotty Plots aus Monte-Carlo-Laeufen und HSY-Plots
# Benötigt eine "Multi.txt" als Input (die vom Monte-Carlo im HBV erstellt wird)
#===================================================================================================
#rm(list = ls())

#Arbeitsverzeichnis
setwd("C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/HBV-light_2018/Brettach") #In diesem Verzeichnis muss ein "Multi.txt" liegen

#Pfade
path <- paste0(getwd(),"/")
path.file <- paste0(path,"/Results/Multi.txt")
path.plot <- paste0(path,"dottyHSY_", format(Sys.time(), "%H%M"), ".pdf")

#Guetmass waehlen (nur 1) (Muss exakt so geschrieben sein wie im Header der Multi.txt)
ObjFun <- "Reff"

#Anzahl der HSY-behavioral-runs definieren, um HSY-threshold zu berechnen
anz.behav <- 750

# Threshold/Miniumum fuer Efficiency auf der y-Acse definieren
threshold.y <- 0.3

# -----------------------------------------------------------------------------

#Daten laden
data <- read.table(path.file, header = T, sep = "\t", dec = ".")
#sortieren
data <- data[with(data, order(data[ObjFun], decreasing = T)),]
#welche Parameter sind im File?
n.paras <- 2:(grep("Contribution", names(data))[1]-1)
#Farbgebung fuer Plot
cr	<- c("#4C6CB090","#FF7C5D90")

#pdf Erzeugung starten
pdf(path.plot, height = 5, width = 10)

for(i in n.paras){
      par(mfrow = c(1, 2), mar = c(5, 5, 4, 2), cex.axis = 1.1, cex.lab = 1.2, las = 1)
      
      #Erster Plot: Dotty Plot
      plot(data[1:anz.behav,i], data[[ObjFun]][1:anz.behav], 
           xlab = names(data)[i], ylab = ObjFun, 
           ylim = c(threshold.y, max(data[[ObjFun]])),
           pch = 21, cex = 0.9, las=1, bg=cr[1], lwd=0.5,
           main="Dotty plot mit T fuer behav. runs")
      points(data[1,i], data[[ObjFun]][1],
             pch=22,cex=1.9, lwd=1.8, col="black")
      mtext(paste("best Para. Value  & Eff.=",round(data[1,i],3),", ",round(data[[ObjFun]][1],3)),
            side=3, line=0.2, cex=0.9)
      points(data[anz.behav:length(data[,i]),i], data[[ObjFun]][anz.behav:length(data[,i])],
             pch=21, cex=0.9, bg=cr[2], lwd=0.5)
      abline(h=data[[ObjFun]][anz.behav], 
             col="black", lty="solid", lwd=3)
      mtext(paste("MCs=",nrow(data),", behav=",anz.behav, sep=""), 
            side=4, las=3, line=0.2)
      box(lwd=2)
      
      #KS-Test
      KS_value <- ks.test(data[1:anz.behav,i],data[anz.behav:nrow(data),i])
      
      #create plot-frame for behavioral
      hsy.b <- data.frame(para = data[1:anz.behav,i], 
                          ObjFun = data[[ObjFun]][1:anz.behav], 
                          ObjFun.norm = NA)
      hsy.b <- hsy.b[with(hsy.b, order(hsy.b$para, decreasing = F)),]
      if(min(hsy.b$ObjFun) > 0){
            for(j in 1:anz.behav){ 
                  hsy.b$ObjFun.norm[j] <- hsy.b$ObjFun[j] / sum(hsy.b$ObjFun) 
            }
      } 
      if(min(hsy.b$ObjFun) <= 0){
            for(j in 1:anz.behav){ 
                  hsy.b$ObjFun.norm[j] <- hsy.b$ObjFun[j] - min(hsy.b$ObjFun) 
            }
            dummy <- sum(hsy.b$ObjFun.norm)
            for(j in 1:anz.behav){ 
                  hsy.b$ObjFun.norm[j] <- hsy.b$ObjFun.norm[j] / dummy 
            }
      }
      
      #create plot-frame for non-behavioral
      hsy.nb <- data.frame(para = data[(anz.behav + 1):nrow(data),i], 
                           ObjFun = data[[ObjFun]][(anz.behav + 1):nrow(data)], 
                           ObjFun.norm = NA)
      hsy.nb <- hsy.nb[with(hsy.nb, order(hsy.nb$para, decreasing = F)),]
      if(min(hsy.nb$ObjFun) > 0){
            for(j in 1:(nrow(data)-anz.behav)){ 
                  hsy.nb$ObjFun.norm[j] <- hsy.nb$ObjFun[j] / sum(hsy.nb$ObjFun) 
            }
      } 
      if(min(hsy.nb$ObjFun) <= 0){
            for(j in 1:(nrow(data)-anz.behav)){ 
                  hsy.nb$ObjFun.norm[j] <- hsy.nb$ObjFun[j] - min(hsy.nb$ObjFun) 
            }
            dummy <- sum(hsy.nb$ObjFun.norm)
            for(j in 1:(nrow(data)-anz.behav)){ 
                  hsy.nb$ObjFun.norm[j] <- hsy.nb$ObjFun.norm[j] / dummy 
            }
      }
      
      #Zweiter Plot: HSY-Kurven
      plot(hsy.b$para, cumsum(hsy.b$ObjFun.norm),
           xlim = c(min(data[,i]), max(data[,i])), ylim = c(0,1),
           type="l",col="blue", las=1, xaxs="i", yaxs="i",
           xlab=names(data[i]),ylab="CDF: behav(blue)/non-behav(red)", 
           lwd=2, main=paste(ObjFun,", T: ",round(data[[ObjFun]][anz.behav],3), sep=""))
      grid()
      abline(v=data[1,i],col="black", lwd=1, lty="solid")
      lines(hsy.nb$para, cumsum(hsy.nb$ObjFun.norm),
            col="red", lwd=2.75, lty="dashed")
      lines(c(min(data[,i]),max(data[,i])),c(0,1), lty = 3)
      
      #Legende mit K-Test
      legend("bottomright", cex=0.6,
             legend=c(paste("Two-sample KS Test",sep=""),
                      paste("Dmax:",round(KS_value$statistic,3)),
                      paste("p-value:",round(KS_value$p.value,3))),
             bg=c(ifelse(KS_value$p.value <= 0.05 & 
                               KS_value$statistic <.1,"grey",
                         ifelse(KS_value$p.value <= 0.05 & 
                                      KS_value$statistic >.2,"red",                                
                                ifelse(KS_value$p.value ==0.0000 & 
                                             KS_value$statistic >.2,"red", 
                                       ifelse(KS_value$p.value <= 0.05 & KS_value$statistic >=.1 & 
                                                    KS_value$statistic<=.2,"orange",
                                              ifelse(KS_value$p.value > 0.05,"white","white")
                                       )
                                )
                         )
             ))
      )
      box(lwd=2)
}
dev.off()

