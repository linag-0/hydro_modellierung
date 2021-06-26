# -------------------------------------------------------------------------------
#  Gruppe 3: Basisabflusstrennung: BFI (base flow index)
#  Einzugsgebietshydrologie
# 
# 
#  Dunja Powroschnik und Philipp Zurmoehle, November 2020
# -------------------------------------------------------------------------------

#  Kommentare zum Ausfuehren des Codes:

#  Hauptpfad und Abflussdaten eingeben in Zeile 310 u. 316
#  Parametereingabe ab Zeile: 383 
#  Fuer stuendliche BFI-Berechnung siehe Zeile 449

# -------------------------------------------------------------------------------
# Funktionen 
# -------------------------------------------------------------------------------
   
# trotz fehlender Werte ausfuehren
    meanNA <- function(x){mean(x, na.rm = TRUE)}
    minNA <- function(x){min(x, na.rm = TRUE)}
    maxNA <- function(x){max(x, na.rm = TRUE)}
    
# Wie viele NA's?
    sumMissing <- function(x){sum(is.na(x))} 
    
# Ursprung auf 1970-01-01 setzen    
    as.date <- function(x, origin='1970-01-01'){as.numeric(x, origin=origin)}
    
    
# BFI Funktionen
# -------------------------------------------------------------------------------
# Basisabflusstrennung und Berechnung der Basisabflusslinie
# -------------------------------------------------------------------------------
  
  # Schritt 1: Tageszeitreihe (Q [m³/s]) in gleich große Bloecke von ws Tagen (window size) unterteilen

  calcbaseline <- function(datad,ws,tpf){
  
  datad$ws <- NA
  mindate <- NULL
  
    i=1
    while (i <= nrow(datad) - (ws-1)) { # Fuer jeden Block neue Spalte "ws" mit Startdatum des Blocks 
     temp <- data.frame(matrix(NA, nrow = ws, ncol = 2)) # temporaere Matrix "temp" fuer jeden Block 
     colnames(temp) <- c("date","q")
    for (j in 1:ws) {
      temp$date[j] <- datad$date[i+j-1]
      temp$q[j] <- datad$q[i+j-1]
      datad$ws[i+j-1] <- datad$date[i] 
    }
    tempmin <- temp$date[which.min(temp$q)] # zugehoeriges Datum der Minima fuer jeden Block finden
    mindate <- c(mindate, tempmin) # Vektor aller Minima und zugehoerigen Daten erstellen
    i <- i + ws
  }
  
  mindate <- as.date(mindate) 
  
  # Minima ueber die ws Tagesbloecke aggregieren
  datad$ws <- as.date(datad$ws)
  dataws <- aggregate(q ~ ws, minNA, data = datad)
  colnames(dataws) <- c("startws","qmin")
  dataws$mindate <- mindate
  
  # Schritt 2: Turning points definieren
  # wenn tpf*qmin <= angrenzende qmins ist, dann wird qmin zu einem Turning point
  # Bereich [2:end-1] festlegen, da kein qmin[i-1] fuer i=1 und kein qmin[i+1] fuer i=nrow erhalten werden kann
  
  turnpoints <- NULL  
  
  for (i in 2:(nrow(dataws)-1)){
    if (tpf*dataws$qmin[i] <= dataws$qmin[i-1] &&
        tpf*dataws$qmin[i] <= dataws$qmin[i+1]){
      
      tp <- c(dataws$startws[i], dataws$mindate[i], dataws$qmin[i])
      names(tp) <- c("startws", "mindate", "qt")
      turnpoints <- rbind(turnpoints, tp)    
    } 
  } 
  
  turnpoints <- as.data.frame(turnpoints)
  turnpoints$startws <- as.date(turnpoints$startws)
  turnpoints$mindate <- as.date(turnpoints$mindate)
  
  # Schritt 3: Lineare Interpolation fuer Basisabflusswerte zwischen Turning Points
  
  baseline <- approx(turnpoints$mindate, turnpoints$qt,
                     xout = datad$date, method = "linear")
  baseline <- as.data.frame(baseline)
  colnames(baseline) <- c("date", "qbf")
  
  # Bedingungen festlegen: qbf = qd, wenn qbf > qd
  for (i in 1 :(nrow(baseline))){
    if(is.na(baseline$qbf[i]) | is.na(datad$q[i])) next
    if (baseline$qbf[i] > datad$q[i]){
      baseline$qbf[i] <- datad$q[i]
    }
    
  }
  
  return(baseline)
}

    


# -------------------------------------------------------------------------------------------------------
# Berechnung BFI
# -------------------------------------------------------------------------------------------------------

calcbfi <- function(datad,baseline,start,end){
 
  a <- which(baseline$date == start)# Zeilen mit NA am Anfang und Ende des Dataframes ueberspringen
  while (is.na(baseline$qbf[a])){
    a <- a+1
  }
  
  z <- which(baseline$date == end)
  while (is.na(baseline$qbf[z])){
    z <- z-1
  }
  
  if (z < a){
    print("Error: Ende von calcbfi vor dem Start. Anderes Zeitintervall waehlen.")
    return(NA)
    break
  }
  
  # Integrieren des Basisabflussvolumens in m3
  
  vb <- sum(60*60*24*baseline$qbf[a:z], na.rm = T)
  
  # print(paste0("The base-flow volume from ", baseline$date[a] ," to ", baseline$date[z], " is:"))
  # print(paste0(sprintf("%.2e", vb)," m3"))
  
  # Integrieren des Gesamtabflussvolumens in m3
  
  vt <-  sum(60*60*24*datad$q[a:z], na.rm = T)
  
  # print(paste0("The total-flow volume from ", datad$date[a] ," to ", datad$date[z], " is:"))
  # print(paste0(sprintf("%.2e", vt)," m3"))
  
  # BFI
  bfi <- vb/vt
  # print(paste0("The base-flow index from ", datad$date[a]," to ", datad$date[z], " is:"))
  # print(sprintf("%.3f", bfi))
  return(bfi)
}

# -------------------------------------------------------------------------------------------------------
# Berechnung BFI fuer verschiedene Zeitintervalle
# -------------------------------------------------------------------------------------------------------
    
  bfiinterval <- function(datad, baseline, start, end, mode){ 
 
  # mode entweder "year" oder "month"
  # Liste mit dem ersten Tag jedes Intervalls
  liststart <- seq(from = start, to = end , by = mode)
  
  # Liste mit dem letzten Tag jedes Intervalls
  listend <- NULL
  for (i in 1:(length(liststart)-1)){
    
    listend <- append(listend, liststart[i+1]-1)
  }
  
  listend <- append(listend, end)
  
  # Liste mit BFI fuer jedes Intervall 
  listbfi <- NULL
  for (i in 1:length(liststart)){
    listbfi <- append(listbfi, 
                      calcbfi(datad, baseline, liststart[i], listend[i]))
  }
  
  bfiinterval <- data.frame(liststart,listbfi)
  colnames(bfiinterval) <- c("date","bfi")
  
  return(bfiinterval)
}



# Plot Funktionen    
# -------------------------------------------------------------------------------------------------------
# Plot Abflussganglinie und Basisabflusslinie (dygraph) fuer Zeitreihen
# -------------------------------------------------------------------------------------------------------
    
    plotdygraph <- function(datad, baseline){
      Qbf <- xts(baseline$qbf, order.by = baseline$date)
      Q <- xts(datad$q, order.by = datad$date)
      
      # merge
      t1 <- merge(Qbf, Q, all = T ) 
      
      # Plot  
      return(
        dygraph(t1, main = "Abflussganglinie und Basisabflusslinie")  %>% 
          dyAxis("y", label = "Q [m3/s]", independentTicks = T) %>%
          dySeries("Qbf", axis = "y",fillGraph = T, color = "red") %>%
          dyRangeSelector()   %>%   
          dyOptions(stepPlot = T, connectSeparatedPoints = T, 
                    fillGraph = T, fillAlpha = 0.2)
      )
      
      
    }
    

    
    
# -------------------------------------------------------------------------------------------------------
# Plot BFI fuer gewaehltes Zeitintervall
# -------------------------------------------------------------------------------------------------------
      
      plotbfi <- function(bfiinterval, mode, ezg){
      #  mode = "jaehrlicher" oder "monatlicher" 
      start <- bfiinterval$date[1]
      end <- bfiinterval$date[nrow(bfiinterval)]
      print(nrow(bfiinterval))
      tit = paste0(mode, " BFI \n von ",
                   start, " bis ", end, " (", ezg, ")")
      
      par(xpd = T, mar = c(5,5,5,3))
      plot(bfiinterval$date, bfiinterval$bfi,
           main = tit, xlab = "Datum", ylab = "BFI",
           col = "forestgreen", type = "s", lwd = 2.0, ylim = c(0.0,1.0))
    }
    
    
   
# -------------------------------------------------------------------------------------------------------
# Plot Berechnen und zeichnen durchschnittl. monatl. BFI von Zeitintervall
# ------------------------------------------------------------------------------------------------------- 
      
    bfimonav <- function(bfimonth, ezg){
               # Zeitintervall von bfimonth
                start <- bfimonth$date[1]
                end <- bfimonth$date[nrow(bfimonth)]
            
            # Aggregieren von BFI zu Monat ueber Jahre
                bfimonth$month <- as.numeric(format(bfimonth$date, "%m"))
                bfimonav <- aggregate(bfi~month, meanNA, data = bfimonth)
                
                tit = paste0("Durchschnittl. monatl. BFI \n von ",
                             start, " bis ", end, " (", ezg, ")")
                par(xpd = T, mar = c(5, 5, 5, 3))
                plot(bfimonav$month, bfimonav$bfi,
                     main = tit, xlab = "Monat", ylab = "BFI",
                     col = "forestgreen", type = "s", lwd = 2.0, ylim = c(0.0, 1.0))
                
                return(bfimonav)
    }
        
        

    

# -------------------------------------------------------------------------------------------------------
# Plot Abflussganglinie und Basisabflusslinie fuer gewaehltes Zeitintervall
# -------------------------------------------------------------------------------------------------------
      plotbaseline <- function(datad,baseline,start,end,ezg){  
    
      bfi <- calcbfi(datad,baseline,start,end)
      
      # Datenrahmen auf ausgewaehltes Zeitintervall zuschneiden     
     # datad <- datad[(which(datad$date == start)):(which(datad$date == end)),]
    #  baseline <- baseline[(which(baseline$date == start)):(which(baseline$date == end)),]
      
      # Zeilen mit NA am Anfang und Ende des Datenrahmens ueberspringen  
      a <- 1 #which(baseline$date == start)
      while (is.na(baseline$qbf[a])){
        a <- a+1
      }
      
      z <- length(baseline$date) #which(baseline$date == end)
      while (is.na(baseline$qbf[z])){
        z <- z-1
      }
      
      
      tit = paste0("Abflussganglinie und Basisabflusslinie \n von ",
                   datad$date[a], " bis ", datad$date[z], " (", ezg, ")")
      par(xpd = T, mar = c(5,5,5,5))
      plot(datad$date, datad$q,
           main = tit, xlab = "Datum", ylab = "Q [m3/s]",
           col = "blue", type = "l", lwd = 1.0)
      
      lines(baseline$date, baseline$qbf, col = "red", lwd = 1.0)
      
      # show bfi value for time interval in legend    
      legend("topright",
             legend = c("Q", "Qbf", paste0("BFI = ", sprintf("%.2f", bfi))),
             col = c("blue", "red", "white"), lwd = c(1,1,0), lty = c(1,1,0) ,
             cex = 0.85, xpd = T)
    }   
          
 
    
# -------------------------------------------------------------------------------
# Pakete laden
# -------------------------------------------------------------------------------
    library(dygraphs)
    library(xts)
    library(stats)
    
# -------------------------------------------------------------------------------
# Hauptpfad
# -------------------------------------------------------------------------------

# hauptpfad <- "D:/Uni/EZG_Hydrologie/Mein_EZG/"
# ergebnispfad <- paste0(hauptpfad, "Ergebnisse/")
    
hauptpfad <- "C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/HBV-light_2018/Brettach/"
    ergebnispfad <- "C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/Ergebnisse/"
    
# -------------------------------------------------------------------------------
# Daten importieren und aufbereiten
# -------------------------------------------------------------------------------
    
     importpfad <- paste0(hauptpfad, "Results/Results.txt")
    data <- read.table(importpfad,  header = TRUE, sep = "\t", 
        dec = ".", stringsAsFactors = FALSE) 
  colnames(data) <- c("datetime", "q", "qobs")
    
    # head(data)
    # mode(data$datetime)
    
    

# Zeit in Datumsformat
  data$date<- as.POSIXct(strptime(data$datetime, format = "%Y%m%d",
    tz="GMT"), tz="GMT")  
 

# mode(data$date) # numeric
# class(data$date) # POSIXct
  
# -------------------------------------------------------------------------------
# Tests
# -------------------------------------------------------------------------------  

# Fehlende Werte? stuendlich

wo <- which(data$q < 0)
data$q[wo] <- NA
        
apply(data, 2, sumMissing) 

# Fehlende Stunden? Datetime in Sekunden seit origin. Dividieren durch 3600s fuer Stunden
        diffh <- c(1, diff(as.numeric(data$date)))
        print("Highest number of continuously missing hours:")
        print(maxNA(diffh/3600)-1) 

        
# -------------------------------------------------------------------------------
# Daten auf Tageswerte aggregieren und erneut testen
# -------------------------------------------------------------------------------        
  
  data$days <- format(data$date, "%Y-%m-%d") 
  #datad <- aggregate(q ~ days, meanNA, data = data) #  Voreinstellungen na.action = na.omit
  
  #datad <- aggregate(q ~ days, meanNA, data = data, na.action = na.pass) #  fuehrt aggregate aus, ohne Zeilen 
                                                                           #  mit NAs zu loeschen, damit Spalte mit 
                                                                           #  stuendlichen BFIs spaeter angehaengt werden kann
  datad <- dplyr::select(data, "date",  'q'='qobs')     
  
  datad$date <- as.Date(as.character(datad$date))
  
  #wo <- which(datad$q < 0)
  #datad$q[wo] <- NA
  

  #head(datad)
  #mode(datad$days) # character
  
  
  # days ins Datumsformat 
  #datad$date <- as.Date(datad$days, format = "%Y-%m-%d")
  
  # mode(datad$date)# numeric
  # class(datad$date)# Date
  
  
  # Fehlende Tage?
    datad$differenz <- c(1, diff(datad$date))
    minNA(datad$differenz)
    maxNA(datad$differenz)    # die Tage an denen keine Daten (NA) vorhanden sind, 
                              # werden bei dem Schritt Aggregieren zu Tagesdaten geloescht.
                              
    

# -------------------------------------------------------------------------------
# Parameter definieren
# -------------------------------------------------------------------------------

#  Einzugsgebiet
ezg <- "Brettach"

# Beispiel window size 5 Tage (WMO Standard)
ws <- 5  

# turning point Faktor, Standardwert der WMO (tpf=0.9)
tpf <- 0.9

# Start und Enddatum fuer Abflussganglinie und Basisabflusslinie und fuer BFI Berechnungen    
start <- as.Date("2005-11-01")

# Enddatum muss vor dem letzten Monat der Zeitreihe enden (fuer die Funktion "bfiinterval")  
end <- as.Date("2018-10-23") 

attributes(baseline$date)
# -------------------------------------------------------------------------------
# Funktionen ausfuehren
# -------------------------------------------------------------------------------

# Basisabflusswerte fuer jeden Tag
    
    baseline <- calcbaseline(datad,ws,tpf)
    head(baseline,20)
    
# Plot Ganglinie und Basisabflusslinie (dygraph) 
    
    plotdygraph(datad,baseline)
    
# Plot Ganglinie und Basisabflusslinie fuer gewaehltes Zeitintervall  
    baseline$date <- as.Date(as.character(baseline$date))#, format="%Y-%m-%d")
  #  attr(baseline$date, "tzone") <- attributes(start)$tzone
    
    pdf(paste0(ergebnispfad, "Abflussganglinie_Basisabfluss_qobs.pdf"))
  #  par(mfrow = c(1,1))
    plotbaseline(datad,baseline,start,end,ezg)
    dev.off()
    
# Baseflow index (BFI) fuer gewaehltes Zeitintervall 
    bfi <- calcbfi(datad,baseline,start,end)
    print(paste0("BFI zwischen ", start," to ", end, " is:"))
    print(sprintf("%.3f", bfi))

# Baseflow index (BFI) fuer jeden Monat/jedes Jahr   
    bfimonth <- bfiinterval(datad, baseline, start, end , mode = "month")
    bfimonth
    bfiyear <- bfiinterval(datad, baseline, start, end, mode = "year")
    bfiyear

# Plots fuer Baseflow index (BFI) fuer jeden Monat/jedes Jahr  
    png(paste0(ergebnispfad, "BFI_monatlich_84-19.png"))
    plotbfi(bfimonth, mode = "monatlicher",ezg)
    dev.off()
    
    png(paste0(ergebnispfad, "BFI_jaehrlich_84-19.png"))
    plotbfi(bfiyear,mode = "jaehrlicher",ezg)
    dev.off()

# Plot des monatlichen Baseflow index (BFI) fuer ein Zeitintervall    
    png(paste0(ergebnispfad, "Durchschnitt_Monatlicher_BFI_Zeitintervall.png"))
    bfimonav(bfimonth,ezg)
    dev.off()

    
#  neue Tabelle "datah" mit Basisabflusswerten pro Stunde erstellen (Ausgangswert fuer andere Analysen)
    
    # Hierfuer bitte Code ab Zeile 357 ausfuehren (vorher bitte alle Plots in anderen Ordner speichern, sonst 
    # werden diese ueberschrieben)
   # datad$bfiday <- (baseline$qbf/datad$q)
   # data$bfih <- rep(datad$bfiday, each = 24)
    data$bfih <- rep(datad$qbf, each = 24)
    data_baseline_h <- subset(data, select=c(date,bfih))
    
    head( data)
    View(datah)
  
