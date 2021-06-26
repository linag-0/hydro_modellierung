### GLUE ANALYSIS 2018 ###
#rm(list = ls())

# change directory
setwd("C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/HBV-light_2018/Brettach") #In diesem Verzeichnis muss ein "Multi.txt" liegen

##=====================================================================================
# preparation for GLUE Analysis: Batch run in HBV
#======================================================================================

ObjFun <- "Reff"   ### choose your objective function ##write exactly as in header multi.txt

#### run a batch run with your best 1000 monte carlo sets -- selection of 1000 best see below:

MC_all <- read.table("Results/multi.txt",header=TRUE,sep="\t")  ## read Monte Carlo file

MC_all_order <- MC_all[with(MC_all, order(MC_all[ObjFun], decreasing = T)),] ##order based on your objective function

MC_best <- MC_all_order[1:1000,]  ##select 1000 best 

write.table(MC_best,"MC_multi_best1000.txt",row.names=F,sep="\t",quote = F) ## write to file that will be used by HBV for Batch run

#### next steps:

## 1. make a copy of this file into your data folder
## 2. rename it "Batch.txt"
## 3. run the Batch tool in HBV


## output of Batch run (Batchrun.txt and BatchQsim.txt) will be used in the rest of the script

#=================================================================================
# ----- settings  -----
#================================================================================

# objective function (bzw. likelihood measure) (Name wie im Batch.txt/Multi.txt-header)
#lik <- "Reff"  ## same as ObjFun above
lik <- ObjFun

#Konfidenzintervall für die Unsicherheitsbänder (einseitig)
conf <- 0.025

#=============================================================================
# ----- Daten laden  -----
#=============================================================================

dt <- read.table("results/BatchRun.txt", header = T, sep = "\t")

#List of objective functions
lik.all <- dt[,27:which(names(dt) == "MeanDiff")]

Batchsim <- read.table("Results/BatchQsim.txt",header=T,sep="\t")  ##in this file, every row is one model run, each column is one day (timestep)

column_names=colnames(Batchsim)

start_val=paste0("X","20121101")  ## fill in here when your validation period starts
index=which(column_names==start_val)

##Batchsim for validation period
Batchsim_val=Batchsim[,index:ncol(Batchsim)]

sim_MC <- Batchsim_val

##remove from memory
rm(dt)
rm(Batchsim)
rm(Batchsim_val)

#Observed time series and simulated best with GAP
dt <- read.table("results/ResultsbestGAP.txt", header = T, sep = "\t") ##change name to your results output with best GAP parameters
obs <- dt[,c(1,3)]
sim_GAP <- dt[,c(1,2)]

###only select validation period #####
start_val_2="20121101"

obs_val=obs[which(obs$Date==start_val_2):nrow(obs),]
sim_GAP_val=sim_GAP[which(sim_GAP$Date==start_val_2):nrow(sim_GAP),]

rm(dt)

# ----- selection of objective function  -----

#extrahieren
lik.calc <- lik.all[,lik]

# Gewichte Normalisieren
w <- (lik.calc) / sum(lik.calc ) 


# ----- prediction quantiles  -----

#Define prediction function of empirical cumulative distribution function
ecdf.pred <- function(perc,x,w){
      #perc = percentile
      #x = values
      #w = weights - sum to 1
      
      # trim x to only have vaules with positive weights
      x <- x[w>0]
      w <- w[w>0]
      
      # form the empricial cdf
      sort.x <- sort(x,index=TRUE)
      ecdf <- cumsum(w[sort.x$ix])
      
      # calculate the percentiles
      out <- rep(NA,length(perc))
      for(ii in 1:length(perc)){
            jj <- which.min(abs(ecdf-perc[ii]))
            flag <- TRUE
            while(flag == TRUE){
                  if(perc[ii] <= 0.5){
                        if(ecdf[jj] > perc[ii]){
                              jj <- jj-1
                        }else{
                              flag <- FALSE
                        }
                  }else{
                        if(ecdf[jj] < perc[ii]){
                              jj <- jj+1
                        }else{
                              flag <- FALSE
                        }
                  }
            }
            out[ii] <- sort.x$x[jj]
      }
      return(out)
}

#calculate prediction quantiles
sim_MC.ptile <- matrix(NA,ncol(sim_MC),2)
sim_MC.median <- rep(NA,ncol(sim_MC))
for(ii in 1:ncol(sim_MC)){
      out <- ecdf.pred(c(conf,1-conf,0.5),sim_MC[,ii],w)  
      sim_MC.ptile[ii,] <- out[1:2]
      sim_MC.median[ii] <- out[3]
}


#=====================================================================================
# ----- plot  -----
#====================================================================================


# Define minimum and maximum t for x-axis (t in days)
minT <- 1   
maxT <- 365*1

# maxQ für ausreichend y-axis im plot()
maxQ <- max(sim_MC.ptile[(minT:maxT),2], na.rm=T)*1.05

arg1 <- paste("Median (red) and 95% GLUE prediction limits with best 1000 MC based on",ObjFun)

#Oberes Ende des Unsicherheitsbands
plot(minT:maxT,sim_MC.ptile[minT:maxT,2], type="l", lty=2, lwd=1, ylim=c(0, maxQ),
     col="white", main=arg1, las = 1,
     xlab="time", ylab="Q_SIM, Q_OBS & 95% CI [mm/d]")
####
polygon(c((minT:maxT),rev(minT:maxT)),c(sim_MC.ptile[minT:maxT,1], rev(sim_MC.ptile[minT:maxT,2])), col = "gray85", border = NA) 
#Unteres Ende des Unsicherheitsbands
points(minT:maxT,sim_MC.ptile[minT:maxT,1],type="l",lty=2, lwd=1, col=c("black"))
points(minT:maxT,sim_MC.ptile[minT:maxT,2],type="l",lty=2, lwd=1, col=c("black"))

#Median des Bands
lines(minT:maxT,sim_MC.median[minT:maxT],col="red", lwd=2)
#Observierte Werte
lines(minT:maxT,obs_val$Qobs[minT:maxT], col = "blue")
#GAP-Lauf
lines(minT:maxT,sim_GAP_val$Qsim[minT:maxT], col = "orange")

legend(x= 210, y = 11,legend=c("Qsim GAP","Qobs","Qmed GLUE","GLUE uncertainty bands"),col=c("orange","blue","red","gray85"),lwd=c(2,2,2,18),bty="n")

legend(x="topleft",legend="2012",bty="n",cex=1.5)  ### change this to the year(s) you are showing, or alternatively change the xaxis, based on minT and maxT also

