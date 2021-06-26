 install.packages("gplots")
 install.packages("sfsmisc")
 install.packages("MASS")
 install.packages("zoo")
 install.packages("KernSmooth")
 install.packages("grDevices")
 install.packages("hydroGOF")

#rm(list = ls())

### evaluation with your best GAP parameter applied to validation period

## evaluation with best Monte Carlo parameter set - applied to validation period


setwd("C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/HBV-light_2018/Brettach") #In diesem Verzeichnis muss ein "Multi.txt" liegen


#---------------------------------------------------------------------------------------------------
### Read DATA

DATA_GAP 	<- read.table("Results/ResultsbestGAP.txt", dec=".", sep="\t", header=T) ### change name to your results output
#DATA_MC   <- read.table("Results/ResultsbestMC.txt", dec=".", sep="\t", header=T) ### change name to your results output

### check time steps in your DATA files -- do they include validation period (and possibly calibration period)

obs_GAP 	<- DATA_GAP[,3]
#obs_MC    <- DATA_MC[,3]

sim_GAP 	<- DATA_GAP[,2]
#sim_MC    <- DATA_MC[,2]


Date1=as.character(DATA_GAP$Date)
Date2=as.Date(Date1,format='%Y%m%d')

#----------------------------------------------------------------------------------------
## checking Reff / NSE for validation period
library(hydroGOF)

start_cal="20071101"  ### change to your dates
end_cal="20151031"

start_val="20071101"
end_val="20151031"

Data_cal=DATA_GAP[(which(DATA_GAP$Date==start_cal):which(DATA_GAP$Date==end_cal)),]
Data_val=DATA_GAP[(which(DATA_GAP$Date==start_val):which(DATA_GAP$Date==end_val)),]

obs_GAP_cal 	<- Data_cal[,3]
obs_GAP_val   <- Data_val[,3]

sim_GAP_cal 	<- Data_cal[,2]
sim_GAP_val   <- Data_val[,2]


nse_cal=NSE(sim_GAP_cal,obs_GAP_cal)
nse_val=NSE(sim_GAP_val,obs_GAP_val)

#---------------------------------------------------------------------------------------------------
### SCATTER PLOT ###

plot(obs_GAP_val,sim_GAP_val, pch=21, cex=.75, log="", xlim=c(min(sim_GAP_val,obs_GAP_val), max(sim_GAP_val,obs_GAP_val)), ylim=c(min(sim_GAP_val,obs_GAP_val), max(sim_GAP,obs_GAP)), las=1, main="Q(obs) vs. Q(sim)", ylab="Q sim [mm]", xlab="Q obs [mm]", bg="#7f7f7f90", lwd=.5)
grid()
abline(0,1, lwd=2, col="black", lty="dashed")
box(lwd=2)


##density

# check out this video in order to understand kernel density estimation:
#https://www.youtube.com/watch?v=x5zLaWT5KPs&t=231s

## A color palette from blue to yellow to red
library(gplots)
k <- 10
my.cols <- colorpanel(k, "red", "yellow", "blue")
## compute 2D kernel density, see MASS book, pp. 130-131
library(MASS)
z <- kde2d(obs_GAP_val, sim_GAP_val, n=100)
contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=1)

#---------------------------------------------------------------------------------------------------
### RESIDUEN PLOT ###

res <- (sim_GAP_val-obs_GAP_val)/obs_GAP_val
plot(obs_GAP_val, res, ylim=c(-1,1), log="", pch=16, cex=.75, las=1, yaxt="n", ylab="Residuen [%]", xlab="obs Q [mm]")
axis(2, at=c(-1,-0.5,0,0.5,1), labels=c(-1,-0.5,0,0.5,1)*100, las=1)
abline(h=0, col="red")
grid()
box(lwd=2)


#---------------------------------------------------------------------------------------------------
#### Empirical Probability Density Functions for OBs and SIM
library(KernSmooth)
### set max Q
maxQ     <- 15

obs.p		 <- bkde(obs_GAP_val, bandwidth=0.1, range.x=c(0,maxQ), truncate=T, canonical=TRUE)
sim.p		 <- bkde(sim_GAP_val, bandwidth=0.1, range.x=c(0,maxQ), truncate=T, canonical=TRUE)
step	 	<- unique(round(diff(obs.p$x),6))
maxy		 <- max(sim.p$y)

plot(1,1, col="transparent", las=1, xlim=c(0,maxQ),ylim=c(0,0.06), pch=16, type="o", xlab="Dis", ylab="Freq", xaxs="i", yaxs="i")
grid(lty="solid", lwd=0.5)

#polygon(obs.p$x, obs.p$y*step, col="#0000FF40", lwd=2, border="blue")  ## depending on your data polygons do not always work
#polygon(sim.p$x, sim.p$y*step, col="transparent", lwd=2, border="red")

lines(obs.p$x,obs.p$y*step,col="blue",lwd=2)
lines(sim.p$x, sim.p$y*step, col="red",lwd=2)
legend("topright", c("Qobs", "Qsim"), col = c("blue", "red"), lwd = 2)
box(lwd=3)

#---------------------------------------------------------------------------------------------------
#### DQ/DT PLOTS ######
library(sfsmisc)
dq.obs <- c(NA,diff(obs_GAP_val))
dq.sim <- c(NA,diff(sim_GAP_val))

rec <- dq.obs < 0    ## selecting the "recession parts" where Q next day is lower than previous day

plot(obs_GAP_val[rec], -dq.obs[rec], log="xy", pch = 16, las=1, cex=.75, xaxt="n", yaxt="n")
points(obs_GAP_val[rec], -dq.sim[rec], las=1, pch=21, cex=.75, col="red")
grid()
eaxis(1)
eaxis(2)

#----------------------------------------------------------------------------------------
#### Looking into model results file

### call Data_cal and Data_val to do the analysis for Validation and / or calibration period


SWE=DATA_GAP$Snow                       ### snow information
Snowcover=DATA_GAP$Snowcover


SM=DATA_GAP$SM     ## soil moisture

Q0=DATA_GAP$Q0                      ## groundwater information
Q1=DATA_GAP$Q1
Q2=DATA_GAP$Q2
Recharge=DATA_GAP$Recharge


