##### script to plot (relative) parameter ranges GAP optimization #####

#rm(list=ls())
#####  set working directory
setwd("C:/Users/linag/Desktop/Uni/Hydrologische_Modellierung/HBV-light_2018/Brettach")
#######

##uncomment the option that you used
#==========================
## option 1: 5 runs 1 by 1
#==========================
 Gap_file_1=read.table("Results/GA_best1.txt",header=TRUE,sep="\t")  ## fill in the names of the different GAP files how you renamed them
# Gap_file_2=read.table("Results/......txt",header=TRUE,sep="\t")  
# Gap_file_3=read.table("Results/......txt",header=TRUE,sep="\t")  
# Gap_file_4=read.table("Results/......txt",header=TRUE,sep="\t")  
# Gap_file_5=read.table("Results/......txt",header=TRUE,sep="\t")  

#==============================
## option 2: calibrate 5 times
#==============================
Gap_file=read.table("Results/GA_best1.txt",header=TRUE,sep="\t")  ## each row shows the results of 5 calibrations (only best/last saved)

#===================================
## option 3: create 5 populations
#===================================
# Gap_file_1=read.table("Results/GA_best1.txt",header=TRUE,sep="\t")  ## Each file represents one population
# Gap_file_2=read.table("Results/GA_best2.txt",header=TRUE,sep="\t")  
# Gap_file_3=read.table("Results/GA_best3.txt",header=TRUE,sep="\t")  
# Gap_file_4=read.table("Results/GA_best4.txt",header=TRUE,sep="\t")  
# Gap_file_5=read.table("Results/GA_best5.txt",header=TRUE,sep="\t")  


########################################################################
########################################################################
#===========================================================================
# Parameter ranges GAP optimization
#===========================================================================
#### get the values from your GAP settings screen

##PERC
Perc_min=0  ### change values according to your settings
Perc_max=4

##UZL
UZL_min=0
UZL_max=70
  
##K0
K0_min=0.1
K0_max=0.5
  
##K1
K1_min=0.01
K1_max=0.3
  
##K2
K2_min=0.00005
K2_max=0.1
  
##MAXBAS
MAXBAS_min=1
MAXBAS_max=2.5

##TT
TT_min=-2
TT_max=0.5

##CFMAX
CFMAX_min=0.5
CFMAX_max=4

##SFCF
SFCF_min=0.5
SFCF_max=1.3

##FC
FC_min=100
FC_max=500

##LP
LP_min=0.3
LP_max=1

##BETA
BETA_min=1
BETA_max=5

  
###optional (default fixed parameters)

##SP

##CFR

##CWH


###########################################################################
##########################################################################
#======================================================================
# standardizing the parameter values found in the GAP calibration
#======================================================================

#### uncomment your option

#option 1 and option 3
#--------------------------------------------------------------------------
# Perc_1=Gap_file_1$PERC_1[nrow(Gap_file_1)]   ### last row contains the best parameter set after .. runs
# Perc_1_st=(Perc_1-Perc_min)/(Perc_max-Perc_min)
# 
# Perc_2=Gap_file_2$PERC_1[nrow(Gap_file_2)]
# Perc_2_st=(Perc_2-Perc_min)/(Perc_max-Perc_min)
# 
# Perc_3=Gap_file_3$PERC_1[nrow(Gap_file_3)]
# Perc_3_st=(Perc_3-Perc_min)/(Perc_max-Perc_min)
# 
# Perc_4=Gap_file_4$PERC_1[nrow(Gap_file_4)]
# Perc_4_st=(Perc_4-Perc_min)/(Perc_max-Perc_min)
# 
# Perc_5=Gap_file_5$PERC_1[nrow(Gap_file_5)]
# Perc_5_st=(Perc_5-Perc_min)/(Perc_max-Perc_min)

#####

## etc for all the other parameters


#option 2
#---------------------------------------------------------------------------
Perc_1=Gap_file$PERC_1[1]
Perc_1_st=(Perc_1-Perc_min)/(Perc_max-Perc_min)

Perc_2=Gap_file$PERC_1[2]
Perc_2_st=(Perc_2-Perc_min)/(Perc_max-Perc_min)

Perc_3=Gap_file$PERC_1[3]
Perc_3_st=(Perc_3-Perc_min)/(Perc_max-Perc_min)

Perc_4=Gap_file$PERC_1[4]
Perc_4_st=(Perc_4-Perc_min)/(Perc_max-Perc_min)

Perc_5=Gap_file$PERC_1[5]
Perc_5_st=(Perc_5-Perc_min)/(Perc_max-Perc_min)

###
UZL_1=Gap_file$UZL_1[1]
UZL_1_st=(UZL_1-UZL_min)/(UZL_max-UZL_min)

UZL_2=Gap_file$UZL_1[2]
UZL_2_st=(UZL_2-UZL_min)/(UZL_max-UZL_min)

UZL_3=Gap_file$UZL_1[3]
UZL_3_st=(UZL_3-UZL_min)/(UZL_max-UZL_min)

UZL_4=Gap_file$UZL_1[4]
UZL_4_st=(UZL_4-UZL_min)/(UZL_max-UZL_min)

UZL_5=Gap_file$UZL_1[5]
UZL_5_st=(UZL_5-UZL_min)/(UZL_max-UZL_min)

K0_st <- (Gap_file$K0_1 -K0_min)/(K0_max-K0_min)

K1_st <- (Gap_file$K1_1 -K1_min)/(K1_max-K1_min)

K2_st <- (Gap_file$K2_1 -K2_min)/(K2_max-K2_min)

MAXBAS_st <- (Gap_file$MAXBAS_1 - MAXBAS_min ) / (MAXBAS_max- MAXBAS_min)

TT_st <- (Gap_file$TT_1_1 - TT_min ) / (TT_max- TT_min)

CFMAX_st <- (Gap_file$CFMAX_1_1 - CFMAX_min ) / (CFMAX_max- CFMAX_min)

SFCF_st <- (Gap_file$SFCF_1_1 - SFCF_min ) / (SFCF_max- SFCF_min)

FC_st <- (Gap_file$FC_1_1 - FC_min ) / (FC_max- FC_min)

LP_st <- (Gap_file$LP_1_1 - LP_min ) / (LP_max- LP_min)

BETA_st <- (Gap_file$BETA_1_1 - BETA_min ) / (BETA_max- BETA_min)

### etc for the other parameters




##########################################################################
#========================================================================
# plotting
#========================================================================

plot(1, type="n", xlab="", ylab="Parameter-range (standardized)", xlim=c(0, 13), ylim=c(0, 1),xaxt="n")
axis(1,at=0:11,labels=c("PERC","UZL","K0","K1","K2","MAXBAS","TT","CFMAX","SFCF","FC","LP","Beta"),las=2)
mtext("Parameters",side=1,line=4)

##PERC
points(0,Perc_1_st,pch=21,col="black",bg="cyan",cex=1.2)
points(0,Perc_2_st,pch=21,col="black",bg="cyan",cex=1.2)
points(0,Perc_3_st,pch=21,col="black",bg="cyan",cex=1.2)
points(0,Perc_4_st,pch=21,col="black",bg="cyan",cex=1.2)
points(0,Perc_5_st,pch=21,col="black",bg="cyan",cex=1.2)

##UZL
points(1,UZL_1_st,pch=21,col="black",bg="cyan",cex=1.2)
points(1,UZL_2_st,pch=21,col="black",bg="cyan",cex=1.2)
points(1,UZL_3_st,pch=21,col="black",bg="cyan",cex=1.2)
points(1,UZL_4_st,pch=21,col="black",bg="cyan",cex=1.2)
points(1,UZL_5_st,pch=21,col="black",bg="cyan",cex=1.2)

#K0
points(rep(2, length(K0_st)),K0_st, pch=21, col="black", bg = "cyan", cex=1.2)

#K1
points(rep(3, length(K1_st)),K1_st, pch=21, col="black", bg = "cyan", cex=1.2)

#K2
points(rep(4, length(K2_st)),K2_st, pch=21, col="black", bg = "cyan", cex=1.2)

#MAXBAS
points(rep(5, length(MAXBAS_st)),MAXBAS_st, pch=21, col="black", bg = "cyan", cex=1.2)

#TT
points(rep(6, length(TT_st)),TT_st, pch=21, col="black", bg = "cyan", cex=1.2)

#CFMAX
points(rep(7, length(CFMAX_st)),CFMAX_st, pch=21, col="black", bg = "cyan", cex=1.2)

#SFCF
points(rep(8, length(SFCF_st)),SFCF_st, pch=21, col="black", bg = "cyan", cex=1.2)

#FC
points(rep(9, length(FC_st)),FC_st, pch=21, col="black", bg = "cyan", cex=1.2)

#LP
points(rep(10, length(LP_st)),LP_st, pch=21, col="black", bg = "cyan", cex=1.2)

#BETA
points(rep(11, length(BETA_st)),BETA_st, pch=21, col="black", bg = "cyan", cex=1.2)

