
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM implementation
#  Serge NABENEZA - JANVIER 2024

library(readxl)
library(moveHMM)
library(dplyr)

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())


#### Importation données classées par animal
gpsSourceDir <- "./1_Data_clean_and_merge/"
GPS_par_anx <- read.table(
  paste0(gpsSourceDir, "HAMAC-SN-GPSpANX.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS_par_anx$DHACQ<-ymd_hms(GPS_par_anx$DHACQ)
head(GPS_par_anx)

#### Calcul des steps et des angles
hmmdata <- prepData(GPS_par_anx, type = "LL",coordNames=c("LON","LAT"))
# Step sort en km pour LL, dépend de l'unité d'entrée en UTM

#### Retrait des derniers outliers
# NA omit
nrow(hmmdata)
# hmmdata <- na.omit(hmmdata)  # Pas certain que ce soit nécessaire, en fait

# Retrait Iridium
dim(GPS_par_anx)
dim(GPS_par_anx[GPS_par_anx$ORI!=T,])
GPS_par_anx<-GPS_par_anx[GPS_par_anx$ORI==T,]
dim(GPS_par_anx)

# Suppression d'outliers sur la vitesse
whichzero <- which(hmmdata$step == 0)
length(whichzero)/nrow(hmmdata)
dim(hmmdata)
dim(hmmdata[hmmdata$step>2.25,]) # nombre de locs > 4,5 km/h
hmmdata<-hmmdata[hmmdata$step<=2.25,]
dim(hmmdata)


#### Assessments visuels
plot(hmmdata, compact=T)

summary(hmmdata$step)
hist(hmmdata$step, xlab = "step length (km)", main = "",breaks = 1000, xlim = c(0,2000),ylim=c(0,100000))

hist(hmmdata$angle, breaks = seq(-pi, pi, length = 15), xlab = "angle", main = "")


#### Première régression

# try 2 states
#stepMean0 <- c(15, 350) # initial means (one for each state)
#stepSD0 <- c(14,340)# initial standard deviations (one for each state)

stepMean0 <-c(50, 300) # initial means (one for each state)
stepSD0 <- c(45,200)
#

#stepMean0 <- c(15, 400) # initial means (one for each state)
#stepSD0 <- c(5,200) # initial standard deviations (one for each state)

stepPar0 <- c(stepMean0, stepSD0)

angleMean0 <- c(pi, 0) # initial means (one for each state)
angleCon0 <- c(1, 10) # initial concentrations (one for each state)
anglePar0 <- c(angleMean0, angleCon0)

m <- fitHMM(data = hmmdata, nbStates = 2, stepPar0 = stepPar0, anglePar0 = anglePar0)
m
#intervalle confiance 95%
CI(m)
plot(m,plotCI=TRUE)

# compute the pseudo-residuals
pr <- pseudoRes(m)

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(m)

#Décodage des états
states <- viterbi(m)
#Etats des 25 premieres observation
states[1:25]

sp <- stateProbs(m)
head(sp)

#Try many starting values

# For reproducibility
set.seed(12345)
# Number of tries with different starting values
niter <- 25
# Save list of fitted models
allm <- list()

for(i in 1:niter) {
  # Step length mean
  stepMean0 <- runif(3,
                     min = c(10, 50,300),
                     max = c(100, 250,1000))
  # Step length standard deviation
  stepSD0 <- runif(3,
                   min = c(10,30,100),
                   max = c(200,200,300))
  # Turning angle mean
  angleMean0 <- c(0, 0,0)
  # Turning angle concentration
  angleCon0 <- runif(3,
                     min = c(0.5, 3,5),
                     max = c(2, 10,15))
  # Fit model
  stepPar0 <- c(stepMean0, stepSD0)
  anglePar0 <- c(angleMean0, angleCon0)
  allm[[i]] <- fitHMM(data = hmmdata, nbStates = 3, stepPar0 = stepPar0,
                      anglePar0 = anglePar0)
}
#"allm" est la liste des 25 modèles, pour extraire le meilleur modèle nous comparons leurs probabilités
#et le plus petit log-likelihood désignera le meilleur modèle
# Extract likelihoods of fitted models
allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))
allnllk
#si les valeurs sont les memes: il y a convergence des modèles et donc c'est un signe de stabilité

# Index of best fitting model (smallest negative log-likelihood)
whichbest <- which.min(allnllk)

# Best fitting model
mbest <- allm[[whichbest]]
mbest