
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM implementation
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)
library(lubridate)


setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())


#### Importation données sorties de prepData
# NE MARCHE PAS (soucis de format), rerunner prepData

repDonnees <- "./1_Data_clean_and_merge/"
hmmdata <- read.table(
  paste0(repDonnees, "HAMAC-SN-HMMDATA.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
hmmdata$DHACQ<-ymd_hms(hmmdata$DHACQ)
head(hmmdata)
summary(hmmdata)


#### Premier essai : 2 états
############################

### Paramètres de départ
## Step
stepMean0 <-c(0.050, 0.300) # initial means (one for each state)
stepSD0 <- c(0.045,0.200)
whichzero <- which(hmmdata$step == 0)
zeroMass0 <- c(length(whichzero)/nrow(hmmdata), 0.0001)
# 0.0001 estimation perso (peu de step à zero dans l'état 2 qui est du mouvement)

## Angle
angleMean0 <- c(pi, 0) # initial means (one for each state)
angleCon0 <- c(1, 10) # initial concentrations (one for each state)

### Fitting du modèle
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm2etats <- fitHMM(data = hmmdata, nbStates = 2, stepPar0 = stepPar0, anglePar0 = anglePar0)

### Sorties
## Estimations des maxima de vraisemblance des paramètres
modhmm2etats

## Intervalle confiance (95%)
CI(modhmm2etats)
plot(modhmm2etats,plotCI = TRUE) # Densités de probabilité vs histogrammes
# + prob de transition en fonction des covariables

## Etats à chaque point
# Séquence décodée
states <- viterbi(modhmm2etats)
states[1:25]
# A rbinder et à concaténer pour enregistrement et valo ??
# Probabilités locales (! moins bien que Viterbi)
sp <- stateProbs(modhmm2etats)
head(sp)
# Plot
plotStates(modhmm2etats)
plotStates(modhmm2etats, animals = "VBT11")

## Probabilité de rester dans chaque état en fonction des covariables
plotStationary(modhmm2etats, plotCI = T)

# compute the pseudo-residuals
pr <- pseudoRes(modhmm2etats)

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(modhmm2etats)



#### Deuxième essai : 3 états
#############################

### Paramètres de départ
## Step
stepMean0 <-c(0.01, 0.05, 0.300) # initial means (one for each state)
stepSD0 <- c(0.05, 0.1, 0.200)
whichzero <- which(hmmdata$step == 0)
zeroMass0 <- c(length(whichzero)/nrow(hmmdata), 0.0001, 0.0001)
# 0.0001 estimation perso (peu de step à zero dans l'état 2 qui est du mouvement)

## Angle
angleMean0 <- c(0, 0, 0) # initial means (one for each state)
angleCon0 <- c(1, 5, 10) # initial concentrations (one for each state)

### Fitting du modèle
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm3etats <- fitHMM(data = hmmdata, nbStates = 2, stepPar0 = stepPar0, anglePar0 = anglePar0)

### Sorties
## Estimations des maxima de vraisemblance des paramètres
modhmm3etats

## Intervalle confiance (95%)
CI(modhmm3etats)
plot(modhmm3etats,plotCI = TRUE) # Densités de probabilité vs histogrammes
# + prob de transition en fonction des covariables

## Etats à chaque point
# Séquence décodée
states <- viterbi(modhmm3etats)
states[1:25]
# A rbinder et à concaténer pour enregistrement et valo ??
# Probabilités locales (! moins bien que Viterbi)
sp <- stateProbs(modhmm3etats)
head(sp)
# Plot
plotStates(modhmm3etats)
plotStates(modhmm3etats, animals = "VBT11")

## Probabilité de rester dans chaque état en fonction des covariables
plotStationary(modhmm3etats, plotCI = T)

# compute the pseudo-residuals
pr <- pseudoRes(modhmm3etats)

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(modhmm3etats)


#### Comparaison des modèles
############################

AIC(modhmm2etats, modhmm3etats)