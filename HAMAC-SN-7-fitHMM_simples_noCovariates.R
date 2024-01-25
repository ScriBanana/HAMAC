
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM implementation
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)

### ATTENTION : faire tourner le script Préliminaires_Fits au préalable
# hmmdataFull <- hmmdata
# hmmdata <- hmmdataFull[hmmdataFull$ID == "VSR21" , ]

head(hmmdata)
summary(hmmdata)

#### Premier essai : 2 états
############################

### Paramètres de départ
## Step
stepMean0 <-c(0.050, 0.300) # initial means (one for each state) dans [0, +∞[
stepSD0 <- c(0.045,0.200) # dans [0, +∞[
propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
zeroMass0 <- c(propzero, propzero/100)
# 0.0001 estimation perso (peu de step à zero dans l'état 2 qui est du mouvement)

## Angle
angleMean0 <- c(pi, 0) # initial means (one for each state) dans [-π, π]
angleCon0 <- c(1, 10) # initial concentrations (one for each state) dans [0, +∞[

### Fitting du modèle
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm2Et0Cov <- fitHMM_Log(data = hmmdata, nbStates = 2, stepPar0 = stepPar0, anglePar0 = anglePar0)

### Sorties
## Estimations des maxima de vraisemblance des paramètres
modhmm2Et0Cov
# Sorties de la fonction d'optimisation :
modhmm2Et0Cov$mod
# AIC du modèle :
AIC(modhmm2Et0Cov)

## Intervalle confiance (95%)
CI(modhmm2Et0Cov)
plot(modhmm2Et0Cov, plotCI = TRUE) # Densités de probabilité vs histogrammes
# + prob de transition en fonction des covariables
# + Plot des trajets avec les points de Viterbi (plotTracks = T)

## Etats à chaque point
# A rbinder et à concaténer pour enregistrement et valo ??
# Probabilités locales (! moins bien que Viterbi)
sp <- stateProbs(modhmm2Et0Cov)
head(sp)
# Séquence décodée
states <- viterbi(modhmm2Et0Cov)
states[1:25]
# Plot
plotStates(modhmm2Et0Cov)
plotStates(modhmm2Et0Cov, animals = "VBT11")

## Probabilité de rester dans chaque état en fonction des covariables
# plotStationary(modhmm2Et0Cov, plotCI = T)

# compute the pseudo-residuals
pr <- pseudoRes(modhmm2Et0Cov)

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(modhmm2Et0Cov)



#### Deuxième essai : 3 états
#############################

### Paramètres de départ
## Step
stepMean0 <-c(0.001, 0.2, 0.500) # initial means (one for each state)
stepSD0 <- c(0.05, 0.1, 0.300)
propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
zeroMass0 <- c(propzero, propzero/100, propzero/100)
# 0.0001 estimation perso (peu de step à zero dans l'état 2 qui est du mouvement)

## Angle
angleMean0 <- c(pi, 0, 0) # initial means (one for each state)
angleCon0 <- c(1, 5, 10) # initial concentrations (one for each state)

### Fitting du modèle
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm3Et0Cov <- fitHMM_Log(data = hmmdata, nbStates = 3, stepPar0 = stepPar0, anglePar0 = anglePar0)

### Sorties
## Estimations des maxima de vraisemblance des paramètres
modhmm3Et0Cov
# Sorties de la fonction d'optimisation :
modhmm3Et0Cov$mod
# AIC du modèle :
AIC(modhmm3Et0Cov)

## Intervalle confiance (95%)
CI(modhmm3Et0Cov)
plot(modhmm3Et0Cov,plotCI = TRUE) # Densités de probabilité vs histogrammes
# + prob de transition en fonction des covariables
# + Plot des trajets avec les points de Viterbi (plotTracks = T)

## Etats à chaque point
# A rbinder et à concaténer pour enregistrement et valo ??
# Probabilités locales (! moins bien que Viterbi)
sp <- stateProbs(modhmm3Et0Cov)
head(sp)
# Séquence décodée
states <- viterbi(modhmm3Et0Cov)
states[1:25]
# Plot
plotStates(modhmm3Et0Cov)
plotStates(modhmm3Et0Cov, animals = "VBT11")

## Probabilité de rester dans chaque état en fonction des covariables
plotStationary(modhmm3Et0Cov, plotCI = T)

# compute the pseudo-residuals
pr <- pseudoRes(modhmm3Et0Cov)

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(modhmm3Et0Cov)



#### Comparaison des modèles
############################

AIC(modhmm2Et0Cov, modhmm3Et0Cov)
