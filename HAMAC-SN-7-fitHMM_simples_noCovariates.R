
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM implementation
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)

### ATTENTION : faire tourner le script Préliminaires_Fits au préalable

head(hmmdata)
summary(hmmdata)

#### Sélection d'un subset de données

# Un animal
hmmdata <- hmmdata[hmmdata$ID == "VSR21" , ]

# Transhumants
hmmdata <- hmmdata[substr(hmmdata$ID, 3, 3) == "T", ]

# Sédentaires
hmmdata <- hmmdata[substr(hmmdata$ID, 3, 3) == "R", ]



#### Param?tres

nbStates <- 3

switch((nbStates - 1),
  { ### Si 2 ?tats :
    
    ## Step
    stepMean0 <-c(0.050, 0.300) # initial means (one for each state) dans [0, +∞[
    stepSD0 <- c(0.045,0.200) # dans [0, +∞[
    propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
    zeroMass0 <- c(propzero, propzero/100)
    # 0.0001 estimation perso (peu de step à zero dans l'état 2 qui est du mouvement)
    
    ## Angle
    angleMean0 <- c(pi, 0) # initial means (one for each state) dans [-π, π]
    angleCon0 <- c(1, 10) # initial concentrations (one for each state) dans [0, +∞[

  }, 
  { ### Si 3 ?tats :
    stepMean0 <-c(0.020, 0.3, 0.900) # initial means (one for each state)
    stepSD0 <- c(0.02, 0.2, 0.500)
    propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
    zeroMass0 <- c(propzero, propzero/100, propzero/100)
    # 0.0001 estimation perso (peu de step à zero dans l'état 2 qui est du mouvement)
    
    ## Angle
    angleMean0 <- c(pi, 0, 0) # initial means (one for each state)
    angleCon0 <- c(1, 5, 5) # initial concentrations (one for each state)

  }
)

### Fitting du modèle
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm <- fitHMM_Log(data = hmmdata, nbStates = nbStates,
                            stepPar0 = stepPar0, anglePar0 = anglePar0)

### Sorties
## Estimations des maxima de vraisemblance des paramètres
modhmm
# Sorties de la fonction d'optimisation :
modhmm$mod
# AIC du modèle :
AIC(modhmm)

## Intervalle confiance (95%)
CI(modhmm)
plot(modhmm, plotCI = TRUE) # Densités de probabilité vs histogrammes
# + prob de transition en fonction des covariables
# + Plot des trajets avec les points de Viterbi (plotTracks = T)

## Etats à chaque point
# A rbinder et à concaténer pour enregistrement et valo ??
# Probabilités locales (! moins bien que Viterbi)
sp <- stateProbs(modhmm)
head(sp)
# Séquence décodée
states <- viterbi(modhmm)
states[1:25]
# Plot
plotStates(modhmm)
plotStates(modhmm, animals = "VBT11")

## Probabilité de rester dans chaque état en fonction des covariables
# plotStationary(modhmm, plotCI = T)

# compute the pseudo-residuals
pr <- pseudoRes(modhmm)

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(modhmm)

