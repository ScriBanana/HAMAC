
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM implementation
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)
library(lubridate)


setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())

outDir <- "./2_Fits_outputs/"
logFile <- "HAMAC-SN-Log.csv"

#### Fit avec le log
if (!file.exists(logFile)) {
  header <- c(
    "timestamp", "nbStates",
    "modhmm$mod$minimum", "AIC", "modhmm$mod$iterations",
    "Fit (état par état) : angleMean, angleCon, stepMean, stepSD, zeroMass évnt."
  )
  write.table(matrix(header, ncol = length(header)),
              file = paste0(outDir, logFile), sep = ";", col.names = FALSE, row.names = FALSE)
}

fitHMM_Log <- function (data, nbStates, stepPar0, anglePar0, logFile) {
  
  timestamp <- Sys.time()
  
  modhmm <- fitHMM(data = data, verbose = 1, nbStates = nbStates, anglePar0 = anglePar0, stepPar0 = stepPar0)
  
  indicateurs <- c(modhmm$mod$minimum, AIC(modhmm), modhmm$mod$iterations)
  anglePar <- c(modhmm$mle$anglePar[1,], modhmm$mle$anglePar[2,])

  if (nrow(modhmm$mle$stepPar) == 2) { # si pas de zeromass
    stepPar <- c(modhmm$mle$stepPar[1,], modhmm$mle$stepPar[2,])
  } else {
    stepPar <- c(modhmm$mle$stepPar[1,], modhmm$mle$stepPar[2,], modhmm$mle$stepPar[3,])
  }

  ligneLog <- c(timestamp, nbStates, indicateurs, anglePar, stepPar)
  
  write.table(
    matrix(ligneLog, ncol = length(ligneLog)), file = logFile,
    sep = ";", col.names = FALSE, row.names = FALSE, append = TRUE
  )
  
  return(modhmm)
}

#### Importation données sorties de prepData
# NE MARCHE PAS (soucis de format), rerunner prepData

repDonnees <- "./1_Data_clean_and_merge/"
hmmdata <- readRDS(paste0(repDonnees,"/HAMAC-SN-HMMDATA.rds"))
head(hmmdata)
summary(hmmdata)

#### Premier essai : 2 états
############################

### Paramètres de départ
## Step
stepMean0 <-c(0.001, 0.300) # initial means (one for each state) dans [0, +∞[
stepSD0 <- c(0.050, 0.200) # dans [0, +∞[
whichzero <- which(hmmdata$step == 0)
zeroMass0 <- c(length(whichzero)/nrow(hmmdata), 0.00001)
# 0.0001 estimation perso (peu de step à zero dans l'état 2 qui est du mouvement)

## Angle
angleMean0 <- c(pi, 0) # initial means (one for each state) dans [-π, π]
angleCon0 <- c(1, 10) # initial concentrations (one for each state) dans [0, +∞[

### Fitting du modèle
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm2Et0Cov <- fitHMM_Log(
    data = hmmdata,
    nbStates = 2, stepPar0 = stepPar0,
    anglePar0 = anglePar0, logFile = paste0(outDir, logFile)
  )

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
whichzero <- which(hmmdata$step == 0)
zeroMass0 <- c(length(whichzero)/nrow(hmmdata), 0.0001, 0.0001)
# 0.0001 estimation perso (peu de step à zero dans l'état 2 qui est du mouvement)

## Angle
angleMean0 <- c(pi, 0, 0) # initial means (one for each state)
angleCon0 <- c(1, 5, 10) # initial concentrations (one for each state)

### Fitting du modèle
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm3Et0Cov <- fitHMM_Log(data = hmmdata, nbStates = 3, stepPar0 = stepPar0,
                           anglePar0 = anglePar0, logFile = paste0(outDir, logFile))

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
