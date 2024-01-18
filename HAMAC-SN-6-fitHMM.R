
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

repDonnees <- "./1_Data_clean_and_merge/"
hmmdata <- read.table(
  paste0(repDonnees, "HAMAC-SN-HMMDATA.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
hmmdata$DHACQ<-ymd_hms(hmmdata$DHACQ)
head(hmmdata)
summary(hmmdata)


#### Premier essai : 2 états

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

#### Deuxième essai : trois states + loop analyse sensi one at a time

# For reproducibility
set.seed(12345)
# Number of tries with different starting values
niter <- 25
# Save list of fitted models
allm <- list()

for(i in 1:niter) {
  # Step length mean
  stepMean0 <- runif(3,
                     min = c(0.01, 0.05,0.300),
                     max = c(0.100, 0.250,1.000))
  # Step length standard deviation
  stepSD0 <- runif(3,
                   min = c(0.010,0.030,0.100),
                   max = c(0.200,0.200,0.300))
  # Turning angle mean
  angleMean0 <- c(0, 0, 0)
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