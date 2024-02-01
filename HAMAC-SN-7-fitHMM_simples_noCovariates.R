
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


################################################################################
### Sorties

## Charger un RDS
cheminSorties <- "D:/USERS/SergeEtArthur/WorkspaceR/hamac/2_Fits_outputs/"
modhmm <- readRDS(paste0(cheminSorties ,"240127193238-HAMAC-SN-ModHMM-3Et.rds"))

## Estimations des maxima de vraisemblance des paramètres
modhmm
# Sorties de la fonction d'optimisation :
modhmm$mod
# AIC du modèle :
AIC(modhmm)

## Intervalle confiance (95%)
CI(modhmm)
plot(modhmm, plotCI = TRUE, ask = FALSE) # Densités de probabilité vs histogrammes
# + prob de transition en fonction des covariables
# + Plot des trajets avec les points de Viterbi (plotTracks = T)


# Enregistrement des distributions en PDF
repSauvegardes <- "./2_Fits_outputs/"
pdf(paste0(repSauvegardes, "Out_Graphs/", format(Sys.time(), format = "%y%m%d%H%M%S"), '-PlotModHMM.pdf'),
    width = 8, height = 10,
    colormodel = "cmyk",
    paper = "A4")
par(mfrow = c(2, 1))
plot(modhmm, plotCI = TRUE, ask = FALSE)
plotPR(modhmm)
dev.off()


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


#### Manipulations sur le modèle

# Ajoute aux données une colonne avec les états selon Viterbi
vit <- viterbi(modhmm)
hmmdatavit <- modhmm$data %>% mutate(VIT = vit)



################################################################################
#### Comparaison de mod�les


repSauvegardes <- "./2_Fits_outputs/"
meilleurModele2e <- readRDS(paste0(repSauvegardes,"240129181847-HAMAC-SN-ModHMM-2Et.rds"))
meilleurModele3e <- readRDS(paste0(repSauvegardes,"240127202639-HAMAC-SN-ModHMM-3Et.rds"))
AIC(meilleurModele2e, meilleurModele3e)
AIC2 <- AIC(meilleurModele2e)
AIC3 <- AIC(meilleurModele3e)
exp((AIC3-AIC2)/2)
