
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM implementation
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)

### ATTENTION : faire tourner le script Preliminaires_Fits au prealable

head(hmmdata)
summary(hmmdata)


#### Selection d'un subset de donnees

# Un animal
hmmdata <- hmmdata[hmmdata$ID == "VSR21" , ]

# Transhumants
hmmdata <- hmmdata[substr(hmmdata$ID, 3, 3) == "T", ]

# Sedentaires
hmmdata <- hmmdata[substr(hmmdata$ID, 3, 3) == "R", ]


#### Parametres initiaux

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


#### Fit d'un modele
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm <- fitHMM_Log(data = hmmdata, nbStates = nbStates,
                            stepPar0 = stepPar0, anglePar0 = anglePar0)

