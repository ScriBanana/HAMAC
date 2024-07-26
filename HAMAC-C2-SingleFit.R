###################
## HAMAC Routine ##
###################

## Single fit with move HMM
## Serge NABENEZA - JANVIER 2024

### ATTENTION : faire tourner le script Preliminaires_Fits au prealable

### Libraries
library(moveHMM)

### Paths
outDir <- "./2_OutFits"
graphDir <- "./4_VisualOutputs"
filesPrefix <- "/HAMAC-SN-"


### Functions


### Execution

## Selection d'un subset de donnees
subSetType <- 1

switch (subSetType, {
  # None
  print("No subset")
}, {
  # Un animal
  hmmdata <- hmmdata[hmmdata$ID == "VSR21" , ]
}, {
  # Transhumants
  hmmdata <- hmmdata[substr(hmmdata$ID, 3, 3) == "T", ]
}, {
  # Sedentaires
  hmmdata <- hmmdata[substr(hmmdata$ID, 3, 3) == "R", ]
}, {
  # Un subset des donn?es
  hmmdata <- hmmdata[1:10000, ]
})

## Parametres initiaux

nbStates <- 3

switch(nbStates,
       { ### 1 état
         stepMean0 <-c( 0.200) # initial means (one for each state)
         stepSD0 <- c( 0.200)
         propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
         zeroMass0 <- c(propzero)
         ## Angle
         angleMean0 <- c(0) # initial means (one for each state)
         angleCon0 <- c(0.3) # initial concentrations (one for each state)
         
       },
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
         stepMean0 <-c(0.020, 0.200, 0.800) # initial means (one for each state)
         stepSD0 <- c(0.020, 0.200, 0.400)
         propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
         zeroMass0 <- c(propzero, propzero/100, propzero/100)
         
         ## Angle
         angleMean0 <- c(-3.05, 0, 0) # initial means (one for each state)
         angleCon0 <- c(0.3, 0.06, 1.7) # initial concentrations (one for each state)
         
       }
)

#### Covariables
formula =
  ~1
# ~GPS_TMP
# ~AcX + AcY + AcZ
# ~AcX + AcY + AcZ + GPS_TMP
# ~SES
# ~HMS

#### Fit d'un modele
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
modhmm <- fitHMM_Log(data = hmmdata, nbStates = nbStates, formula = formula,
                     stepPar0 = stepPar0, anglePar0 = anglePar0)

#### Intermediate data save
# Done in the fit wrapper function
