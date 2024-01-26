
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM parameter space exploration
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)

### ATTENTION : faire tourner le script Préliminaires_Fits au préalable

head(hmmdata)
summary(hmmdata)


################################################################################
#### Définition des fonctions
fitWithParam <- function(initial_params) { # Simplifie les calls
  fitHMM_Log(data = hmmdata, nbStates = nbStates,
             stepPar0 = c(initial_params$stepMean0,
                          initial_params$stepSD0,
                          initial_params$zeroMass0),
             anglePar0 = c(initial_params$angleMean0,
                           initial_params$angleCon0))
}


################################################################################
#### Paramètres
nJxParamInit <- 44
nThreads <- 22
nbStates <- 2

# Génère les jeux de paramètres initiaux. Remplir où indiqué
generate_initial_params <- function() {
  if (length(which(hmmdata$step == 0)) != 0 ) {
    propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
  } else {
    propzero <- 0
  }
  
  list(
    stepMean0 = runif(nbStates, # Ici :
                      min = c(0.01, 0.05, 0.300),
                      max = c(0.100, 0.250, 0.400)),
    stepSD0 = runif(nbStates, # Là :
                    min = c(0.010, 0.030, 0.100),
                    max = c(0.200, 0.200, 0.300)),
    angleMean0 = rep(0, nbStates),
    angleCon0 = runif(nbStates, # Et là :
                      min = c(0.5, 3,5),
                      max = c(2, 10,15)),
    zeroMass0 = c(propzero, rep(propzero/100, nbStates - 1))
  )
}

################################################################################
#### Execution

# Génère les jeux de paramètres aléatoires
initial_params_list <- map(1:nJxParamInit, ~ generate_initial_params())

# Fait tourner fitHMM_Log sur chaque jeu
result_list <- initial_params_list %>% map(fitWithParam)


################################################################################
#### Enregistrement



################################################################################
#### Sorties

