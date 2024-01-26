
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM parameter space exploration
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)
library(purrr)
library(furrr)
library(ggplot2)

### ATTENTION : faire tourner le script PrÃ©liminaires_Fits au prÃ©alable

head(hmmdata)
summary(hmmdata)


################################################################################
#### DÃ©finition des fonctions
fitWithParam <- function(initial_params) { # Simplifie les calls
  fitHMM_Log(data = hmmdata, nbStates = nbStates,
             stepPar0 = c(initial_params$stepMean0,
                          initial_params$stepSD0,
                          initial_params$zeroMass0),
             anglePar0 = c(initial_params$angleMean0,
                           initial_params$angleCon0))
}


################################################################################
#### ParamÃ¨tres
nJxParamInit <- 22
nThreads <- 44
nbStates <- 2

# GÃ©nÃ¨re les jeux de paramÃ¨tres initiaux. Remplir oÃ¹ indiquÃ©
generate_initial_params <- function() {
  
  propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)

  list(
    stepMean0 = runif(nbStates, # Ici :
                      min = c(0.010, 0.050, 0.300),
                      max = c(0.100, 0.250, 1.000)),
    stepSD0 = runif(nbStates, # Là :
                    min = c(0.010, 0.030, 0.100),
                    max = c(0.200, 0.200, 0.300)),
    angleMean0 = rep(0, nbStates),
    angleCon0 = runif(nbStates, # Et là :
                      min = c(0.5, 3, 5),
                      max = c(2, 10, 15)),
    zeroMass0 = c(propzero, rep(propzero/100, nbStates - 1))
  )
}

################################################################################
#### Execution

# Génère les jeux de paramètres alÃ©atoires
initial_params_list <- map(1:nJxParamInit, ~ generate_initial_params())

#### Début parallelisation sur nThreads threads
print(paste0("Lancement de la boucle : ", date()))
tpsDebut <- Sys.time()
plan(multisession, workers = nThreads)

# Fait tourner fitHMM_Log sur chaque jeu
modhmmList <- initial_params_list %>% future_map(fitWithParam)

plan(sequential) # Fin parallelisation
print(paste0("Fin des calculs : ", date()))
print(Sys.time() - tpsDebut)


################################################################################
#### Enregistrement



################################################################################
#### Sorties

# Tu peux décommenter les lignes pour explorer la donnée (ctrl + maj + c)
# modhmmList %>% map("mle")
# modhmmList %>% map("mle") %>% map("stepPar")
# modhmmList %>% map("mod") %>% map("minimum")
plot(unlist(modhmmList %>% map("mod") %>% map("minimum")),
     xlab = "Id du jeu de données",
     ylab = "Maximum likelihood")
plot(unlist(modhmmList %>% map("mle") %>% map("stepPar") %>% map(1)),
     xlab = "Id du jeu de données",
     ylab = "Mean step state 1")
plot(unlist(modhmmList %>% map("mle") %>% map("stepPar") %>% map(2)),
     xlab = "Id du jeu de données",
     ylab = "Mean step state 2")
plot(unlist(modhmmList %>% map("mle") %>% map("anglePar") %>% map(1)),
     xlab = "Id du jeu de données",
     ylab = "Mean angle state 1")
plot(unlist(modhmmList %>% map("mle") %>% map("anglePar") %>% map(2)),
     xlab = "Id du jeu de données",
     ylab = "Mean angle state 2")

# Prévu un ggplot qui donne les valeurs de mean angle/step pour chaque état,
# avec les CI et par ordre de likelihood.
