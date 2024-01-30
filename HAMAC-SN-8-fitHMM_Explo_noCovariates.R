
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM parameter space exploration
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)
library(purrr)
library(furrr)
library(ggplot2)

### ATTENTION : faire tourner le script PrÃ©liminaires_Fits au prÃ©alable

setwd("/home/scriban/Dropbox/ThÃ¨se/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

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
nJxParamInit <- 44
nThreads <- 22
nbStates <- 2

# GÃ©nÃ¨re les jeux de paramÃ¨tres initiaux. Remplir oÃ¹ indiquÃ©
generate_initial_params <- function() {
  
  propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)

  list(
    stepMean0 = runif(nbStates, # Ici :
                      min = c(0.010, 0.500, 0.300),
                      max = c(0.500, 1.000, 1.000)),
    stepSD0 = runif(nbStates, # Là :
                    min = c(0.010, 0.300, 0.100),
                    max = c(0.200, 0.500, 0.300)),
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


repSauvegardes <- "./2_Fits_outputs/"
meilleurModele2e <- readRDS(paste0(repSauvegardes,"240129181847-HAMAC-SN-ModHMM-2Et.rds"))

meilleurModele3e <- readRDS(paste0(repSauvegardes,"240127202639-HAMAC-SN-ModHMM-3Et.rds"))
AIC(meilleurModele2e, meilleurModele3e)
AIC2 <- AIC(meilleurModele2e)
AIC3 <- AIC(meilleurModele3e)
exp((AIC3-AIC2)/2)


################################################################################
#### Sorties

# Tu peux décommenter les lignes pour explorer la donnée (ctrl + maj + c)
 modhmmList %>% map("mle")
 modhmmList %>% map("mle") %>% map("stepPar")
 modhmmList %>% map("mod") %>% map("minimum")
 modhmmList %>% map("mod") %>% map("iterations")

# Plots ultra rudimentaires pour comparer les sorties (likelihood, stepMean, angleMean...)
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


################################################################################
#### Meilleur modèle

meilleurModeleID <- which.min(unlist(lapply(modhmmList, function(m) m$mod$minimum)))
meilleurModele <- modhmmList[[meilleurModeleID]]

