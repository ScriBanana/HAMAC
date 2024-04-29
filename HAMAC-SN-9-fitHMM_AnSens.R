
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM parameter space exploration
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)
library(purrr)
library(furrr)
library(ggplot2)

### ATTENTION : faire tourner le script Préliminaires_Fits au préalable

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

head(hmmdata)
summary(hmmdata)


################################################################################
#### Définition des fonctions
fitWithParam <- function(initial_params) { # Simplifie les calls
  fitHMM_Log(data = hmmdata, nbStates = nbStates,
             formula = formula,
             stepPar0 = c(initial_params$stepMean0,
                          initial_params$stepSD0,
                          initial_params$zeroMass0),
             anglePar0 = c(initial_params$angleMean0,
                           initial_params$angleCon0))
}


################################################################################
#### Paramètres
nJxParamInit <- 22
nThreads <- 22
nbStates <- 5#3
formula =
  ~1
  # ~GPS_TMP
  # ~ AcX + AcY + AcZ
  # ~AcX + AcY + AcZ + GPS_TMP
  # ~SES
  # ~AcX + AcY + AcZ + HRM
  # ~AcX + AcY + AcZ + MND
  # ~HRM + MND

# Génère les jeux de paramètres initiaux. Remplir où indiqué
generate_initial_params <- function() {
  
  propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
  list(
    
    ### 3 �tats
  #   stepMean0 = runif(nbStates, # Ici :
  #                 min = c(0.005, 0.050, 0.100),
  #                 max = c(0.100, 0.250, 1.000)),
  #   stepSD0 = runif(nbStates, # L? :
  #                 min = c(0.005, 0.050, 0.100),
  #                 max = c(0.100, 0.500, 1.000)),
  #   angleMean0 = runif(nbStates, # L? :
  #                 min = c(-3, -3, -0.5),
  #                 max = c(3, 3, 0.5)),
  #   angleCon0 = runif(nbStates, # Et l? :
  #                 min = c(0.1, 1.5, 1),
  #                 max = c(1, 5, 15)),
    
    ### 4 �tats
  # stepMean0 = runif(nbStates, # Ici :
  #           #                  E1    E2      E3    E4
  #                   min = c(0.005, 0.050, 0.100, 0.300),
  #                   max = c(0.100, 0.250, 0.800, 1.000)),
  # stepSD0 = runif(nbStates, # L? :
  #           #                  E1    E2      E3    E4
  #                   min = c(0.005, 0.050, 0.100, 0.400),
  #                   max = c(0.100, 0.500, 0.800, 1.000)),
  # angleMean0 = runif(nbStates, # L? :
  #           #                 E1  E2 E3    E4
  #                    min = c(-3, -3, -3, -0.5),
  #                    max = c(3, 3, 3, 0.5)),
  # angleCon0 = runif(nbStates, # Et l? :
  #           #                  E1   E2 E3 E4
  #                     min = c(0.1, 0.5, 1, 1),
  #                     max = c(1, 5, 5, 15)),
  
  ### 5 �tats
  stepMean0 = runif(nbStates,
                #         E1    E2      E3    E4      E5
                min = c(0.001, 0.010, 0.050, 0.100, 0.300),
                max = c(0.050, 0.250, 0.600, 0.800, 1.000)),
  stepSD0 = runif(nbStates,
                #         E1    E2      E3    E4      E5
                min = c(0.005, 0.010, 0.050, 0.100, 0.400),
                max = c(0.100, 0.250, 0.600, 0.800, 1.000)),
  angleMean0 = runif(nbStates,
                #         E1    E2    E3    E4      E5
                min = c(  -3,   -3,   -3,  -0.5,  -0.5),
                max = c(   3,    3,    3,   0.5,   0.5)),
  angleCon0 = runif(nbStates,
                #         E1    E2    E3    E4   E5
                min = c( 0.1,  0.2,  0.3,  0.5,   1),
                max = c(   1,    5,    5,    5,  15)),
  
    zeroMass0 = c(propzero, rep(propzero/100, nbStates - 1))
  )
}

# ajouter accéléro

################################################################################
#### Execution

# G?n?re les jeux de param?tres aléatoires
initial_params_list <- map(1:nJxParamInit, ~ generate_initial_params())

#### D?but parallelisation sur nThreads threads
print(paste0("Lancement de la boucle : ", date()))
tpsDebut <- Sys.time()
plan(multisession, workers = nThreads)

# Fait tourner fitHMM_Log sur chaque jeu
modhmmList <- initial_params_list %>% future_map(possibly(fitWithParam))

plan(sequential) # Fin parallelisation
print(paste0("Fin des calculs : ", date()))
print(Sys.time() - tpsDebut)


################################################################################
#### Sorties

# Tu peux d�commenter les lignes pour explorer la donn�e (ctrl + maj + c)
 # modhmmList %>% map("mle")
 # modhmmList %>% map("mle") %>% map("stepPar")
 # modhmmList %>% map("mod") %>% map("minimum")
 # modhmmList %>% map("mod") %>% map("iterations")

# Plots ultra rudimentaires pour comparer les sorties (likelihood, stepMean, angleMean...)
plot(unlist(modhmmList %>% map("mod") %>% map("minimum")),
     xlab = "Id du jeu de donn?es",
     ylab = "Maximum likelihood")
plot(unlist(modhmmList %>% map("mle") %>% map("stepPar") %>% map(1)),
     xlab = "Id du jeu de donn?es",
     ylab = "Mean step state 1")
plot(unlist(modhmmList %>% map("mle") %>% map("stepPar") %>% map(2)),
     xlab = "Id du jeu de donn?es",
     ylab = "Mean step state 2")
plot(unlist(modhmmList %>% map("mle") %>% map("anglePar") %>% map(1)),
     xlab = "Id du jeu de donn?es",
     ylab = "Mean angle state 1")
plot(unlist(modhmmList %>% map("mle") %>% map("anglePar") %>% map(2)),
     xlab = "Id du jeu de donn?es",
     ylab = "Mean angle state 2")

# Pr?vu un ggplot qui donne les valeurs de mean angle/step pour chaque ?tat,
# avec les CI et par ordre de likelihood.


################################################################################
#### Meilleur mod?le

meilleurModeleID <- which.min(unlist(lapply(modhmmList, function(m) m$mod$minimum)))
meilleurModele <- modhmmList[[meilleurModeleID]]
modhmm <- meilleurModele
