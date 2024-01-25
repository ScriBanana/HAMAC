
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM parameter space exploration
#  Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)

### ATTENTION : faire tourner le script Préliminaires_Fits au préalable

head(hmmdata)
summary(hmmdata)

#### Loop analyse sensi one at a time

# For reproducibility
# set.seed(12345) Pas besoin, au contraire, à mon avis
# Number of tries with different starting values
niter <- 25
# Save list of fitted models
allm <- list()

for(i in 1:niter) {
  # Step length mean
  stepMean0 <- runif(3,
                     min = c(0.01, 0.05,0.300),
                     max = c(0.100, 0.250,0.400))
  whichzero <- which(hmmdata$step == 0)
  zeromass0<-c(length(whichzero)/nrow(hmmdata),0,0) ###### A MODIF
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
  stepPar0 <- c(stepMean0, stepSD0,zeromass0)
  anglePar0 <- c(angleMean0, angleCon0)
  allm[[i]] <- fitHMM_Log(data = hmmdata, nbStates = 3, stepPar0 = stepPar0,
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




################################################################
######## Tentative de parallélisation (ne marche pas encore)

#### Paramètres
nJeux <- 44
nThreads <- 22
nbStates <- 2

#### Définition des fonctions
process_batch <- function(batch_indices) {
  result_list <- future_map(batch_indices,
                            ~ fitHMM_Log(data = hmmdata, nbStates = nbStates,
                                         stepPar0 = stepPar0, anglePar0 = anglePar0))
  return(result_list)
}


#### Début parallelisation sur nThreads threads
plan(multisession, workers = nThreads)

# Divise le jeu de données en nbThreads
batch_size <- ceiling(nJeux / nThreads)
batches <- split(1:nJeux, (0:(nJeux - 1)) %/% batch_size)

# Cartesian product of arrays
stepPar0 <- c(stepMean0, stepSD0, zeroMass0)
anglePar0 <- c(angleMean0, angleCon0)
param_combinations <- expand.grid(stepPar0 = stepPar0, anglePar0 = anglePar0)

modhmm <- future_map(param_combinations$stepPar0, ~ {
  fitHMM_Log(data = hmmdata, nbStates = nbStates, stepPar0 = .x,
             anglePar0 = param_combinations$anglePar0[which(param_combinations$stepPar0 == .x)])
})

plan(sequential) # Fin parallelisation
print(paste0("Fin des calculs : ", date()))


