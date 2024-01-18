
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / MoveHMM parameter space exploration
#  Serge NABENEZA - JANVIER 2024



#### Loop analyse sensi one at a time

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
                     max = c(0.100, 0.250,0.400))
  whichzero <- which(hmmdata$step == 0)
  zeromass0<-c(length(whichzero)/nrow(hmmdata),0,0)
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