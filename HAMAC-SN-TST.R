
library(moveHMM)
library(dplyr)
library(furrr)

############################ Brouillons et snippets ############################


haversine <- function(lon1, lat1, lon2, lat2) {
  # Calcule la distance entre deux points sur la surface d'une sph�re. (Full ChatGPT, ofc)
  R <- 6371 #rayon de la terre
  d_lon <- (lon2 - lon1) * (pi/180)
  d_lat <- (lat2 - lat1) * (pi/180)
  
  a <- sin(d_lat/2)^2 + cos(lat1 * (pi/180)) * cos(lat2 * (pi/180)) * sin(d_lon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  dist <- R * c
  return(dist)
}


#### Importation données classées par animal
repDonnees <- "./1_Data_clean_and_merge/"
GPS_ACT_par_anx <- read.table(
  paste0(repDonnees, "HAMAC-SN-GPSnACTpANX.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS_ACT_par_anx$DHACQ<-ymd_hms(GPS_ACT_par_anx$DHACQ)
head(GPS_ACT_par_anx)

GPS_ACT_par_anx <- subset(GPS_ACT_par_anx, IDCOL == 44159)

GPS_ACT_par_anx <- GPS_ACT_par_anx %>% mutate(DIST = haversine(lag(LON), lag(LAT), LON, LAT))

hist(GPS_ACT_par_anx$DIST, xlab = "step length", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))



GPSACQorig <- read.table(
  "./0_raw_data/GPS/GPS_Collar44159_20230706143528.csv",
  sep=";",header=T, skip=0,na.strings = "N/A")
cat("\nGPS Table:\n")
print(head(GPSACQorig))
# GPS data

datatoplot <- GPSACQ %>% mutate(DIST = haversine(lag(LON), lag(LAT), LON, LAT))
hist(datatoplot$DIST, xlab = "Dist de Haversine", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))
hist(hmm59$step, xlab = "step length", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))


datadeltaT <- GPSACQ %>% mutate(DELTAT = as.numeric((lag(DHACQ) - DHACQ) / 60))
datadeltaT <- datadeltaT[datadeltaT$DELTAT <= 240,]
datadeltaT <- datadeltaT[datadeltaT$DELTAT > -240,]
boxplot(datadeltaT$DELTAT, xlab = "deltaT (min)", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))
hist(datadeltaT$DELTAT, xlab = "deltaT (min)", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))
summary(datadeltaT$DELTAT)

#### Calcul des steps et des angles
hmmdata <- prepData(GPS_ACT_par_anx, type = "LL",coordNames=c("LON","LAT"))


summary(hmmdata$step)
hist(hmmdata$step, xlab = "step length (km)", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))


# datatoplot <- GPSACQ[GPSACQ$IDCOL == 44163, 1:2]
plot(GPSACQ[GPSACQ$IDCOL == 44173, 1:2][, "DHACQ"])
plot(GPSACQ[GPSACQ$IDCOL == 44172, 1:2][(
  GPSACQ[GPSACQ$IDCOL == 44172, 1:2]$DHACQ > as.POSIXct("2022-12-04") &
    GPSACQ[GPSACQ$IDCOL == 44172, 1:2]$DHACQ < as.POSIXct("2025-10-01")
), "DHACQ"])

ggplot(datatoplot, aes(x = (1:nrow(datatoplot)), y = DHACQ)) +
  geom_point() +
  labs(title = "Chronological Order Check", x = "id", y = "date") +
  theme_minimal()






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






################################################################################


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
nJxParamInit <- 44
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
batch_size <- ceiling(nJxParamInit / nThreads)
batches <- split(1:nJxParamInit, (0:(nJxParamInit - 1)) %/% batch_size)

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



a <- mtcars$drat

test <- function(a, b) {
  d <- b * mean(a)
  return (d)
}

listb <- c(-2,-1,0,1,2, 1,2,3,4,5)

listbc <- list(listb = listb, listc = listc)
listbc

map_dbl(listbc, mean)
listbc %>% map_dbl(mean)

mtcars$drat %>% map_dbl(test, b = 2)
map(listbc, test(mtcars, x$listb, x$listc))



listeParamInit$anglePar0 <- anglePar0
listeParamInit$stepPar0 <- stepPar0
map(listeParamInit, fitHMM_Log(data = hmmdata, nbStates = nbStates,
                               stepPar0 = .x$stepPar0, anglePar0 = .x$anglePar0))



############################## Dummy fit
# Dummy fitHMM_Log function for testing
fitHMM_Log <- function(data, nbStates, stepPar0, anglePar0 = NULL, beta0 = NULL,
                       delta0 = NULL, stepDist = c("gamma", "weibull", "lnorm", "exp"),
                       angleDist = c("vm", "wrpcauchy", "none"), angleMean = NULL,
                       stationary = FALSE, knownStates = NULL, verbose = 0, nlmPar = NULL, fit = TRUE) {
  
  # Mock results
  result <- list(
    stepPar0 = stepPar0,
    anglePar0 = anglePar0,
    mle = list(stepPar = c(1, 2, 3), anglePar = c(0.1, 0.2, 0.3),
               beta = matrix(0.01, nrow = 3, ncol = 6), delta = c(0.2, 0.3)),
    data = data,
    mod = list(),
    conditions = list(),
    rawCovs = list(),
    knownStates = knownStates,
    nlmTime = c(user.self = 0.1, sys.self = 0.05, elapsed = 0.2)
  )
  
  if (fit) {
    # Perform some mock optimization
    Sys.sleep(1)  # Simulating a lengthy optimization process
  }
  
  return(result)
}

# Example usage of the dummy fitHMM_Log function
dummy_result <- fitHMM_Log(data = hmmdata, nbStates = 2,
                           stepPar0 = c(1, 2, 0.5, 1.5), anglePar0 = c(0.1, 0.2, 1, 2))



#########################################
niter <- 25

# Function to generate a random set of initial parameters
generate_initial_params <- function() {
  propzero <- length(which(hmmdata$step == 0))/nrow(hmmdata)
  list(
    stepMean0 = runif(2, min = c(0.5, 3), max = c(2, 8)),
    stepSD0 = runif(2, min = c(0.5, 3), max = c(2, 8)),
    angleMean0 = c(0, 0),
    angleCon0 = runif(2, min = c(0.5, 5), max = c(2, 15))#,
    # zeroMass0 = c(propzero, propzero/100)
  )
}

# Generate a list of diverse initial parameter sets
initial_params_list <- map(1:niter, ~ generate_initial_params())

# Function to fit the HMM using fitHMM_Log
fitWithParam <- function(initial_params) {
  fitHMM_Log(data = hmmdata, nbStates = 2,
             stepPar0 = c(initial_params$stepMean0,
                          initial_params$stepSD0),
             # initial_params$zeroMass0),
             anglePar0 = c(initial_params$angleMean0,
                           initial_params$angleCon0))
}

c(initial_params_list[[1]]$stepMean0, initial_params_list[[1]]$stepSD0)
fitWithParam(initial_params_list[[1]])$stepPar0

# Run the HMM fitting in parallel using pmap
result_list <- initial_params_list %>% map(fitWithParam)

c(initial_params_list[[1]]$stepMean0, initial_params_list[[1]]$stepSD0)
result_list[[1]]$stepPar0




