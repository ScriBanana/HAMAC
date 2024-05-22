
#  SENEGAL CATTLE GPS DATA
#  Simulated datasets
#  Arthur SCRIBAN - FEVRIER 2024


library(moveHMM)
library(ggplot2)

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")


#### Test simData
stepPar <- c(0.0135348774, 1.801552e-01, 6.854884e-01,
             0.0119984945, 1.492473e-01, 4.398711e-01,
             0.0004213871, 9.060636e-15, 7.128825e-12) # mean1, mean2, sd1, sd2, z1, z2
anglePar <- c(-3.0534134, 5.344411e-05, -0.01464707,
              0.2928482, 1.874756e-01,  1.95541075) # mean1, mean2, k1, k2
stepDist <- "gamma"
angleDist <- "vm"

plotSim <- function(nbPts, stepPar, anglePar) {
  data <- simData(nbAnimals=1,nbStates=1,stepDist=stepDist,angleDist=angleDist,stepPar=stepPar,
                  anglePar=anglePar,nbCovs=0,zeroInflation=TRUE,obsPerAnimal=nbPts)
  
  ### Copied from moveHMM source code
  
  nbAnimals <- length(unique(data$ID))
  animalsInd <- 1:nbAnimals
  
  # determine bounds
  # ind <- which(data$ID %in% unique(data$ID)[animalsInd])
  # xlim <- range(data$x[ind], na.rm = TRUE)
  # ylim <- range(data$y[ind], na.rm = TRUE)
  
  # plot tracks
  plot(NA, xlim = xlim, ylim = ylim,
       xlab = "", ylab = "", asp = 1)
  for(zoo in animalsInd) {
    ID <- unique(data$ID)[zoo]
    x <- data$x[which(data$ID == ID)]
    y <- data$y[which(data$ID == ID)]
    points(x, y, type = "o", pch = 20, lwd = 1.3,
           col = "darkblue", cex = 0.5)
  }
} 

 
# png(paste0("/home/scriban/Dropbox/Thèse/Productions/Articles/Mobi/Figures/",
#            format(Sys.time(), format = "%y%m%d"), "-Simplots.png"),
#     width = 1000, height = 500)

par(mfcol = c(1, 3))

xlim <- c(-0.5, 4)
ylim <- c(-0.5, 4)

for (i in 1:3) {
  plotSim(10, c(stepPar[i], stepPar[i + 3], stepPar[i + 6]), c(anglePar[i], anglePar[i + 3]))
}

# dev.off()