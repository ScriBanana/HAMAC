
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / Importation de données préparées
#  Arthur SCRIBAN - JANVIER 2024


setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())

repDonnees <- "./1_Data_clean_and_merge/"
outDir <- "./2_Fits_outputs/"
logFile <- "HAMAC-SN-Log.csv"

#### Fonction de fit avec enregistrement dans le log
fitHMM_Log <- function (data, nbStates, formula = ~1, stepPar0, anglePar0) {
  
  print(paste0("Debut d'execution de FitHMM : ", date()))
  timestamp <- Sys.time()
  
  modhmm <- fitHMM(data = data, verbose = 0,
                   nbStates = nbStates, formula = formula,
                   anglePar0 = anglePar0, stepPar0 = stepPar0)
  
  indicateurs <- c(modhmm$mod$minimum, AIC(modhmm), modhmm$mod$iterations)
  anglePar <- c(modhmm$mle$anglePar[1,], modhmm$mle$anglePar[2,])
  
  if (nrow(modhmm$mle$stepPar) == 2) { # si pas de zeromass
    stepPar <- c(modhmm$mle$stepPar[1,], modhmm$mle$stepPar[2,])
  } else {
    stepPar <- c(modhmm$mle$stepPar[1,], modhmm$mle$stepPar[2,], modhmm$mle$stepPar[3,])
  }
  
  ligneLog <- c(as.numeric(format(Sys.time(), format = "%y%m%d%H%M%S")),
                nbStates, indicateurs, anglePar, stepPar, anglePar0, stepPar0)
  
  saveRDS(modhmm, paste0(outDir,
                         format(Sys.time(), format = "%y%m%d%H%M%S"),
                         "-HAMAC-SN-ModHMM-", nbStates,"Et.rds"))
  
  write.table(
    matrix(ligneLog, ncol = length(ligneLog)), file = paste0(outDir, logFile),
    sep = ";", col.names = FALSE, row.names = FALSE, append = TRUE
  )
  
  print(paste0("Fin d'execution de FitHMM : ", date()))
  print(Sys.time() - timestamp)
  
  return(modhmm)
}

#### Créé le fichier s'il n'existe pas
if (!file.exists(paste0(outDir, logFile))) {
  header <- c(
    "timestamp", "nbStates",
    "modhmm$mod$minimum", "AIC", "modhmm$mod$iterations",
    "Fit (état par état) : angleMean, angleCon, stepMean, stepSD, zeroMass évnt. + les param�tres init (stepMean0, etc)"
  )
  cat("Pas de fichier de log détecté. Nouveau fichier généré.")
  write.table(matrix(header, ncol = length(header)),
              file = paste0(outDir, logFile), sep = ";", col.names = FALSE, row.names = FALSE)
}

#### Importation données sorties de prepData
hmmdata <- readRDS(paste0(repDonnees,"/HAMAC-SN-HMMDATA.rds"))

#### Ajout de colonnes
## Decomposition de la date pour QGIS
hmmdata$DAT <- format(hmmdata$DHACQ, "%y-%m-%d")
hmmdata$YER <- as.numeric(format(hmmdata$DHACQ, "%y"))
hmmdata$MON <- as.numeric(format(hmmdata$DHACQ, "%m"))
hmmdata$DAY <- as.numeric(format(hmmdata$DHACQ, "%d"))
hmmdata$MND <- as.numeric(format(hmmdata$DHACQ, "%j")) / 365 * 12
hmmdata$HUR <- format(hmmdata$DHACQ, "%H:%M:%S")
# hmmdata$HMS <- as.numeric(format(hmmdata$DHACQ, "%H")) * 10000 +
#   as.numeric(format(hmmdata$DHACQ, "%M")) * 100 +
#   as.numeric(format(hmmdata$DHACQ, "%S"))
hmmdata$HRM <- as.numeric(format(hmmdata$DHACQ, "%H")) +
  as.numeric(format(hmmdata$DHACQ, "%M")) / 60


head(hmmdata)
# summary(hmmdata)
