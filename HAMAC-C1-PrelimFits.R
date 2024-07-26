###################
## HAMAC Routine ##
###################

## Preliminary script to running fits
## Arthur SCRIBAN - JANVIER 2024

### Libraries


### Paths
inDir <- "./1_IntermeData"
outDir <- "./2_OutFits"
filesPrefix <- "/HAMAC-SN-"
logFile <- "HAMAC-SN-Log.csv"
  
  
### Functions
# Wrapper for moveHMM fit function, which saves the result and writes the Alog
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
  
  saveRDS(modhmm, paste0(outDir, filesPrefix,
                         format(Sys.time(), format = "%y%m%d%H%M%S"),
                         "-ModHMM-", nbStates,"Et.rds"))
  
  write.table(
    matrix(ligneLog, ncol = length(ligneLog)), file = paste0(outDir, logFile),
    sep = ";", col.names = FALSE, row.names = FALSE, append = TRUE
  )
  
  print(paste0("Fin d'execution de FitHMM : ", date()))
  print(Sys.time() - timestamp)
  
  return(modhmm)
}


### Execution

## Créé le fichier s'il n'existe pas
if (!file.exists(paste0(outDir, "/", logFile))) {
  header <- c(
    "timestamp", "nbStates",
    "modhmm$mod$minimum", "AIC", "modhmm$mod$iterations",
    "Fit (état par état) : angleMean, angleCon, stepMean, stepSD, zeroMass évnt. + les param?tres init (stepMean0, etc)"
  )
  cat("Pas de fichier de log détecté. Nouveau fichier généré.")
  write.table(matrix(header, ncol = length(header)),
              file = paste0(outDir, "/", logFile), sep = ";", col.names = FALSE, row.names = FALSE)
}

#### Importation données sorties de prepData
hmmdata <- readRDS(paste0(inDir,"/HAMAC-SN-HMMDATA.rds"))


#### Intermediate data save
