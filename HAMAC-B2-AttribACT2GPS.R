###################
## HAMAC Routine ##
###################

## ACT aggregation to GPS
## A. SCRIBAN & ChatGPT - Janvier 2024

### Libraries
library(lubridate)
library(dplyr)
library(furrr)
library(data.table)

### Paths
inDir <- "./1_IntermeData"
outDir <- "./1_IntermeData"
filesPrefix <- "/HAMAC-SN-"


### Functions
## Fonction qui moyenne les donn?es acc?l?ro
calcul_moyenne_accelero <- function(ligne_GPS) {
  closest_rows <- ACT_par_anx[[ligne_GPS[["ID"]]]] %>%
    arrange(abs(DHACQ - ymd_hms(ligne_GPS[["DHACQ"]]))) %>%
    slice_head(n = nbACTpts)
  mean_data <- colMeans(closest_rows[,noms_col_2_add], na.rm = TRUE)
  
  return(mean_data)
}

### Execution

#### Importation donnÃ©es classÃ©es par animal

# GPS
GPS_par_anx <- read.table(
  paste0(inDir, filesPrefix, "GPSpANX.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS_par_anx$DHACQ<-ymd_hms(GPS_par_anx$DHACQ)
head(GPS_par_anx)

# ACT
## Ne marche pas en l'Ã©tat mais flemme, plutÃ´t runner Animal segmentation
# ACT_par_anx <- read.table(
#   paste0(sourceDir, "HAMAC-SN-ACTpANX.csv"),
#   sep=";",header=T, skip=0,na.strings = "N/A")
# ACT_par_anx$DHACQ<-ymd_hms(ACT_par_anx$DHACQ)
# head(ACT_par_anx)


#### Parametres 
nbACTpts <- 5 # Nombre de points ACT autour du point GPS
noms_col_2_add <- c("AcX", "AcY", "AcZ", "TMP")
nThreads <- 22

#### Init la table de sortie
GPS_ACT_par_anx <- GPS_par_anx
# GPS_ACT_par_anx <- GPS_par_anx[1:1000,]
colnames(GPS_ACT_par_anx)[colnames(GPS_ACT_par_anx) == 'TMP'] <- "GPS_TMP"
for (col_name in noms_col_2_add) {
  GPS_ACT_par_anx <- mutate(GPS_ACT_par_anx, !!col_name := NA) # ChatGPT magic
}
head(GPS_ACT_par_anx)


#### Execution
print(paste0("Debut d'association : ", date()))
debAssoc <- Sys.time()
plan(multisession, workers = nThreads) # DÃ©but parallelisation sur workers threads

listesAccMoyenne <- asplit(GPS_ACT_par_anx, 1) %>% future_map(calcul_moyenne_accelero)

plan(sequential) # Fin parallelisation
print(paste0("Fin d'association : ", date()))
print(Sys.time() - debAssoc)

setDT(GPS_ACT_par_anx) # Assigne les valeurs calculées à la BD
GPS_ACT_par_anx[, (noms_col_2_add) := transpose(listesAccMoyenne)]

print(Sys.time() - debAssoc)
head(GPS_ACT_par_anx)


#### Intermediate data save
write.table(GPS_ACT_par_anx,
            paste0(outDir, filesPrefix, "GPSnACTpANX.csv"),
            sep=";", row.names=FALSE)
