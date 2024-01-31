
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / ACT aggregation to GPS
#  Arthur SCRIBAN & ChatGPT - JANVIER 2024

library(dplyr)
library(furrr)
library(lubridate)

setwd("/home/scriban/Dropbox/Th√®se/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

# Compter une trentaines de minutes sur le jeu complet, avec 22 coeurs

# Nettoyer le cache
rm(list=ls())


#### Importation donn√©es class√©es par animal
sourceDir <- "./1_Data_clean_and_merge/"

# GPS
GPS_par_anx <- read.table(
  paste0(sourceDir, "HAMAC-SN-GPSpANX.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS_par_anx$DHACQ<-ymd_hms(GPS_par_anx$DHACQ)
head(GPS_par_anx)

# ACT
## Ne marche pas en l'√©tat mais flemme, plut√¥t runner Animal segmentation
# ACT_par_anx <- read.table(
#   paste0(sourceDir, "HAMAC-SN-ACTpANX.csv"),
#   sep=";",header=T, skip=0,na.strings = "N/A")
# ACT_par_anx$DHACQ<-ymd_hms(ACT_par_anx$DHACQ)
# head(ACT_par_anx)


#### Parametres 
nbACTpts <- 5 # Nombre de points ACT autour du point GPS
noms_col_2_add <- c("AcX", "AcY", "AcZ", "TMP")
nThreads <- 22


#### Fonction qui moyenne les donnÈes accÈlÈro
calcul_moyenne_accelero <- function(ligne_GPS) {
  closest_rows <- ACT_par_anx[[ligne_GPS[["ID"]]]] %>%
    arrange(abs(DHACQ - ymd_hms(ligne_GPS[["DHACQ"]]))) %>%
    slice_head(n = nbACTpts)
  mean_data <- colMeans(closest_rows[,noms_col_2_add], na.rm = TRUE)
  
  return(mean_data)
}


#### Init la table de sortie
GPS_ACT_par_anx <- GPS_par_anx
# GPS_ACT_par_anx <- GPS_par_anx[1:100,]
colnames(GPS_ACT_par_anx)[colnames(GPS_ACT_par_anx) == 'TMP'] <- "GPS_TMP"
for (col_name in noms_col_2_add) {
  GPS_ACT_par_anx <- mutate(GPS_ACT_par_anx, !!col_name := NA) # ChatGPT magic
}
head(GPS_ACT_par_anx)


#### Execution
print(paste0("Debut d'association : ", date()))
debAssoc <- Sys.time()
plan(multisession, workers = nThreads) # D√©but parallelisation sur workers threads

listesAccMoyenne <- asplit(GPS_ACT_par_anx, 1) %>% future_map(calcul_moyenne_accelero)

plan(sequential) # Fin parallelisation
print(paste0("Fin d'association : ", date()))
print(Sys.time() - debAssoc)

n <- nrow(GPS_ACT_par_anx)
for (i in 1:n) { # Long mais pas parallÈlisable
  GPS_ACT_par_anx[i, noms_col_2_add] <- listesAccMoyenne[[i]]
}

head(GPS_ACT_par_anx)


#### Enregistrement
workd1<-"./1_Data_clean_and_merge"
write.table(GPS_ACT_par_anx,paste0(workd1,"/HAMAC-SN-GPSnACTpANX.csv"),sep=";", row.names=FALSE)



