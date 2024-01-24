
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / ACT aggregation to GPS
#  Arthur SCRIBAN & ChatGPT - JANVIER 2024

library(dplyr)
library(furrr)

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

# Nettoyer le cache
rm(list=ls())


#### Importation données classées par animal
sourceDir <- "./1_Data_clean_and_merge/"

# GPS
GPS_par_anx <- read.table(
  paste0(sourceDir, "HAMAC-SN-GPSpANX.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS_par_anx$DHACQ<-ymd_hms(GPS_par_anx$DHACQ)
head(GPS_par_anx)

# ACT
## Ne marche pas en l'état mais flemme, plutôt runner Animal segmentation
# ACT_par_anx <- read.table(
#   paste0(sourceDir, "HAMAC-SN-ACTpANX.csv"),
#   sep=";",header=T, skip=0,na.strings = "N/A")
# ACT_par_anx$DHACQ<-ymd_hms(ACT_par_anx$DHACQ)
# head(ACT_par_anx)


# Paramètres 
nbACTpts <- 5 # Nombre de points ACT autour du point GPS
noms_col_2_add <- c("AcX", "AcY", "AcZ", "TMP")

calcul_moyenne_accelero <- function(ligne_GPS) {
  closest_rows <- ACT_par_anx[[ligne_GPS$ID]] %>%
    arrange(abs(DHACQ - ligne_GPS$DHACQ)) %>%
    slice_head(n = nbACTpts)
  mean_data <- colMeans(closest_rows[,noms_col_2_add])
  
  return(mean_data)
}

# GPS_ACT_par_anx <- GPS_par_anx[1:1000,]
GPS_ACT_par_anx <- GPS_par_anx
colnames(GPS_ACT_par_anx)[colnames(GPS_ACT_par_anx) == 'TMP'] <- "GPS_TMP"
for (col_name in noms_col_2_add) {
  GPS_ACT_par_anx <- mutate(GPS_ACT_par_anx, !!col_name := NA) # ChatGPT magic
}
head(GPS_ACT_par_anx)

plan(multisession, workers = 22) # Début parallelisation sur workers threads

n <- nrow(GPS_ACT_par_anx)
result_list <- future_map(1:n, function(i) {
  if (i %% round(n/10) == 0) {
    percentage_completion <- round((i / n) * 100)
    cat(sprintf("Association en cours... %d%%\n", percentage_completion))
  }
  calcul_moyenne_accelero(GPS_ACT_par_anx[i, ])
})

plan(sequential) # Fin parallelisation

for (i in 1:n) {
  GPS_ACT_par_anx[i, noms_col_2_add] <- result_list[[i]]
}
rm(result_list)
head(GPS_ACT_par_anx)


# Enregistrement
workd1<-"./1_Data_clean_and_merge"
write.table(GPS_ACT_par_anx,paste0(workd1,"/HAMAC-SN-GPSnACTpANX.csv"),sep=";", row.names=FALSE)



