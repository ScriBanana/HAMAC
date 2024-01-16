
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / ACT aggregation to GPS
#  Arthur SCRIBAN & ChatGPT - JANVIER 2024

library(dplyr)


# rm(list=ls())

#### Importation données classées par animal
sourceDir <- "./1_Data_clean_and_merge/"

# GPS
GPS_par_anx <- read.table(
  paste0(sourceDir, "HAMAC-SN-GPSpANX.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS_par_anx$DHACQ<-ymd_hms(GPS_par_anx$DHACQ)
head(GPS_par_anx)

# ACT
# Flemme de faire l'import, juste faire tourner Animal_Segmentation


# Paramètres 
delta_t <- 15 # minutes
noms_col_2_add <- c("AcX", "AcY", "AcZ", "TMP")

# Function to calculate the mean of DATA in a window of delta_t minutes around each timestamp
calcul_moyenne_accelero <- function(ligne_GPS) {
  lower_bound <- ligne_GPS$DHACQ - delta_t * 60
  upper_bound <- ligne_GPS$DHACQ + delta_t * 60
  
  pts_dans_fenetre <- subset(ACT_par_anx[[ligne_GPS$ID]], DHACQ >= lower_bound & DHACQ <= upper_bound)
  mean_data <- colMeans(pts_dans_fenetre[,noms_col_2_add])
  
  return(mean_data)
}

# Apply the function to calculate mean_data for each row in table1
# GPS_ACT_par_anx <- GPS_par_anx[1:100,]
GPS_ACT_par_anx <- GPS_par_anx
colnames(GPS_ACT_par_anx)[colnames(GPS_ACT_par_anx) == 'TMP'] <- "GPS_TMP"
for (col_name in noms_col_2_add) {
  GPS_ACT_par_anx <- mutate(GPS_ACT_par_anx, !!col_name := NA) # ChatGPT magic
}
head(GPS_ACT_par_anx)

n <- nrow(GPS_ACT_par_anx)
for (i in 1:n) {
  if (i %% round(n/10) == 0) {
    percentage_completion <- (i / n) * 100
    cat(sprintf("Association en cours... %d%%\n", percentage_completion))
  }
  GPS_ACT_par_anx[i,noms_col_2_add] <- calcul_moyenne_accelero(GPS_ACT_par_anx[i,])
}
head(GPS_ACT_par_anx)

# Enregistrement
workd1<-"./1_Data_clean_and_merge"
write.table(GPS_ACT_par_anx,paste0(workd1,"/HAMAC-SN-GPSnACTpANX.csv"),sep=";", row.names=FALSE)



