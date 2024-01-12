
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / ANIMAL ASSOCIATION
#  Arthur SCRIBAN - JANVIER 2024


setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

library(lubridate)

rm(list=ls())
date()

# Importation données
metaSourceDir <- "./0_raw_data/METADATA/"
ANX <- read.table(
  paste0(metaSourceDir, "AnimalSegmentationTable.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
ANX$debutPortColl <- dmy_hm(ANX$debutPortColl)
ANX$finPortColl <- dmy_hm(ANX$finPortColl)

ANX <- ANX[, c(1, 3, 4, 7, 8, 5, 6)]
names(ANX) <- c("IDANL", "IDCOL", "IDELV", "TRANS", "IDVIL", "DHDEB", "DHFIN")
cat("ANX Table:\n")
print(head(ANX))

gpsSourceDir <- "./1_Data_clean_and_merge/"
GPS <- read.table(
  paste0(gpsSourceDir, "HAMAC-SN-GPS.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS$DHACQ<-ymd_hms(GPS$DHACQ)

cat("\nGPS Table:\n")
print(head(GPS))

actSourceDir <- "./1_Data_clean_and_merge/"
ACT <- read.table(
  paste0(actSourceDir, "HAMAC-SN-ACT.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")

cat("\nACT Table:\n")
print(head(ACT))


# Segmentation GPS
GPS_par_anx <- list()

for (i in 1:nrow(ANX)) {
  IDANL <- ANX$IDANL[i]
  idcol <- ANX$IDCOL[i]
  cat(paste0(IDANL, " ", idcol, "\n"))
  start_date <- ANX$DHDEB[i]
  end_date <- ANX$DHFIN[i]
  
  subset_data <- subset(GPS, IDCOL == idcol & DHACQ >= start_date & DHACQ <= end_date)
  
  GPS_par_anx[[IDANL]] <- subset_data
  print(summary(GPS_par_anx[[IDANL]]))
  cat("\n")
}


# Segmentation ACT
ACT_par_anx <- list()

for (i in 1:nrow(ANX)) {
  IDANL <- ANX$IDANL[i]
  idcol <- ANX$IDCOL[i]
  cat(paste0(IDANL, " ", idcol, "\n"))
  start_date <- ANX$DHDEB[i]
  end_date <- ANX$DHFIN[i]
  
  subset_data <- subset(ACT, IDCOL == idcol & DHACQ >= start_date & DHACQ <= end_date)
  
  ACT_par_anx[[IDANL]] <- subset_data
  print(summary(ACT_par_anx[[IDANL]]))
  cat("\n")
}




