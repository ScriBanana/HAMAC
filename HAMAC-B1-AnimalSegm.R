###################
## HAMAC Routine ##
###################

## ANIMAL ASSOCIATION
## A. SCRIBAN & ChatGPT - Janvier 2024

### Libraries
library(lubridate)
library(stringr)
library(dplyr)

### Paths
inDir <- "./0_RawData"
outDir <- "./1_IntermeData"
filesPrefix <- "/HAMAC-SN-"


### Functions


### Execution

#### Importation données
## Animaux
metaSourceDir <- paste0(inDir, "/METADATA/")
ANX <- read.table(
  paste0(metaSourceDir, "AnimalSegmentationTable.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
ANX$debutPortColl <- dmy_hm(ANX$debutPortColl)
ANX$finPortColl <- dmy_hm(ANX$finPortColl)
ANX <- ANX[, c(1, 3, 4, 7, 8, 5, 6)]
names(ANX) <- c("IDANL", "IDCOL", "IDELV", "TRA", "IDVIL", "DHDEB", "DHFIN")
cat("ANX Table:\n")
print(head(ANX))

## GPS
gpsSourceDir <- paste0(outDir, filesPrefix)
GPS <- read.table(
  paste0(gpsSourceDir, "GPS_brutes.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS$DHACQ<-ymd_hms(GPS$DHACQ)
cat("\nGPS Table:\n")
print(head(GPS))

## Accéléro
actSourceDir <- paste0(outDir, filesPrefix)
ACT <- read.table(
  paste0(actSourceDir, "ACT_brutes.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
ACT$DHACQ <- ifelse (str_length(ACT$DHACQ)==10,paste(ACT$DHACQ," 00:00:00",sep=""),ACT$DHACQ)
# parse_ymd_hms(ACT$DHACQ)
ACT$DHACQ<-ymd_hms(ACT$DHACQ)
cat("\nACT Table:\n")
print(head(ACT))


#### Segmentation
## GPS
GPS_par_anx <- data.frame()

for (i in 1:nrow(ANX)) {
  idanl <- ANX$IDANL[i]
  idcol <- ANX$IDCOL[i]
  cat(paste0(idanl, " ", idcol, "\n"))
  start_date <- ANX$DHDEB[i]
  end_date <- ANX$DHFIN[i]
  
  subset_data <- subset(GPS, IDCOL == idcol & DHACQ >= start_date & DHACQ <= end_date)
  subset_data <- mutate(subset_data, ID = idanl,
                        IDELV = ANX$IDELV[i],
                        TRA = ANX$TRA[i],
                        IDVIL = ANX$IDVIL[i])
  # print(summary(subset_data))
  # cat("\n")
  
  GPS_par_anx <- rbind(GPS_par_anx, subset_data)
}

## ACT
ACT_par_anx <- list()

for (i in 1:nrow(ANX)) {
  IDANL <- ANX$IDANL[i]
  idcol <- ANX$IDCOL[i]
  cat(paste0(IDANL, " ", idcol, "\n"))
  start_date <- ANX$DHDEB[i]
  end_date <- ANX$DHFIN[i]
  
  subset_data <- subset(ACT, IDCOL == idcol & DHACQ >= start_date & DHACQ <= end_date)
  
  if (length(ACT_par_anx[[IDANL]]) == 0) {
    ACT_par_anx[[IDANL]] <- subset_data
  } else {
    # Condition pour gérer les deux colliers de VSR11
    ACT_par_anx[[IDANL]] <- rbind(ACT_par_anx[[IDANL]], subset_data)
  }
  rm(subset_data)
  
  # print(summary(ACT_par_anx[[IDANL]]))
  # cat("\n")
}

#### Sauvegardes CSV.
write.table(GPS_par_anx,paste0(outDir, filesPrefix, "GPSpANX.csv"),sep=";", row.names=FALSE)
write.table(bind_rows(ACT_par_anx),paste0(outDir, filesPrefix, "ACTpANX.csv"),sep=";", row.names=FALSE)
for (i in names(ACT_par_anx)) {
  write.table(ACT_par_anx[[i]],paste0(outDir,"/ACTpANX/HAMAC-SN-ACTpANX-", i, ".csv"),sep=";", row.names=FALSE)
}


########################################################

## Fonction utilitaire pour déterminer les erreurs avec lubridate
# 
# parse_ymd_hms = function(x){
#   d=lubridate::ymd_hms(x, quiet=TRUE)
#   errors = x[!is.na(x) & is.na(d)]
#   if(length(errors)>0){
#     cli::cli_warn("Failed to parse some dates: {.val {errors}}")
#   }
#   d
# }
