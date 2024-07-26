###################
## HAMAC Routine ##
###################

## DATA CLEAN AND MERGE CODE
## Daniel CORNELIS - JUILLET 2023
## Revu par A. SCRIBAN - Janvier 2024

### Libraries
library(dplyr)
library(stringr)
library(lubridate)

### Paths
rawDir <- "./0_RawData"
inDir <- "./1_IntermeData"
filesPrefix <- "/HAMAC-SN-"

### Functions


### Execution

# A.1. Lecture, concatenation et mise en forme des données GPS
## Importation
## Choisir :
# Fichiers Serge initiaux
# filename<- list.files(workd0,full.names = TRUE)
# filename
# GPSACQorig<-read.csv(filename[1],sep=";",header=F,skip=1,na.strings = "NA")
# cat(GPSACQorig[1,2], ncol(GPSACQorig),"\n")
# for(i in 2:(length(filename))) {
#   gpsacq=read.csv(filename[i],sep=";",header=F, skip=1,na.strings = "NA") #N/A pas bon
#   cat(gpsacq[1,2], ncol(gpsacq),"\n")
#   GPSACQorig=rbind(GPSACQorig, gpsacq)
# }
# head(GPSACQorig)

# Scan automatique whole dir
GPSACQorig <- read.table(
  paste0(inDir, filesPrefix, "GPS_WholeDir.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
cat("\nGPS Table:\n")
print(head(GPSACQorig))


# A. Corrections sur la BD brute

#### Suppression des plages de données avec les données surnuméraires
GPSACQorig <- GPSACQorig %>%
  filter(
    !(V2 == 44159 & as.POSIXct(strptime(V3,format="%d/%m/%Y"),tz="GMT") < as.POSIXct("2023-08-14")) &
    !(V2 == 44170 & as.POSIXct(strptime(V3,format="%d/%m/%Y"),tz="GMT") < as.POSIXct("2022-05-25"))
  )

# Remplace les parties foireuses de 44159 et 44170 par des données qui marchent.
GPS59 <- read.table(
  paste0(rawDir, "/GPS/GPS_Collar44159_20230706143528.csv"),
  sep=";",header=F, skip=1,na.strings = "N/A")
GPS59 <- cbind(GPS59, V51 = NA, V52 = NA)
GPS70 <- read.table(
  paste0(rawDir, "/GPS/GPS_Collar44170_20230706144441.csv"),
  sep=";",header=F, skip=1,na.strings = "N/A")
GPS70 <- cbind(GPS70, V51 = NA, V52 = NA)
GPSACQorig <- GPSACQorig %>% rbind(GPS59, GPS70)
rm(GPS59, GPS70)


#### Formatage
# selection des colonnes d'intéret
GPSACQ <- GPSACQorig[,c(2,3,4,14,13,15,48,7,16)]
names(GPSACQ)=c("IDCOL","DACQ","HACQ","LON","LAT","HEI","TMP","ORI","DOP")  # Attribution d'un nom aux colonnes
head(GPSACQ)

# Horodatage en une seule colonne
GPSACQ$DHACQ<-paste(GPSACQ$DACQ,GPSACQ$HACQ)
GPSACQ$DHACQ <- ifelse (str_length(GPSACQ$DHACQ)==10,paste(GPSACQ$DHACQ," 00:00:00",sep=""),GPSACQ$DHACQ)
GPSACQ$DHACQ<-as.POSIXct(strptime(GPSACQ$DHACQ,format="%d/%m/%Y %H:%M:%S"),tz="GMT")
GPSACQ <- GPSACQ[,c(1,10,4,5,6,7,8,9)]

# Formatage des données
GPSACQ$LAT<-str_replace_all(as.character(GPSACQ$LAT),",", ".")
GPSACQ$LON<-str_replace_all(as.character(GPSACQ$LON),",", ".")
GPSACQ$HEI<-str_replace_all(as.character(GPSACQ$HEI),",", ".")
GPSACQ$DOP<-str_replace_all(as.character(GPSACQ$DOP),",", ".")
GPSACQ$LON<-as.numeric(GPSACQ$LON)
GPSACQ$LAT<-as.numeric(GPSACQ$LAT)
GPSACQ$HEI<-as.numeric(GPSACQ$HEI)
GPSACQ$DOP<-as.numeric(GPSACQ$DOP)
GPSACQ$ORI<-as.logical(GPSACQ$ORI == "Collar")


#### NA omit
dim(GPSACQ)
# GPSACQ<-na.omit(GPSACQ)
naaenlever <- which(rowSums(is.na(GPSACQ[, c("DHACQ", "LAT", "LON")])) > 0)
length(naaenlever)
GPSACQ <- GPSACQ[ - naaenlever,]
rm(naaenlever)
dim(GPSACQ)
# head(GPSACQ)
# table(GPSACQ$IDCOL)
# summary(GPSACQ)

#### Suppression des plages ou deltaT n'est pas constant
# Fait ? la main. runner les plots pour confirmer :
# collier <- 44172
# plot(GPSACQ[GPSACQ$IDCOL == collier, 1:2][, "DHACQ"])
# plot(GPSACQ[GPSACQ$IDCOL == collier, 1:2][(
#   GPSACQ[GPSACQ$IDCOL == collier, 1:2]$DHACQ > as.POSIXct("2022-12-04") &
#     GPSACQ[GPSACQ$IDCOL == collier, 1:2]$DHACQ < as.POSIXct("2025-10-01")
# ), "DHACQ"])

GPSACQ <- GPSACQ %>%
  filter( # Intervalles a enlever
    !(IDCOL == 44159 &
        DHACQ > ymd_hms("2022-11-20 00:00:00") &
        DHACQ < ymd_hms("2023-08-10 00:00:00")
    ) &
    !(IDCOL == 44160 &
        DHACQ > ymd_hms("2023-06-17 00:00:00") # DeltasT de 1h
    ) &
    !(IDCOL == 44161 &
        DHACQ > ymd_hms("2023-02-15 00:00:00")
    ) &
    !(IDCOL == 44164 &
        DHACQ > ymd_hms("2022-10-28 00:00:00") &
        DHACQ < ymd_hms("2022-12-10 00:00:00")
    ) &
    !(IDCOL == 44164 &
        DHACQ > ymd_hms("2023-07-28 00:00:00")
    ) &
    !(IDCOL == 44171 &
        DHACQ > ymd_hms("2021-09-02 00:00:00") &
        DHACQ < ymd_hms("2021-09-29 00:00:00")
    ) &
    !(IDCOL == 44172 &
        DHACQ > ymd_hms("2022-12-04 00:00:00")
    )
  )

# A.2. Check doublons éventuels

dim(GPSACQ)
dupli<-duplicated(GPSACQ[,c(1:2)])
summary(dupli)
GPSACQ<-GPSACQ[dupli==F,]
# table(GPSACQ$IDCOL)
dim(GPSACQ)
# head(GPSACQ)
# summary(GPSACQ)


# A.3. Fenêtres temporelle et spatiale + retrait iridium

#plot(GPSACQ$LON,GPSACQ$LAT,asp=1)
dim(GPSACQ)
GPSACQ<-GPSACQ[GPSACQ$DHACQ>=as.POSIXct("2021-05-09"),]

GPSACQ<-GPSACQ[GPSACQ$LON<(-10),] # retire les  donnees aberrantes
GPSACQ<-GPSACQ[GPSACQ$LON>(-17.3),] # retire les  donnees aberrantes
GPSACQ<-GPSACQ[GPSACQ$LAT<20,] # retire les  donnees aberrantes
GPSACQ<-GPSACQ[GPSACQ$LAT>(10),] # retire les  donnees aberrantes
#plot(GPSACQ$LON,GPSACQ$LAT,asp=1)

# boxplot(GPSACQ$HEI)
# dim(GPSACQ[GPSACQ$HEI<(-100),])
# dim(GPSACQ[GPSACQ$HEI>(650),]) # 650 m point le plus haut du Sénégal

# hist(GPSACQ$TMP)
# GPSACQ<-GPSACQ[GPSACQ$TMP!=0,]

# boxplot(GPSACQ$DOP)
# dim(GPSACQ[GPSACQ$DOP>3,])
# GPSACQ<-GPSACQ[GPSACQ$DOP<3,]

#### Retrait Iridium
dim(GPSACQ)
dim(GPSACQ[GPSACQ$ORI!=T,])
GPSACQ<-GPSACQ[GPSACQ$ORI==T,]
GPSACQ$ORI <- NULL

dim(GPSACQ)
summary(GPSACQ)

