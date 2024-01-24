
#  SENEGAL CATTLE GPS DATA
#  DATA CLEAN AND MERGE CODE 
#  Daniel CORNELIS - JUILLET 2023
#  Revu par A. SCRIBAN - Janvier 2024


#Stipuler le repertoire de travail
setwd("/home/scriban/Dropbox/ThÃ¨se/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

library(MASS)
library(chron)
library(adehabitatHR)
library(adehabitatHS)
library(adehabitat) # N'existe pas??
library(rgdal) # Deprecated
options(digits = 10)
library(raster)
library(StreamMetabolism)
library(SOAR)
library(openxlsx)
library(readxl)
library(stringr)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)


rm(list=ls()) # fonction qui permet de virer tous les objets generes anterieurements
date()

workd0<-"./0_raw_data/GPS"
workd1<-"./1_Data_clean_and_merge"

################################################################################
## A. Concatenation et mise en forme des donnees GPS acquises
################################################################################

################################################################################
# A.1. Lecture, concatenation et mise en forme des donnÃ©es GPS

#### Importation
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
gpsSourceDir <- "./1_Data_clean_and_merge/"
GPSACQorig <- read.table(
  paste0(gpsSourceDir, "HAMAC-SN-GPS_WholeDir.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
cat("\nGPS Table:\n")
print(head(GPSACQorig))


################################################################################
# A. Corrections sur la BD brute

#### Suppression des plages de données avec les données surnuméraires
GPSACQ <- GPSACQ %>%
  filter(
    !(IDCOL == 44159 & DHACQ < as.POSIXct("2023-08-14")) &
    !(IDCOL == 44170 & DHACQ < as.POSIXct("2022-05-25"))
  )

# Remplace les parties foireuses de 44159 et 44170 par des données qui marchent.
GPS59 <- read.table(
  "./0_raw_data/GPS/GPS_Collar44159_20230706143528.csv",
  sep=";",header=F, skip=1,na.strings = "N/A")
GPS59 <- cbind(GPS59, V51 = NA, V52 = NA)
GPS70 <- read.table(
  "./0_raw_data/GPS/GPS_Collar44170_20230706144441.csv",
  sep=";",header=F, skip=1,na.strings = "N/A")
GPS70 <- cbind(GPS70, V51 = NA, V52 = NA)
GPSACQorig <- GPSACQorig %>% rbind(GPS59, GPS70)
rm(GPS59, GPS70)



#### Suppression des plages où deltaT n'est pas constant
# Pour l'instant à la main. Pourrait être précisé avec une automatisation subtile.

GPSACQ <- GPSACQ %>%
  filter(
    !(IDCOL == 44172 & DHACQ > as.POSIXct("2023-01-25"))
  )


################################################################################
#### Formatage
# selection des colonnes d'intÃ©ret
GPSACQ <- GPSACQorig[,c(2,3,4,14,13,15,48,7,16)]
names(GPSACQ)=c("IDCOL","DACQ","HACQ","LON","LAT","HEI","TMP","ORI","DOP")  # Attribution d'un nom aux colonnes
head(GPSACQ)

# Horodatage en une seule colonne
GPSACQ$DHACQ<-paste(GPSACQ$DACQ,GPSACQ$HACQ)
GPSACQ$DHACQ <- ifelse (str_length(GPSACQ$DHACQ)==10,paste(GPSACQ$DHACQ," 00:00:00",sep=""),GPSACQ$DHACQ)
GPSACQ$DHACQ<-as.POSIXct(strptime(GPSACQ$DHACQ,format="%d/%m/%Y %H:%M:%S"),tz="GMT")
GPSACQ <- GPSACQ[,c(1,10,4,5,6,7,8,9)]

# Formatage des donnÃ©es
GPSACQ$LAT<-str_replace_all(as.character(GPSACQ$LAT),",", ".")
GPSACQ$LON<-str_replace_all(as.character(GPSACQ$LON),",", ".")
GPSACQ$HEI<-str_replace_all(as.character(GPSACQ$HEI),",", ".")
GPSACQ$DOP<-str_replace_all(as.character(GPSACQ$DOP),",", ".")
GPSACQ$LON<-as.numeric(GPSACQ$LON)
GPSACQ$LAT<-as.numeric(GPSACQ$LAT)
GPSACQ$HEI<-as.numeric(GPSACQ$HEI)
GPSACQ$DOP<-as.numeric(GPSACQ$DOP)
GPSACQ$ORI<-as.logical(GPSACQ$ORI == "Collar")

# NA omit et formatage
dim(GPSACQ)
GPSACQ<-na.omit(GPSACQ)
dim(GPSACQ)
head(GPSACQ)
table(GPSACQ$IDCOL)
summary(GPSACQ)

##############################################################################
# A.2. Check doublons Ã©ventuels

dim(GPSACQ)
dupli<-duplicated(GPSACQ[,c(1:2)])
summary(dupli)
GPSACQ<-GPSACQ[dupli==F,]
table(GPSACQ$IDCOL)
dim(GPSACQ)
head(GPSACQ)
summary(GPSACQ)


###############################################################################
# A.3. FenÃªtres temporelle et spatiale + retrait iridium

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
# dim(GPSACQ[GPSACQ$HEI>(650),]) # 650 m point le plus haut du SÃ©nÃ©gal

# hist(GPSACQ$TMP)
# GPSACQ<-GPSACQ[GPSACQ$TMP!=0,]

# boxplot(GPSACQ$DOP)
# dim(GPSACQ[GPSACQ$DOP>3,])
# GPSACQ<-GPSACQ[GPSACQ$DOP<3,]

#### Retrait Iridium
dim(GPSACQ)
dim(GPSACQ[GPSACQ$ORI!=T,])
GPSACQ<-GPSACQ[GPSACQ$ORI==T,]
dim(GPSACQ)

dim(GPSACQ)
summary(GPSACQ)


###############################################################################
# A.4. Ajout d'un champ Day / Night int?grant les variations horaires du lever et coucher du soleil

# Calcul des heures de lever et coucher du soleil
lat<-mean(GPSACQ$LAT)
lon<-mean(GPSACQ$LON)
begin<-paste(substr(min(GPSACQ$DHACQ),1,4),substr(min(GPSACQ$DHACQ),6,7),substr(min(GPSACQ$DHACQ),9,10),sep="/")
XX<-as.numeric(round((max(GPSACQ$DHACQ)-min(GPSACQ$DHACQ)),0))

SUNTABLE<-sunrise.set(lat,lon,begin,timezone="UTC", num.days = XX)
#SUNTABLE$sunrise<-SUNTABLE$sunrise+(0*3600) # les donn?es AWT sont exprim?es en UTC+2 -> je rajouter 2 heures ? l'heure de lever UTC
#SUNTABLE$sunset<-SUNTABLE$sunset+(2*3600) # les donn?es AWT sont exprim?es en UTC+2 -> je rajouter 2 heures ? l'heure de coucher UTC

day <- substr((SUNTABLE$sunrise),1,10)
day <- as.POSIXct(strptime(day,format="%Y-%m-%d"),tz="GMT")
SUNTABLE <- cbind(day,SUNTABLE)
head(SUNTABLE)

day <- substr((GPSACQ$DHACQ),1,10)
day <- as.POSIXct(strptime(day,format="%Y-%m-%d"),tz="GMT")
GPSACQ <- cbind(day,GPSACQ)

GPSACQ$id  <- 1:nrow(GPSACQ)
GPSACQ <- merge(GPSACQ,SUNTABLE,by="day", all.x=T ,all.y=F)
GPSACQ <- GPSACQ[order(GPSACQ$id),]
GPSACQ$DN <- ifelse(GPSACQ$DHACQ>=GPSACQ$sunrise & GPSACQ$DHACQ<=GPSACQ$sunset, "DAY","NIGHT")
head(GPSACQ)
GPSACQ <- GPSACQ[,-c(1,10,11,12)]
head(GPSACQ)
rm(SUNTABLE)

################################################################################
# A.Maintien de l'ordre chronologique. 

GPSACQ <- GPSACQ %>% arrange(IDCOL, DHACQ)

table(GPSACQ$IDCOL)
# ggplot(subset(GPSACQ, IDCOL == 44159), aes(x = (1:nrow(subset(GPSACQ, IDCOL == 44159))), y = DHACQ)) +
# ggplot(GPSACQ, aes(x = (1:nrow(GPSACQ)), y = DHACQ)) +
#   geom_point() +
#   labs(title = "Chronological Order Check", x = "id", y = "date") +
#   theme_minimal()
# plot(GPSACQ$DHACQ)



################################################################################
# A.8. Exportation des donnees

# au format txt
GPSACQtoSave <- GPSACQ
GPSACQtoSave$DHACQ<-as.character(GPSACQtoSave$DHACQ)
write.table(GPSACQtoSave,paste0(workd1,"/HAMAC-SN-GPS_brutes.csv"),sep=";", row.names=FALSE)
write.table(GPSACQtoSave,paste0(workd1,"/HAMAC-SN-GPS_brutes.txt"),sep=";", row.names=FALSE)
rm(GPSACQtoSave)



################################################################################
## NE PAS EXECUTER A PARTIR D'ICI - VIEILLE ROUTINE MAINTENANT EN MOVEHMM
################################################################################

# 
# # au format shapefile
# GPSACQ1<-GPSACQ
# GPSACQ1$DHACQ<-as.character(GPSACQ1$DHACQ) # la conversion en shape buggue pour les formats dates -> je passe la date en format character
# #GPSACQ1$dist<-as.character(GPSACQ1$dist)
# #GPSACQ1$Act<-as.character(GPSACQ1$Act)
# #GPSACQ1$dt<-as.character(GPSACQ1$dt)
# #GPSACQ1$speed_KMH<-as.character(GPSACQ1$speed_KMH)
# coordinates(GPSACQ1) = c("LON", "LAT") # conversion au format SpatialPointsDataFrame
# proj4string(GPSACQ1) <- CRS("+proj=utm +zone=28 +north +datum=WGS84") # d?finition de la projection
# filename<- "GPSACQ"
# 
# writeOGR(GPSACQ1, getwd(), filename, driver="ESRI Shapefile", overwrite=T) 
# #write_sf(GPSACQ1, getwd(), filename, driver="ESRI Shapefile", overwrite=T) 
# 
# 
# 
# ################################################################################
# # A.7. REDUCTION A UNE LOC PAR JOUR ET Exportation des donnees
# 
# GPSACQ2<-GPSACQ1
# 
# GPSACQ3<-data.frame(matrix(nrow = 0, ncol = 3))
# colnames(GPSACQ3)<-c("IDCOL","LON","LAT") 
# 
# LIST_NAME <- unique(GPSACQ2$IDCOL)
# for(i in 1:length(LIST_NAME)){
#   #i=1
#   SUB<- subset(GPSACQ2, subset = IDCOL == LIST_NAME[i])
#   head(SUB)
#   SUB$DACQ<-substr(SUB$DHACQ,1,10)
#   LON<-as.data.frame(tapply(SUB$LON,SUB$DACQ, mean))
#   LAT<-as.data.frame(tapply(SUB$LAT,SUB$DACQ, mean))
#   IDCOL<-as.data.frame(tapply(SUB$IDCOL,SUB$DACQ, mean))
#   DATE<- as.data.frame(row.names(IDCOL))
#   
#   SUB1<-cbind(DATE,IDCOL,LON,LAT)
#   colnames(SUB1)<-c("DATE","IDCOL","LON","LAT")
#   
#   GPSACQ3 <-rbind(GPSACQ3,SUB1)
# }
# 
# # Export au format shapefile (pour animation tracking analyst)
# GPSACQ3$DATE<-as.character(GPSACQ3$DATE) # la conversion en shape buggue pour les formats dates -> je passe la date en format character
# coordinates(GPSACQ3) = c("LON", "LAT") # conversion au format SpatialPointsDataFrame
# proj4string(GPSACQ3) <- CRS("+proj=utm +zone=28 +north +datum=WGS84") # definition de la projection
# filename<- "GPSACQ_1loc_per_day"
# writeOGR(GPSACQ3, getwd(), filename, driver="ESRI Shapefile", overwrite=T) 
# 
# 
# ################################################################################
# #A.5. Conversion des coordonnees en UTM
# 
# #library(MASS)
# # library(rgdal)        => DÃ©jÃ  au dessus
# # options(digits = 10)  => DÃ©jÃ  au dessus
# 
# coordinates(GPSACQ) <- ~ LON + LAT
# proj4string(GPSACQ) <- CRS("+proj=longlat +datum=WGS84")
# GPSACQ <- spTransform(GPSACQ, CRS("+proj=utm +zone=28 +north +datum=WGS84"))
# GPSACQ<-as.data.frame(GPSACQ)
# 
# GPSACQ$DHACQ <- as.character(GPSACQ$DHACQ)
# GPSACQ$DHACQ <- ifelse (str_length(GPSACQ$DHACQ)==10,paste(GPSACQ$DHACQ," 00:00:00",sep=""),GPSACQ$DHACQ)
# head(GPSACQ)
# GPSACQ$DHACQ<-as.POSIXct(strptime(GPSACQ$DHACQ,"%Y-%m-%d %H:%M:%S"))
# 
# head(GPSACQ)
# dim(GPSACQ)
# summary(GPSACQ)
# GPSACQ<-na.omit(GPSACQ)
# 
# 
# ################################################################################
# #A.6. recherche et retrait d'outliers 
# 
# 
# ltr <- as.ltraj(xy = GPSACQ[, c("LON", "LAT")], date = GPSACQ$DHACQ, id=GPSACQ$IDCOL)
# ltr <-ld(ltr)
# ltr$speed_KMH <- (ltr$dist/ltr$dt)*3.6
# dim(ltr[ltr$speed_KMH>4.5,]) # nombre de locs > 4,5 km/h
# ltr<-ltr[ltr$speed_KMH<=4.5,] # retrait des locs > 4,5 km/h
# ltr<-ltr[c(11,3,1,2)]
# colnames(ltr)<-c("IDCOL","DHACQ","LON","LAT")
# dim(ltr)
# GPSACQ<-ltr
# GPSACQ<-na.omit(GPSACQ)
# 
# 
# 
# ################################################################################
# #A.7. Ajout de variables attributaires
# 
# ltr <- as.ltraj(xy = GPSACQ[, c("LON", "LAT")], date = GPSACQ$DHACQ, id=GPSACQ$IDCOL)
# ltr <-ld(ltr) 
# ltr$speed_KMH <- (ltr$dist/ltr$dt)*3.6
# ltr$Act <- ifelse(ltr$speed>0.01389,"ACT","INACT")  # seuil : 50m/heure
# dim(ltr)
# ltr<-na.omit(ltr)
# dim(ltr)
# 
# locs2 <- merge(GPSACQ, ltr, by.x = c("IDCOL","DHACQ"), by.y = c("id","date"), all.x = TRUE, all.y=FALSE)
# dim(locs2)
# head(locs2)
# locs2<-na.omit(locs2)
# GPSACQ<-locs2[,c(1,2,4,5,10,11,17,18,3)]
# colnames(GPSACQ)<-c("IDCOL","DHACQ","LON","LAT","DIST_M","DT_SEC","SPEED_KMH","ACT","DN")
# head(GPSACQ)
# dim(GPSACQ)
# 
# 

