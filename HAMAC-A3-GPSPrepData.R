###################
## HAMAC Routine ##
###################

## DATA CLEAN AND MERGE CODE
## Daniel CORNELIS - JUILLET 2023
## Revu par A. SCRIBAN - Janvier 2024

### Libraries
library(StreamMetabolism)
library(data.table)

### Paths
# inDir <- "./0_RawData"
outDir <- "./1_IntermeData"
filesPrefix <- "/HAMAC-SN-"


### Functions


### Execution
# A.4. Ajout d'un champ Day / Night int?grant les variations horaires du lever et coucher du soleil

# Calcul des heures de lever et coucher du soleil
SUNTABLE <- sunrise.set(
  mean(GPSACQ$LAT),
  mean(GPSACQ$LON),
  paste(substr(min(GPSACQ$DHACQ),1,4),substr(min(GPSACQ$DHACQ),6,7),substr(min(GPSACQ$DHACQ),9,10),sep="/"),
  timezone="UTC",
  num.days = as.numeric(round((max(GPSACQ$DHACQ)-min(GPSACQ$DHACQ)),0))
)
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
GPSACQ <- merge(GPSACQ,SUNTABLE, by = "day", all.x = T, all.y = F)
GPSACQ <- GPSACQ %>% arrange(id)
GPSACQ$DAYTM <- ifelse(GPSACQ$DHACQ>=GPSACQ$sunrise & GPSACQ$DHACQ<=GPSACQ$sunset, T, F)
head(GPSACQ)
GPSACQ$day <- NULL
GPSACQ$id <- NULL
GPSACQ$sunrise <- NULL
GPSACQ$sunset <- NULL
head(GPSACQ)
rm(SUNTABLE, day)

################################################################################
# A. Covariable de saison

GPSACQ$MON <- as.numeric(format(GPSACQ$DHACQ, "%m"))

# Hivernage: juin-octobre Saison sèche froide: novembre-février Saison sèche chaude: mars-mai
debutSSc <- 3
debutSP <- 6
debutSSf <- 11

attribSeason <- function(idMois) {
  ifelse(idMois >= debutSSc & idMois < debutSP, "SSc",
         ifelse(idMois >= debutSP & idMois < debutSSf, "SP",
                "SSf"))
}

setDT(GPSACQ)
GPSACQ[, SES := attribSeason(MON)]

GPSACQ$MON <- NULL

################################################################################
# A.Maintien de l'ordre chronologique.

GPSACQ <- GPSACQ %>% arrange(IDCOL, DHACQ)

table(GPSACQ$IDCOL)
# ggplot(subset(GPSACQ, IDCOL == 44159), aes(x = (1:nrow(subset(GPSACQ, IDCOL == 44159))), y = DHACQ)) +
# ggplot(GPSACQ, aes(x = (1:nrow(GPSACQ)), y = DHACQ, color = IDCOL)) +
#   geom_point() +
#   labs(title = "Chronological Order Check", x = "id", y = "date") +
#   theme_minimal()
# plot(GPSACQ$DHACQ)

#### Intermediate data save

GPS <- GPSACQ # Pour Animal segmentation

# au format txt
GPSACQtoSave <- GPSACQ
GPSACQtoSave$DHACQ<-as.character(GPSACQtoSave$DHACQ)
write.table(GPSACQtoSave,paste0(outDir, filesPrefix, "GPS_brutes.csv"),sep=";", row.names=FALSE)
write.table(GPSACQtoSave,paste0(outDir, filesPrefix, "GPS_brutes.txt"),sep=";", row.names=FALSE)
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
# # library(rgdal)        => Déjà au dessus
# # options(digits = 10)  => Déjà au dessus
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
