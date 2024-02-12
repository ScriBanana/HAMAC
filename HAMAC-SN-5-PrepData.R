
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / Calcul des steps et des angles et préparation du jeu pour les fits
#  Serge NABENEZA & A. SCRIBAN - JANVIER 2024

library(moveHMM)
library(dplyr)
library(lubridate)
library(ggplot2)

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())


#### Importation données classées par animal
repDonnees <- "./1_Data_clean_and_merge/"
GPS_ACT_par_anx <- read.table(
  paste0(repDonnees, "HAMAC-SN-GPSnACTpANX.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")
GPS_ACT_par_anx$DHACQ<-ymd_hms(GPS_ACT_par_anx$DHACQ)
head(GPS_ACT_par_anx)

#### Calcul des steps et des angles
debPrepData <- Sys.time()
hmmdata <- prepData(GPS_ACT_par_anx, type = "LL",coordNames=c("LON","LAT"))
# Step sort en km pour LL, dépend de l'unité d'entrée en UTM
print(Sys.time() - debPrepData) # Moins de 2 min


#### NA omit
nrow(hmmdata)
# na.omit, mais sans retirer les premiers points :
hmmdata <- hmmdata[complete.cases(hmmdata[, -which(colnames(hmmdata) == "angle")]),]

#### Sauvegarde avec outliers
repDonnees<-"./1_Data_clean_and_merge"
saveRDS(hmmdata, paste0(repDonnees,"/HAMAC-SN-HMMDATA_avec_outliers_vitesse.rds"))



#### Alternative de calcul des steps pour v?rifications
# haversine <- function(lon1, lat1, lon2, lat2) {
#   # Calcule la distance entre deux points sur la surface d'une sph?re. (Full ChatGPT, ofc)
#   R <- 6371 #rayon de la terre
#   d_lon <- (lon2 - lon1) * (pi/180)
#   d_lat <- (lat2 - lat1) * (pi/180)
#   
#   a <- sin(d_lat/2)^2 + cos(lat1 * (pi/180)) * cos(lat2 * (pi/180)) * sin(d_lon/2)^2
#   c <- 2 * atan2(sqrt(a), sqrt(1 - a))
#   dist <- R * c
#   return(dist)
# }
# 
# datatoplot <- GPS_ACT_par_anx %>% mutate(DIST = haversine(lag(LON), lag(LAT), LON, LAT))
# hist(datatoplot$DIST, xlab = "Dist de Haversine", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))

# Histogramme des deltaT
# datadeltaT <- GPS_ACT_par_anx %>% mutate(DELTAT = as.numeric((lag(DHACQ) - DHACQ) / 60))
# boxplot(datadeltaT$DELTAT, xlab = "deltaT (min)", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))
# hist(datadeltaT$DELTAT, xlab = "deltaT (min)", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))
# summary(datadeltaT$DELTAT)


#### Suppression d'outliers sur la vitesse
# Désactivé pour l'heure, pour ne pas créer de trous dans la bd

dim(hmmdata)
dim(hmmdata[hmmdata$step>2.25,]) # nombre de locs > 4,5 km/h
hmmdata<-hmmdata[hmmdata$step<=2.25,]
dim(hmmdata)


#### Assessments visuels
summary(hmmdata)
plot(hmmdata) #, compact=T)

summary(hmmdata$step)

ggplot(hmmdata, aes(x = (1:nrow(hmmdata)), y = DHACQ, color = ID)) +
  geom_point() +
  labs(title = "Constance des DeltaT", x = "id", y = "date") +
  theme_minimal()



#### Enregistrement des distributions en PDF
pdf(paste0(repDonnees, "/Out_Graphs/", format(Sys.time(), format = "%y%m%d"), '-DistriStepEtAngleHMMdata.pdf'),
    width = 8, height = 10,
    colormodel = "cmyk",
    paper = "A4")
par(mfrow = c(2, 1))
hist(hmmdata$step, xlab = "step length (km)", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))
hist(hmmdata$angle, breaks = seq(-pi, pi, length = 30), xlab = "angle (rad)", main = "")
dev.off()

#### Sauvegarde en RDS
repDonnees<-"./1_Data_clean_and_merge"
saveRDS(hmmdata, paste0(repDonnees,"/HAMAC-SN-HMMDATA.rds"))
write.table(hmmdata,paste0(repDonnees,"/HAMAC-SN-HMMDATA.csv"),sep=";", row.names=FALSE)

