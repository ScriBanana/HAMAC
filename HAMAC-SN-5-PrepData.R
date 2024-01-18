
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / Calcul des steps et des angles et préparation du jeu pour les fits
#  Serge NABENEZA & A. SCRIBAN - JANVIER 2024

library(moveHMM)
library(dplyr)
library(lubridate)

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

#### Retrait Iridium
dim(GPS_ACT_par_anx)
dim(GPS_ACT_par_anx[GPS_ACT_par_anx$ORI!=T,])
GPS_ACT_par_anx<-GPS_ACT_par_anx[GPS_ACT_par_anx$ORI==T,]
dim(GPS_ACT_par_anx)

#### Calcul des steps et des angles
hmmdata <- prepData(GPS_ACT_par_anx, type = "LL",coordNames=c("LON","LAT"))
# Step sort en km pour LL, dépend de l'unité d'entrée en UTM

#### Retrait des derniers outliers
# NA omit
nrow(hmmdata)
# hmmdata <- na.omit(hmmdata)  # Pas certain que ce soit nécessaire, en fait

# Suppression d'outliers sur la vitesse
# Désactivé pour l'heure, pour ne pas créer de trous dans la bd

# dim(hmmdata)
# dim(hmmdata[hmmdata$step>2.25,]) # nombre de locs > 4,5 km/h
# hmmdata<-hmmdata[hmmdata$step<=2.25,]
# dim(hmmdata)


#### Assessments visuels
summary(hmmdata)
plot(hmmdata, compact=T)

summary(hmmdata$step)
hist(hmmdata$step, xlab = "step length (km)", main = "",breaks = 50) #, xlim = c(0,2000),ylim=c(0,100000))

hist(hmmdata$angle, breaks = seq(-pi, pi, length = 30), xlab = "angle (rad)", main = "")

#### Sauvegarde en CSV
# NE MARCHE PAS; ne garde pas le type hmmdata que nécessite la fonction de fit
repDonnees<-"./1_Data_clean_and_merge"
write.table(hmmdata,paste0(repDonnees,"/HAMAC-SN-HMMDATA.csv"),sep=";", row.names=FALSE)
