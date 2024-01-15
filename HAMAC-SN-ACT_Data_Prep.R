
#  SENEGAL CATTLE GPS DATA
#  ACT DATA CLEAN AND MERGE CODE 
#  A. SCRIBAN - Janvier 2024


library(stringr)

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())
date()

workd0<-"./0_raw_data/ACT"
workd1<-"./1_Data_clean_and_merge"

################################################################################
## A. Concatenation et mise en forme des donnees ACT acquises
################################################################################

################################################################################
# A.1. Lecture, concatenation et mise en forme des données ACT

# Concaténation
filename<- list.files(workd0,full.names = TRUE)
filename
ACTACQorig<-read.csv(filename[1],sep=";",header=F,skip=1,na.strings = "NA")
for(i in 2:length(filename)) {
  actacq=read.csv(filename[i],sep=";",header=F, skip=1,na.strings = "NA")
  ACTACQorig=rbind(ACTACQorig, actacq)
}
head(ACTACQorig)

# selection des colonnes d'intéret
ACTACQ <- ACTACQorig[,c(2,3,4,12,13,14,15)]
names(ACTACQ)=c("IDCOL","DACQ","HACQ","AcX","AcY","AcZ","TMP")
head(ACTACQ)

# Horodatage en une seule colonne
ACTACQ$DHACQ<-paste(ACTACQ$DACQ,ACTACQ$HACQ)
# ACTACQ$DHACQ <- ifelse (str_length(ACTACQ$DHACQ)==10,paste(ACTACQ$DHACQ," 00:00:00",sep=""),ACTACQ$DHACQ)
# ACTACQ$DHACQ<-as.POSIXct(strptime(ACTACQ$DHACQ,format="%d/%m/%Y %H:%M:%S"),tz="GMT")
ACTACQ$DHACQ<-dmy_hms(ACTACQ$DHACQ)
ACTACQ <- ACTACQ[,c(1,8,4,5,6,7)]

# Formatage des données
# ACTACQ$ORI<-as.logical(ACTACQ$ORI == "Collar")

# NA omit et formatage
dim(ACTACQ)
ACTACQ<-na.omit(ACTACQ)
dim(ACTACQ)
head(ACTACQ)
table(ACTACQ$IDCOL)
summary(ACTACQ)

##############################################################################
# A.2. Check doublons éventuels

dim(ACTACQ)
dupli<-duplicated(ACTACQ[,c(1:2)])
summary(dupli)
ACTACQ<-ACTACQ[dupli==F,]
table(ACTACQ$IDCOL)
dim(ACTACQ)
head(ACTACQ)
summary(ACTACQ)


###############################################################################
# A.3. Fenêtres temporelle et spatiale

#plot(ACTACQ$LON,ACTACQ$LAT,asp=1)
dim(ACTACQ)
ACTACQ<-ACTACQ[ACTACQ$DHACQ>=as.POSIXct("2021-05-09"),]

# boxplot(ACTACQ$TMP)
# dim(ACTACQ[ACTACQ$TMP<0,])
# dim(ACTACQ[ACTACQ$TMP>60,])
# a <- rbind(ACTACQ[ACTACQ$TMP<0,], ACTACQ[ACTACQ$TMP>60,])
# hist(a$TMP, breaks = 10)
ACTACQ<-ACTACQ[ACTACQ$TMP>0,]
ACTACQ<-ACTACQ[ACTACQ$TMP<60,]

dim(ACTACQ)
summary(ACTACQ)


################################################################################
# A.8. Exportation des donnees

# au format txt
ACTACQ$DHACQ<-as.character(ACTACQ$DHACQ)
write.table(ACTACQ,paste0(workd1,"/HAMAC-SN-ACT.csv"),sep=";", row.names=FALSE)
write.table(ACTACQ,paste0(workd1,"/HAMAC-SN-ACT.txt"),sep=";", row.names=FALSE)

