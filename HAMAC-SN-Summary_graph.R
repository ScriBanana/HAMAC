
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / SUMMARY GRAPHS
#  Daniel CORNELIS - JUILLET 2023

library(MASS)
library(chron)
library(adehabitatHR)
library(futile.matrix)
library(rgdal)
library(dplyr)


rm(list=ls()) # fonction qui permet de virer tous les objets generes anterieurements
date()
workd4<-"D:/Mes Donnees/AAA-DOSSIERS COURANTS/R/GPS_Processing/Senegal/1_Data_clean_and_merge"


################################################################################
## A. EXPLORATION DES DONNEES ELEPHANTS
################################################################################


## A.0. Parameters

UTM <- "+proj=utm +zone=28 +north +datum=WGS84"
STUDY_DURATION <- 900  # days  (fin décembre 2014)

## A.1. Reads and prepares data

## A.1.1. Reads loc data

setwd(workd4)
LOC<-read.table("SENEGAL_CATTLE.txt",sep=";",header=T, skip=0,na.strings = "N/A")
#LOC$DHACQ <- as.POSIXct(strptime(LOC$DHACQ, "%Y-%m-%d %H:%M:%S"))
LOC<-na.omit(LOC) #  PB : je vire les NAs générés à l'importation des dates qui tombent à 00:00:00

LOC$DACQ <- substr(LOC$DHACQ,1,10)
LOC$DACQ <- as.POSIXct(strptime(LOC$DACQ, "%Y-%m-%d"))


LIST_NAME<-unique(LOC$IDCOL)


## A.1.2. TRAME preparation

STUDY_TERM <- min(LOC$DACQ)+ (STUDY_DURATION*86400)
STUDY_TERM
PERIOD<-as.data.frame(seq(min(LOC$DACQ),STUDY_TERM, by = 86400))
colnames(PERIOD)<- "DACQ"
PERIOD$DACQ <- as.POSIXct(strptime(PERIOD$DACQ, "%Y-%m-%d"))
head(PERIOD)

## Summary graph preparation

NUMROW<-as.numeric(STUDY_TERM-min(LOC$DACQ)+1)
NUMCOL<-length(LIST_NAME)

GRAPH<-matrix(data = NA, nrow = NUMROW, ncol = NUMCOL, byrow = FALSE,dimnames = NULL)
dim(GRAPH)
colnames(GRAPH)<-LIST_NAME
#rownames(GRAPH)<-as.character(seq(min(LOC$DACQ),STUDY_TERM, by = 86400))
rownames(GRAPH)<-as.character(PERIOD$DACQ)

head(GRAPH)


################################################################################

# Subset by collar 

for(i in 1:length(LIST_NAME)){
#i=1

SUB<- subset(LOC, subset = IDCOL == LIST_NAME[i])
str(SUB)

SUB$COUNT<- rep(1,nrow(SUB))

## A.1.1.  synthetic dataframe  (daily rather than hourly)

DACQ<-na.omit(as.data.frame(unique(SUB$DACQ)))
LON<-as.data.frame(tapply(SUB$LON,SUB$DACQ, mean))
LAT<-as.data.frame(tapply(SUB$LAT,SUB$DACQ, mean))
SUCESS_ABS<-as.data.frame(tapply(SUB$COUNT,SUB$DACQ, sum))

SUB1<-cbind(DACQ,LON,LAT,SUCESS_ABS)
colnames(SUB1)<-c("DACQ","LON","LAT","SUCESS_ABS")

SUB1<-merge(PERIOD,SUB1,by.x="DACQ",by.y="DACQ",all.x=T,all.y=F)
SUB1$SUCESS_ABS<-ifelse(is.na(SUB1$SUCESS_ABS),0,1)
head(SUB1)

GRAPH[,i]<-SUB1$SUCESS_ABS
}

### Production of the COMPLETE synthetic graph

dim(GRAPH)
#GRAPH<-t(GRAPH)
head(GRAPH)
colnames(GRAPH)

colnames(GRAPH)<-LIST_NAME
dim(GRAPH)

### Graph

x<-time(SUB1$DACQ)
y<-seq(1,length(LIST_NAME),1)
#myPal <- colorRampPalette( c("red","green") )

filename<-paste(getwd(),"/Out_Graphs/","Synoptic representation of tracking design.jpg",sep="")
jpeg(file=filename,height=21,width=29,units="cm",quality=300,res=300)

par(mar=c(3,6,5,3))
image(x,y,GRAPH,col=c("white","darkgrey"),frame.plot=TRUE,axes=F,xlab="",ylab="",main="",xaxt="n",yaxt="n",breaks=c(0,0.5,1))
day<-as.numeric(format(SUB1$DACQ,"%d"))
label.index1<-which(day==1)
axis(side=3,at=(time(SUB1$DACQ)[label.index1]),labels=c(paste(format(SUB1$DACQ[label.index1],"%b"),format(SUB1$DACQ[label.index1],"%y"))),cex.axis=0.8)
axis(side=1,at=(time(SUB1$DACQ)[label.index1]),labels=c(paste(format(SUB1$DACQ[label.index1],"%b"),format(SUB1$DACQ[label.index1],"%y"))),cex.axis=0.8)
axis(side=2,at=y,labels=colnames(GRAPH),cex.axis=0.8,las=1)
title(main="Suivi GPS de bovins transhumants et sédentaires dans le Ferlo et le Bassin Arachidier du Sénégal (2021-2023)",line=4,cex.main=1)

dev.off()



