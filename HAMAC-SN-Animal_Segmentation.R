
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / ANIMAL ASSOCIATION
#  Arthur SCRIBAN - JANVIER 2024


setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")


rm(list=ls())
date()

# Importation données
metaSourceDir <- "./0_raw_data/METADATA/"
ANX <- read.table(
  paste0(metaSourceDir, "AnimalSegmentationTable.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")

gpsSourceDir <- "./1_Data_clean_and_merge/"
GPS <- read.table(
  paste0(gpsSourceDir, "HAMAC-SN-GPS.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")


actSourceDir <- "./1_Data_clean_and_merge/"
ACT <- read.table(
  paste0(actSourceDir, "HAMAC-SN-ACT.csv"),
  sep=";",header=T, skip=0,na.strings = "N/A")

# Segmentation



