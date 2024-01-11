
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / ANIMAL ASSOCIATION
#  Arthur SCRIBAN - JANVIER 2024


setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")


rm(list=ls())
date()

sourceDir <- "./0_raw_data/METADATA/"
ANX <- read.table(paste0(sourceDir, "AnimalSegmentationTable.csv"),sep=";",header=T, skip=0,na.strings = "N/A")

