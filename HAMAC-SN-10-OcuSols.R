
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / Landscape unit association
#  Arthur SCRIBAN - FEVRIER 2024


library(dplyr)
library(ggplot2)

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())

#### Importations données
cheminDonnees <- "../../../Productions/Articles/Mobi/PartageCGPS/OcuSols/"
## Importation métadonnées
legende <- read.csv(
  paste0(cheminDonnees, "Legendesx2Ocusols.csv"),
  sep=";",header = T, skip = 0,na.strings = "#N/A")
## Importation base
BaseOcusols <- read.csv(
  paste0(cheminDonnees, "occusol_part1_2.csv"),
  sep=";",header = T, skip = 0)

head(BaseOcusols)
head(legende)


#### Concaténation
OcuSolsLegende <- merge(BaseOcusols, legende, by = c("GRIDCODE"))
OcuSolsLegende <- OcuSolsLegende %>% arrange(fid)

# Enlève les lignes sans correspondant
# dim(OcuSolsLegende)
# OcuSolsLegende <- OcuSolsLegende[!is.na(OcuSolsLegende$ID),]
# dim(OcuSolsLegende)

OcuSolsLegende$Classe.finale <- NULL
OcuSolsLegende$Légende.1 <- NULL
OcuSolsLegende$Couleur..html..1 <- NULL

head(OcuSolsLegende)
summary(OcuSolsLegende$GRIDCODE)
summary(OcuSolsLegende$Classe.y)
summary(OcuSolsLegende$Classe.x)

## Donne la meilleure classe
OcuSolsLegende$Classe.finale <- ifelse(OcuSolsLegende$Classe.x == 0,
                                       OcuSolsLegende$Classe.y, OcuSolsLegende$Classe.x)
OcuSolsLegende$Legende.finale <- ifelse(OcuSolsLegende$Classe.x == 0,
                                       OcuSolsLegende$Légende, OcuSolsLegende$LUSurfaces)

OcuSolsLegende$Classe.x <- NULL
OcuSolsLegende$Classe.y <- NULL
OcuSolsLegende$GRIDCODE <- NULL
OcuSolsLegende$LUSurfaces <- NULL
OcuSolsLegende$Légende <- NULL
OcuSolsLegende$Couleur..html. <- NULL
table(OcuSolsLegende$Classe.finale)
table(OcuSolsLegende$Legende.finale)


## Sauvegarde
write.csv2(OcuSolsLegende, file =  paste0(cheminDonnees, "OcuSolsClasses.csv"),
            row.names = FALSE)
