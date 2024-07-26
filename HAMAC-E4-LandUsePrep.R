###################
## HAMAC Routine ##
###################

## Landscape unit association
## Arthur SCRIBAN - FEVRIER 2024

### Libraries
library(dplyr)

### Paths
inDir <- "./0_RawData/METADATA"
outDir <- "./1_IntermeData"


### Functions


### Execution

## Importation métadonnées
legende <- read.csv(
  paste0(inDir, "/Legendesx2Ocusols.csv"),
  sep=";",header = T, skip = 0,na.strings = "#N/A")
## Importation base
BaseOcusols <- read.csv(
  paste0(inDir, "/all_occusols_1_2.csv"),
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

## Partie inutile de légende
OcuSolsLegende$Classe.finale <- NULL
OcuSolsLegende$Légende.1 <- NULL
OcuSolsLegende$Couleur..html..1 <- NULL

head(OcuSolsLegende)
summary(OcuSolsLegende$GRIDCODE)
summary(OcuSolsLegende$Classe.y)
summary(OcuSolsLegende$Classe.x)

## Donne la meilleure classe
OcuSolsLegende$Classe.finale <- ifelse(is.na(OcuSolsLegende$Classe.x),
                                       OcuSolsLegende$Classe.y, OcuSolsLegende$Classe.x)
OcuSolsLegende$Legende.finale <- ifelse(is.na(OcuSolsLegende$Classe.x),
                                        OcuSolsLegende$Légende, OcuSolsLegende$LUSurfaces)

OcuSolsLegende$path <- NULL
OcuSolsLegende$Classe.x <- NULL
OcuSolsLegende$Classe.y <- NULL
OcuSolsLegende$GRIDCODE <- NULL
OcuSolsLegende$LUSurfaces <- NULL
OcuSolsLegende$Légende <- NULL
OcuSolsLegende$Couleur..html. <- NULL

head(OcuSolsLegende)
summary(OcuSolsLegende)
table(OcuSolsLegende$Classe.finale)
table(OcuSolsLegende$Legende.finale)


#### Intermediate data save
write.csv2(OcuSolsLegende, file =  paste0(outDir, "/OcuSolsClasses.csv"),
           row.names = FALSE)
