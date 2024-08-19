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
# outDir <- "./3_OutData"

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


## Data cleaning for dataverse posting
# OcuSolsLegende$path <- NULL
# OcuSolsLegende$Couleur..html. <- NULL
# OcuSolsLegende$Classe.x <- NULL
# OcuSolsLegende$LUSurfaces <- NULL
# OcuSolsLegende$AcX.x <- NULL
# OcuSolsLegende$AcY.x <- NULL
# OcuSolsLegende$AcZ.x <- NULL
# OcuSolsLegende$AcX.y <- NULL
# OcuSolsLegende$AcY.y <- NULL
# OcuSolsLegende$AcZ.y <- NULL
# OcuSolsLegende$layer <- NULL
# OcuSolsLegende$DAY <- NULL
# OcuSolsLegende$MON <- NULL
# OcuSolsLegende$YER <- NULL
# OcuSolsLegende$DHACQ <- NULL
# OcuSolsLegende$IDVIL <- NULL
# OcuSolsLegende$GRIDCODE <- NULL
# OcuSolsLegende$IDELV <- NULL
# OcuSolsLegende$GPS_TMP <- NULL
# OcuSolsLegende$Classe.y <- NULL
# 
# OcuSolsLegende <- rename(OcuSolsLegende, ANID = ID)
# OcuSolsLegende <- rename(OcuSolsLegende, FID = fid)
# OcuSolsLegende <- rename(OcuSolsLegende, LON = x)
# OcuSolsLegende <- rename(OcuSolsLegende, LAT = y)
# OcuSolsLegende <- rename(OcuSolsLegende, STEP = step)
# OcuSolsLegende <- rename(OcuSolsLegende, ANGLE = angle)
# OcuSolsLegende <- rename(OcuSolsLegende, OCUSOL = Légende)
# 
# table(OcuSolsLegende$OCUSOL)
# OcuSolsLegende <- OcuSolsLegende %>%
#   mutate(OCUSOL = recode(OCUSOL,
#                          "Champs de brousse" = "Bushfields",
#                          "Cours d'eau" = "River",
#                          "Sol nu" = "Naked ground",
#                          "Bas fonds" = "Lowlands",
#                          "Arbres" = "Trees",
#                          "Parcours" = "Rangelands"))
# 
# head(OcuSolsLegende)
# 
# OcuSolsLegende <- select(OcuSolsLegende, FID, ANID, TRA, IDCOL, DAT, HUR, LON, LAT, HEI, DOP, STEP, ANGLE, VIT, TMP, SES, DAYTM, OCUSOL, everything())
# 
# head(OcuSolsLegende)
# 
# 
# write.csv2(OcuSolsLegende, file =  paste0(outDir, "/240819-GPSHMMSen.csv"),
#            row.names = FALSE)


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
