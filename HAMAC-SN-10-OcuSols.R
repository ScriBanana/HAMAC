
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
  paste0(cheminDonnees, "all_occusols_1_2.csv"),
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


## Sauvegarde
write.csv2(OcuSolsLegende, file =  paste0(cheminDonnees, "OcuSolsClasses.csv"),
            row.names = FALSE)

#### REPRISE ICI

## Graphs
OcuSolsLegende <- read.csv2(paste0(cheminDonnees, "OcuSolsClasses.csv"))
# OcuSolsLegende <- OcuSolsLegende[OcuSolsLegende$ID == "VBT51",]

# Labels pour les différentes variables
OcuSolsLegende$SES <- factor(
  OcuSolsLegende$SES,
  levels = c("SSf", "SSc", "SP"),
  labels = c("CDS", "WDS", "RS"))
OcuSolsLegende$DAYTM <- factor(OcuSolsLegende$DAYTM, labels = c("Nighttime", "Daytime"))
OcuSolsLegende$TRA <- factor(OcuSolsLegende$TRA, labels = c("Resident herds", "Transhuming herds"))
OcuSolsLegende$VIT <- factor(OcuSolsLegende$VIT, labels = c("Resting", "Grazing", "Travelling"))
OcuSolsLegende$Legende.finale <- factor(OcuSolsLegende$Legende.finale, labels = c(
  "Trees", "Lowlands", "Dwellings", "Bushfields", "Homefields*", "Rivers",
  "Fallows*", "Gardens*", "Ponds*", "Rangelands", "Roads*", "Naked ground"))

## Figure globale
ggplot(OcuSolsLegende, aes(x = SES, fill = Legende.finale)) +
  facet_grid(VIT ~ TRA) + # Ajouter factor et labels
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion de paysage occupé par état des troupeaux et par saison",
       x = "Season",
       y = "Observations proportion",
       fill = "Occupation du sol")


## Explo
# [OcuSolsLegende$TRA == 'Resident herds',]
ggplot(OcuSolsLegende,
       aes(x = factor(VIT, labels = c("RST", "GRZ", "TVL")), fill = Legende.finale)) +
  facet_grid(
    TRA ~
      SES +
      factor(DAYTM, levels = c("Daytime", "Nighttime"), labels = c("DT", "NT")),
    scales = "free_y"
    ) +
  geom_bar(stat = "count") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  # scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Activity state",
       y = "Amount of observations",
       fill = "Land use")



## Histogramme des distances parcourues la nuit pour les transhumants par éleveur
ggplot(OcuSolsLegende, aes(x =
                             SES,
                             # factor(MON, labels = c(
                             #   "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                             #   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
  y = step,# group = ID,
  fill = VIT)) +
  facet_grid(DAYTM ~ .) +
  geom_col() +
  # scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ? 100%
  labs(title = "Distances observées parcourues par mois",
       x = "Month",
       y = "Distance (km)",
       fill = "Etat") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





