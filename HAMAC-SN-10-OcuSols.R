
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / Landscape unit association
#  Arthur SCRIBAN - FEVRIER 2024


library(dplyr)
library(ggplot2)

setwd("~/Dropbox/These/GPS/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())

#### Importations données
cheminDonnees <- "../PartageCGPS/OcuSols/"


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
cheminDonnees <- "../PartageCGPS/OcuSols/"
OcuSolsLegende <- read.csv2(paste0(cheminDonnees, "OcuSolsClasses.csv"))
# OcuSolsLegende <- OcuSolsLegende[OcuSolsLegende$ID == "VBT51",]

# Labels pour les différentes variables
OcuSolsLegende$SES <- factor(OcuSolsLegende$SES,
                             levels = c("SSf", "SSc", "SP"),
                             labels = c("Cold dry season", "Warm dry season", "Rainy season"))
OcuSolsLegende$DAYTM <- factor(OcuSolsLegende$DAYTM,
                               labels = c("Nighttime", "Daytime"))
OcuSolsLegende$TRA <- factor(OcuSolsLegende$TRA,
                             labels = c("Resident herds", "Transhumant herds"))
OcuSolsLegende$VIT <- factor(OcuSolsLegende$VIT,
                             labels = c("Resting", "Foraging", "Travelling"))
OcuSolsLegende$Legende.courte <- factor(OcuSolsLegende$Legende.finale, labels = c(
  
  # Arbres           Bas fonds    Bâti     Champs de brousse 
  # Champs de cases  Cours d'eau  Jachère  Jardins 
  # Mares            Parcours     Routes   Sol nu 
  
  # "Trees", "Lowlands", "Dwellings", "Bushfields",
  # "Homefields*", "Rivers", "Fallows*", "Gardens*",
  # "Ponds*", "Rangelands", "Roads*", "Naked ground"))
  
  "Cropland trees*", "Lowlands", "Homefields*", "Bushfields",
  "Homefields*", "Lowlands", "Fallows*", "Other",
  "Lowlands", "Rangelands", "Other", "Bushfields"))

## Explo
# [OcuSolsLegende$TRA == 'Resident herds',]
ggplot(OcuSolsLegende,
       aes(x = factor(VIT, labels = c("RST", "FRG", "TVL")),
           fill = factor(Legende.courte, levels = c(
             "Other", "Lowlands", "Rangelands", "Fallows*", "Bushfields", "Homefields*")))) +
  facet_grid(
    TRA ~
      SES +
      factor(DAYTM, levels = c("Daytime", "Nighttime"), labels = c("DT", "NT")),
    scales = "free_y"
    ) +
  geom_bar(stat = "count") +
  scale_y_continuous(
    name = "Amount of observations",
    labels = function(x) paste0(x / 1000, "k"),
    # sec.axis = sec_axis(~ . * 100, name = "Percentage of observations")
  ) +
  scale_fill_brewer(palette = "BrBG") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(x = "Activity state",
       y = "Amount of observations",
       fill = "Land use")

### Alternative avec des pourcentages
OcuSolsLegendePercent <- OcuSolsLegende %>%
  group_by(TRA, SES, DAYTM, VIT, Legende.courte) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(TRA, SES, DAYTM) %>%
  mutate(percentage = count / sum(count))

ggplot(OcuSolsLegendePercent,
       aes(x = factor(VIT, labels = c("RST", "FRG", "TVL")), y = percentage,
           fill = factor(Legende.courte, levels = c(
             "Other", "Rangelands", "Lowlands", "Cropland trees*",
             "Fallows*", "Bushfields", "Homefields*")))) +
  facet_grid(
    SES ~ TRA + factor(DAYTM, levels = c("Daytime", "Nighttime")),
    scales = "free_y"
  ) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "#666666", "#018571", "#80cdc1", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a"
    )) +
  # scale_fill_brewer(palette = "BrBG", direction=-1) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Activity state",
       y = "Proportions of observations in each state per case",
       fill = "Land use")


## Figure globale
ggplot(OcuSolsLegende, aes(x = SES, fill = Legende.courte)) +
  facet_grid(VIT ~ TRA) + # Ajouter factor et labels
  geom_bar(position = "fill", stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion de paysage occupé par état des troupeaux et par saison",
       x = "Season",
       y = "Observations proportion",
       fill = "Occupation du sol")



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


# Fertility transfers
PerSESTRA <- OcuSolsLegendePercent %>%
  group_by(TRA, VIT, SES, Legende.courte, DAYTM) %>%
  summarise(total_count = sum(count), .groups = 'drop')

CondensPerSESTRA <- PerSESTRA[PerSESTRA$TRA == "Resident herds",]
CondensPerSESTRA <- CondensPerSESTRA[CondensPerSESTRA$SES != "Rainy season",]
CondensPerSESTRA <- CondensPerSESTRA %>%
  group_by(DAYTM, Legende.courte) %>%
  summarise(total_count = sum(total_count), .groups = 'drop')


ggplot(OcuSolsLegendePercent,
       aes(x = DAYTM, y = percentage,
           fill = factor(Legende.courte, levels = c(
             "Other", "Rangelands", "Lowlands", "Cropland trees*",
             "Fallows*", "Bushfields", "Homefields*")))) +
  facet_grid(
    SES ~ TRA,
    scales = "free_y"
  ) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "#666666", "#018571", "#80cdc1", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a"
  )) +
  # scale_fill_brewer(palette = "BrBG", direction=-1) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Activity state",
       y = "Proportions of observations in each state per case",
       fill = "Land use")

