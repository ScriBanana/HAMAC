
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / Manipulation de sorties de mod??le
#  Arthur SCRIBAN & Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)
library(ggplot2)


setwd("/home/scriban/Dropbox/Th√®se/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())


#### Charger un RDS
cheminSorties <- "./2_Fits_outputs/"
modhmm <- readRDS(paste0(cheminSorties ,"240312032645-HAMAC-SN-ModHMM-3Et.rds"))
nbStates <- length(modhmm$mle$stepPar[1,])

## Estimations des maxima de vraisemblance des parametres
modhmm
# Sorties de la fonction d'optimisation :
# modhmm$mod
# AIC du mod????le :
AIC(modhmm)

## Intervalle confiance (95%)
# CI(modhmm)

# Enregistrement des distributions en PDF
repSauvegardes <- "./2_Fits_outputs/"
pdf(paste0(repSauvegardes, "Out_Graphs/", format(Sys.time(), format = "%y%m%d%H%M%S"), '-PlotModHMM.pdf'),
    width = 8, height = 10,
    colormodel = "cmyk",
    paper = "A4")
plotPR(modhmm)
par(mfrow = c(2, 1))
plot(modhmm, plotCI = TRUE, ask = FALSE)
plotStates(modhmm, ask = FALSE)
dev.off()


## Etats ???? chaque point
# A rbinder et ???? concat????ner pour enregistrement et valo ??
# Probabilit????s locales (! moins bien que Viterbi)
sp <- stateProbs(modhmm)
head(sp)

# S????quence d????cod????e
vit <- viterbi(modhmm)
# states[1:25]
#nbStates : le nombre d'?tats dans onglets 7 ? relancer si demande
# R??partition des ??tats
tabprop <- data.frame(
  id = character(nbStates),
  nb = numeric(nbStates),
  prc = numeric(nbStates)
)
for (i in 1:nbStates) {
  tabprop[i, "id"] <- paste0("√âtat ", i)
  tabprop[i, "nb"] <- table(vit)[i]
  tabprop[i, "prc"] <- table(vit)[i]/sum(table(vit))
  print(paste0("Etat ", i,
               " - Nb points : ", table(vit)[i], ", prop : ",
               round(table(vit)[i]/sum(table(vit)) * 100), "%"))
}
tabprop

# treemap(tabprop,
#         index = 1,
#         vSize = 2)
pie(tabprop$nb,
    labels = paste0(
      tabprop$id, "¬†: ",
      round(tabprop$prc * 100), "%"))


# Plot

## Tracer les plots en 2 deux fa?ons soit pour visualisations sans enregistrements  ou avec en pdf
# plot(modhmm, plotCI = TRUE, ask = FALSE)
# Densites de probabilite vs histogrammes
# + prob de transition en fonction des covariables
# + Plot des trajets avec les points de Viterbi (plotTracks = T)

# plotStates(modhmm)
# plotStates(modhmm, animals = "VBT11")

## Probabilite de rester dans chaque etat en fonction des covariables
# plotStationary(modhmm, plotCI = T)

# compute the pseudo-residuals
# pr <- pseudoRes(modhmm)

# time series, qq-plots, and ACF of the pseudo-residuals
# plotPR(modhmm)


################################################################################
#### Manipulations sur les donnees avec les sorties du modele

## Import des donnees
repDonnees <- "./1_Data_clean_and_merge/"
hmmdata <- readRDS(paste0(repDonnees,"/HAMAC-SN-HMMDATA.rds"))

hmmdata$MND <- as.numeric(format(hmmdata$DHACQ, "%j")) / 365 * 12

# COMMENCER ICI

## Ajout des ?tats par Viterbi
# Ajoute aux donnees de sortie une colonne avec les etats selon Viterbi
modhmmdata <- modhmm$data %>% mutate(VIT = vit)
modhmmdata$`(Intercept)` <- NULL

if (length(modhmmdata[, 1]) != length(hmmdata[, 1])) {
  print("ERREUR : Nombre de points diff√©rents dans les donn√©es d'entr√©e et celles du mod√®le")
}

hmmdatavit <- merge(hmmdata, modhmmdata, by = c("ID", "step", "angle", "x", "y"))
hmmdatavit <- hmmdatavit %>% arrange(ID, DHACQ)
head(hmmdatavit)

hmmdatavit$SES <- factor(hmmdatavit$SES, levels = c("SP", "SSf", "SSc")) # TODO mettre les labels
hmmdatavit$DAYTM <- factor(hmmdatavit$DAYTM, labels = c("Nighttime", "Daytime"))
hmmdatavit$TRA <- factor(hmmdatavit$TRA, labels = c("RÈsidents", "Transhumant"))

#### histogramme des Ètats par heure de la journÈe
ggplot(hmmdatavit, aes(x = factor(floor(HRM)), fill = factor(VIT))) +
  facet_grid(SES ~ TRA) +
  geom_bar(stat = "count",
           position = "fill" # Pour stacker ‡ 100%
  ) +
  scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ‡ 100%
  labs(title = "FrÈquence des Ètats par heure et par saison",
       x = "Heure",
       y = "Proportion",
       fill = "Etat")

#### histogramme des Ètats par mois de l'annÈe
ggplot(hmmdatavit, aes(x = factor(ceiling(MND)), fill = factor(VIT))) +
  facet_grid(. ~ TRA) +
  geom_bar(stat = "count",
           position = "fill" # Pour stacker ‡ 100%
  ) +
  scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ‡ 100%
  labs(title = "FrÈquence des Ètats par mois",
       x = "Mois",
       y = "Proportion",
       fill = "Etat")

#### histogramme des Ètats le jour et la nuit
ggplot(hmmdatavit, aes(x = DAYTM, fill = factor(VIT))) +
  facet_grid(SES ~ TRA) +
  geom_bar(stat = "count",
           position = "fill" # Pour stacker ‡ 100%
  ) +
  scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ‡ 100%
  labs(title = "FrÈquence des Ètats le jour et la nuit",
       x = "",
       y = "Proportion",
       fill = "Etat")


#### histogramme des Ètats le jour et la nuit par mois
ggplot(hmmdatavit, aes(x = factor(ceiling(MND)), fill = factor(VIT))) +
  facet_grid(DAYTM ~ TRA) +
  geom_bar(stat = "count",
           position = "fill" # Pour stacker ‡ 100%
  ) +
  scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ‡ 100%
  labs(title = "FrÈquence des Ètats le jour et la nuit par mois",
       x = "Mois",
       y = "Proportion",
       fill = "Etat")


## Sauvegardes
repSauvegardes <- "./2_Fits_outputs/"
saveRDS(hmmdatavit, paste0(repSauvegardes,"/HAMAC-SN-MODHMMDATA.rds"))
write.table(hmmdatavit, paste0(
  repSauvegardes, format(Sys.time(), format = "%y%m%d%H%M%S"),
  "-MODHMMDATA.csv"), sep=";", row.names=FALSE)

for (id in unique(hmmdatavit$ID)) {
  print(id)
  animalData <- subset(hmmdatavit, ID == id)
  write.table(animalData, paste0(
    repSauvegardes, "MODHMMDATA-ParANX/",
    format(Sys.time(), format = "%y%m%d%H%M%S"),
    "-", id,
    "-MODHMMDATA.csv"), sep=";", row.names=FALSE)
}


################################################################################
#### Comparaison de mod??les


repSauvegardes <- "./2_Fits_outputs/"
meilleurModele2e <- readRDS(paste0(repSauvegardes,"Mod2Et0Cov.rds"))
meilleurModele3e <- readRDS(paste0(repSauvegardes,"Mod3Et0Cov.rds"))
meilleurModeleAcc <- readRDS(paste0(repSauvegardes,"Mod3EtAcc.rds"))
AIC(meilleurModele2e, meilleurModele3e, meilleurModeleAcc)
AIC2 <- AIC(meilleurModele2e)
AIC3 <- AIC(meilleurModele3e)
exp((AIC3-AIC2)/2)