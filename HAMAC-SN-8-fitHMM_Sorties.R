
#  SENEGAL CATTLE GPS DATA
#  DATA EXPLORATION CODE / Manipulation de sorties de mod??le
#  Arthur SCRIBAN & Serge NABENEZA - JANVIER 2024


library(moveHMM)
library(dplyr)
library(ggplot2)


setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())


## Charger un RDS
cheminSorties <- "D:/USERS/SergeEtArthur/WorkspaceR/hamac/2_Fits_outputs/"
modhmm <- readRDS(paste0(cheminSorties ,"240127193238-HAMAC-SN-ModHMM-3Et.rds"))

## Estimations des maxima de vraisemblance des param????tres
modhmm
# Sorties de la fonction d'optimisation :
modhmm$mod
# AIC du mod????le :
AIC(modhmm)

## Intervalle confiance (95%)
CI(modhmm)
plot(modhmm, plotCI = TRUE, ask = FALSE) # Densit????s de probabilit???? vs histogrammes
# + prob de transition en fonction des covariables
# + Plot des trajets avec les points de Viterbi (plotTracks = T)


# Enregistrement des distributions en PDF
repSauvegardes <- "./2_Fits_outputs/"
pdf(paste0(repSauvegardes, "Out_Graphs/", format(Sys.time(), format = "%y%m%d%H%M%S"), '-PlotModHMM.pdf'),
    width = 8, height = 10,
    colormodel = "cmyk",
    paper = "A4")
par(mfrow = c(2, 1))
plot(modhmm, plotCI = TRUE, ask = FALSE)
plotPR(modhmm)
dev.off()


## Etats ???? chaque point
# A rbinder et ???? concat????ner pour enregistrement et valo ??
# Probabilit????s locales (! moins bien que Viterbi)
sp <- stateProbs(modhmm)
head(sp)

# S????quence d????cod????e
states <- viterbi(modhmm)
states[1:25]

# R??partition des ??tats
for (i in 1:nbStates) {
  print(paste0("Etat ", i,
               " - Nb points : ", table(states)[i],
               ", prop : ", table(states)[i]/sum(table(states))))
}


# Plot
plotStates(modhmm)
plotStates(modhmm, animals = "VBT11")

## Probabilit???? de rester dans chaque ????tat en fonction des covariables
# plotStationary(modhmm, plotCI = T)

# compute the pseudo-residuals
pr <- pseudoRes(modhmm)

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(modhmm)


#### Manipulations sur le mod????le

# Ajoute aux donn????es une colonne avec les ????tats selon Viterbi
vit <- viterbi(modhmm)
hmmdatavit <- modhmm$data %>% mutate(VIT = vit)
write.table(hmmdatavit, paste0(
  "./2_Fits_outputs/", format(Sys.time(), format = "%y%m%d%H%M%S"),
  "-hmmdatavit.csv"), sep=";", row.names=FALSE)



################################################################################
#### Comparaison de mod??les


repSauvegardes <- "./2_Fits_outputs/"
meilleurModele2e <- readRDS(paste0(repSauvegardes,"240129181847-HAMAC-SN-ModHMM-2Et.rds"))
meilleurModele3e <- readRDS(paste0(repSauvegardes,"240127202639-HAMAC-SN-ModHMM-3Et.rds"))
AIC(meilleurModele2e, meilleurModele3e)
AIC2 <- AIC(meilleurModele2e)
AIC3 <- AIC(meilleurModele3e)
exp((AIC3-AIC2)/2)