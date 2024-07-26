###################
## HAMAC Routine ##
###################

## Model outputs
## Arthur SCRIBAN & Serge NABENEZA - JANVIER 2024

### Libraries
library(moveHMM)

### Paths
inDir <- "./2_OutFits"
modelFileName <- "240312032645-HAMAC-SN-ModHMM-3Et"
graphDir <- "./4_VisualOutputs"

### Functions


### Execution

#### Charger un RDS
modhmm <- readRDS(paste0(inDir ,"/", modelFileName, ".rds"))
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
pdf(paste0(graphDir, "/", modelFileName, '-PlotModHMM.pdf'),
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
  tabprop[i, "id"] <- paste0("État ", i)
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
      tabprop$id, " : ",
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


## Comparaison de mod??les
meilleurModele2e <- readRDS(paste0(inDir,"Mod2Et0Cov.rds"))
meilleurModele3e <- readRDS(paste0(inDir,"Mod3Et0Cov.rds"))
meilleurModeleAcc <- readRDS(paste0(inDir,"Mod3EtAcc.rds"))
AIC(meilleurModele2e, meilleurModele3e, meilleurModeleAcc)
AIC2 <- AIC(meilleurModele2e)
AIC3 <- AIC(meilleurModele3e)
exp((AIC3-AIC2)/2)



#### Intermediate data save
