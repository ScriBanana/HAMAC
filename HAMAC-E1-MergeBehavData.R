###################
## HAMAC Routine ##
###################

## Loads a model and the original GPS data to infer behaviours 
## Arthur SCRIBAN - JANVIER 2024

### Libraries
library(moveHMM)
library(dplyr)

### Paths
dataDir <- "./1_IntermeData"
modDir <- "./2_OutFits"
modelFileName <- "240312032645-HAMAC-SN-ModHMM-3Et"
outDir <- "./3_OutData"   
graphDir <- "./4_VisualOutputs"
filesPrefix <- "/HAMAC-SN-"


### Functions


### Execution
hmmdata <- readRDS(paste0(dataDir,"/HAMAC-SN-HMMDATA.rds"))
modhmm <- readRDS(paste0(modDir ,"/", modelFileName, ".rds"))


## Ajout des ?tats par Viterbi
# Ajoute aux donnees de sortie une colonne avec les etats selon Viterbi
vit <- viterbi(modhmm)
modhmmdata <- modhmm$data %>% mutate(VIT = vit)
modhmmdata$`(Intercept)` <- NULL

if (length(modhmmdata[, 1]) != length(hmmdata[, 1])) {
  print("ERREUR : Nombre de points différents dans les données d'entrée et celles du modèle")
}

hmmdatavit <- merge(hmmdata, modhmmdata,
                    by = c("ID", "step", "angle", "x", "y"))
hmmdatavit <- hmmdatavit %>% arrange(ID, DHACQ)
head(hmmdatavit)


#### Data save
saveRDS(hmmdatavit,
        paste0(outDir, filesPrefix, "MODHMMDATA.rds"))
write.table(hmmdatavit, paste0(
    outDir, filesPrefix, format(Sys.time(), format = "%y%m%d%H%M%S"),
    "-MODHMMDATA.csv"
  ), sep=";", row.names=FALSE)

for (id in unique(hmmdatavit$ID)) {
  print(id)
  animalData <- subset(hmmdatavit, ID == id)
  write.table(animalData, paste0(
    outDir, "/MODHMMDATA-PerANX",
    filesPrefix,
    format(Sys.time(), format = "%y%m%d%H%M%S"),
    "-", id,
    "-MODHMMDATA.csv"), sep=";", row.names=FALSE)
}
