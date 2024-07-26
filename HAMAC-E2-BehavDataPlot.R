###################
## HAMAC Routine ##
###################

## Plots on behavioural data alone
## Arthur SCRIBAN - AVRIL 2024

### Libraries
library(ggplot2)


### Pathggplot2### Paths
inDir <- "./2_OutFits"
graphDir <- "./4_VisualOutputs"
filesPrefix <- "/HAMAC-SN-"


### Functions


### Execution
hmmdatavit <- readRDS(paste0(inDir, filesPrefix, "MODHMMDATA.rds"))

hmmdatavit$MND <- as.numeric(format(hmmdatavit$DHACQ, "%j")) / 365 * 12
head(hmmdatavit)

# Labels pour les différentes variables
# TODO Fusionner avec OcuSols
hmmdatavit$SES <- factor(
  hmmdatavit$SES,
  levels = c("SSf", "SSc", "SP"),
  labels = c("Cold dry season", "Warm dry season", "Rainy season"))
hmmdatavit$DAYTM <- factor(hmmdatavit$DAYTM, labels = c("Nighttime", "Daytime"))
hmmdatavit$TRA <- factor(hmmdatavit$TRA, labels = c("Resident herds", "Transhumant herds"))
hmmdatavit$VIT <- factor(hmmdatavit$VIT, labels = c("Resting", "Foraging", "Travelling"))

#### histogramme des ?tats par heure de la journ?e
ggplot(hmmdatavit, aes(x = factor(floor(HRM + 1)),
                       fill = VIT)) +
  facet_grid(SES ~ TRA) +
  geom_bar(stat = "count",
           position = "fill" # Pour stacker ? 100%
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Greens") +
  scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ? 100%
  labs(#title = "State frequency",
    x = "Hour of the day",
    y = "Proportions of observations",
    fill = "Activity state") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### histogramme des ?tats par mois de l'ann?e
ggplot(hmmdatavit, aes(x = factor(ceiling(MND)), fill = factor(VIT))) +
  facet_grid(. ~ TRA) +
  geom_bar(stat = "count",
           position = "fill" # Pour stacker ? 100%
  ) +
  scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ? 100%
  labs(#title = "Fr?quence des ?tats par mois",
    x = "Month",
    y = "Proportion of observations",
    fill = "Activity state")

#### histogramme des ?tats le jour et la nuit
ggplot(hmmdatavit, aes(x = DAYTM, fill = factor(VIT))) +
  facet_grid(SES ~ TRA) +
  geom_bar(stat = "count",
           position = "fill" # Pour stacker ? 100%
  ) +
  scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ? 100%
  labs(title = "Fr?quence des ?tats le jour et la nuit",
       x = "",
       y = "Proportion",
       fill = "Etat")


#### histogramme des ?tats le jour et la nuit par mois
ggplot(hmmdatavit, aes(x = factor(ceiling(MND)), fill = factor(VIT))) +
  facet_grid(DAYTM ~ TRA) +
  geom_bar(stat = "count",
           position = "fill" # Pour stacker ? 100%
  ) +
  scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ? 100%
  labs(title = "Fr?quence des ?tats le jour et la nuit par mois",
       x = "Mois",
       y = "Proportion",
       fill = "Etat")

## Heatmap des fréquences d'état par heure
ggplot(hmmdatavit, aes(x = VIT, y = factor(floor(HRM)))) +
  facet_grid(. ~ SES) +
  geom_bin2d() +
  labs(x = "VIT Value", y = "Hour", fill = "Frequency") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## Histogramme des distances parcourues par mois
ggplot(hmmdatavit, aes(x = factor(ceiling(MND), labels = c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
  y = step, group = ID, fill = factor(VIT))) +
  # facet_grid(. ~ TRA) +
  geom_boxplot(   ) +
  # scale_y_continuous(labels = scales::percent_format()) + # Pour stacker ? 100%
  labs(title = "Distances observées parcourues par mois",
       x = "Mois",
       y = "Distance (km)",
       fill = "Etat")


#### Intermediate data save
