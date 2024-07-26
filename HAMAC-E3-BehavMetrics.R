###################
## HAMAC Routine ##
###################

## Diverse metrics on trajectories given the states
## Arthur SCRIBAN & ChatGPS - MAI 2024

### Libraries
library(dplyr)

### Paths
inDir <- "./3_OutData"
graphDir <- "./4_VisualOutputs"
filesPrefix <- "/HAMAC-SN-"


### Functions


### Execution
hmmdatavit <- readRDS(paste0(inDir, filesPrefix, "MODHMMDATA.rds"))
head(hmmdatavit)

## Distances parcourues
filtered_data <- hmmdatavit %>%
  filter(VIT == 2, DAYTM == TRUE, TRA == F)

# Group by ID and date, then calculate the total distance traveled per day
daily_distances <- filtered_data %>%
  group_by(ID, DAT) %>%
  summarise(total_distance = sum(step), .groups = 'drop')

# Calculate the average daily distance traveled for each individual
average_daily_distance <- hmmdatavit %>%
  filter(VIT == 2, DAYTM == TRUE) %>%
  group_by(ID, TRA, DAT) %>%
  summarise(total_distance = sum(step), .groups = 'drop') %>%
  group_by(ID, TRA) %>%
  summarise(average_distance = mean(total_distance), .groups = 'drop')

print(average_daily_distance)

average_distance_by_TRA <- average_daily_distance %>%
  group_by(TRA) %>%
  summarise(average_distance = mean(average_distance), .groups = 'drop')
print(average_distance_by_TRA)

average_distance_by_TRA <- hmmdatavit %>%
  filter(DAYTM == TRUE) %>%
  group_by(ID, TRA, VIT, DAT) %>%
  summarise(total_distance = sum(step), .groups = 'drop') %>%
  group_by(TRA, VIT) %>%
  summarise(average_distance_per_day = mean(total_distance), .groups = 'drop')

## Temps passé par état
time_spent <- hmmdatavit %>%
  group_by(ID, DAT, VIT) %>%
  summarise(interval_count = n(), .groups = 'drop')

# Convert the count of intervals to hours (each interval is 0.5 hours)
time_spent <- time_spent %>%
  mutate(hours_spent = interval_count * 0.5)

# Calculate the average time spent per day in each state for each individual
average_time_spent <- time_spent %>%
  group_by(ID, VIT) %>%
  summarise(average_hours_per_day = mean(hours_spent), .groups = 'drop')

average_time_spent_by_TRA <- hmmdatavit %>%
  group_by(ID, TRA, DAT, VIT) %>%
  summarise(interval_count = n(), .groups = 'drop') %>%
  mutate(hours_spent = interval_count * 0.5) %>%
  group_by(ID, TRA, VIT) %>%
  summarise(average_hours_per_day = mean(hours_spent), .groups = 'drop') %>%
  group_by(TRA, VIT) %>%
  summarise(average_hours_per_day = mean(average_hours_per_day), .groups = 'drop')

# View the result
print(average_time_spent)

#### Intermediate data save
