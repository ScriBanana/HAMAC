
#  SENEGAL CATTLE GPS DATA
#  Teste tous les fichiers des exports
#  A. SCRIBAN & ChatGPT - Janvier 2024

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

# Function to concatenate CSV files with a specific prefix
concatenate_csv_files <- function(directory, prefix, nbCol) {
  # Get the list of files with the specified prefix
  files <- list.files(directory, pattern = paste0("^", prefix, ".*\\.csv$"), recursive = TRUE, full.names = TRUE)
  
  # Initialize an empty data frame
  concatenated_data <- data.frame()
  
  # Loop through each file and concatenate data
  for (file in files) {
    # Read CSV file
    
    current_data <- read.csv(file,sep=";",header=F, skip=1,na.strings = "NA")
    cat(file, ncol(current_data),"\n")
    
    if (ncol(current_data) == (nbCol - 2)) {
      current_data <- cbind(current_data, V51 = NA, V52 = NA)
    }
    
    # Concatenate data
    concatenated_data <- rbind(concatenated_data, current_data)
  }
  
  return(concatenated_data)
}

# Specify the root directory where the CSV files are located
search_directory <- "../../ToutesLesDonneesDepuisLeDebut"

# Concatenate files starting with "GPS-"
gps_table <- concatenate_csv_files(search_directory, "GPS_", 52)

cat("\nGPS Table:\n")
print(head(gps_table))

GPSACQorig <- gps_table
## Transition vers HAMAC-SN-GPS_Data_Prep.R


# Concatenate files starting with "ACT-"
act_table <- concatenate_csv_files(search_directory, "ACT_")

# Print the first few rows of each table
cat("ACT Table:\n")
print(head(act_table))

