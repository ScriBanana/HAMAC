
#  SENEGAL CATTLE GPS DATA
#  Teste tous les fichiers des exports
#  A. SCRIBAN & ChatGPT - Janvier 2024

setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

# Function to concatenate CSV files with a specific prefix
concatenate_csv_files <- function(directory, prefix) {
  # Get the list of files with the specified prefix
  files <- list.files(directory, pattern = paste0("^", prefix), recursive = TRUE, full.names = TRUE)
  
  # Initialize an empty data frame
  concatenated_data <- data.frame()
  
  # Loop through each file and concatenate data
  for (file in files) {
    # Read CSV file
    current_data <- read.csv(file, header = TRUE)
    
    # Concatenate data
    concatenated_data <- rbind(concatenated_data, current_data)
  }
  
  return(concatenated_data)
}

# Specify the root directory where the CSV files are located
root_directory <- "./1_Data_clean_and_merge"

# Concatenate files starting with "ACT-"
act_table <- concatenate_csv_files(root_directory, "ACT-")

# Concatenate files starting with "GPS-"
gps_table <- concatenate_csv_files(root_directory, "GPS-")

# Print the first few rows of each table
cat("ACT Table:\n")
print(head(act_table))

cat("\nGPS Table:\n")
print(head(gps_table))
