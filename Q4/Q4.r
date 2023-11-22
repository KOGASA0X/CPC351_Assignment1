# Read CSV file
data <- read.csv("tracks_features.csv")

# Calculate the number of files
num_files <- 40

# For each file
for (i in 1:num_files) {
    # Calculate the range of columns
    start_col <- ((i - 1) %% (ncol(data) / 3)) * 3 + 1
    end_col <- min(start_col + 2, ncol(data))
    
    # Calculate the range of rows
    start_row <- ((i - 1) %/% (ncol(data) / 3)) * 250000 + 1
    end_row <- min(start_row + 249999, nrow(data))
    
    # Select the corresponding rows and columns
    subset <- data[start_row:end_row, start_col:end_col]
    
    # Write to a new CSV file
    write.csv(subset, paste0("Q4/spotify_", i, ".csv"), row.names = FALSE)
}