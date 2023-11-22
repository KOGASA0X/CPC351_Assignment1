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

# Initialize an empty list to store data frames
data_frames <- list()

# For each file
for (i in 1:num_files) {
    # Read the CSV file
    data <- read.csv(paste0("Q4/spotify_", i, ".csv"))
    
    # Add the data frame to the list
    data_frames[[i]] <- data
}

# Initialize an empty list to store merged data frames
merged_data_frames <- list()

# For every 8 files
for (i in seq(1, num_files, by = 8)) {
    # Merge every 8 files into one data frame using the cbind() function
    merged_data_frames[[i %/% 8 + 1]] <- do.call(cbind, data_frames[i:(i+7)])
}

# Merge all the data frames into one data frame using the rbind() function
complete <- do.call(rbind, merged_data_frames)

# Read CSV file
data_csv <- read.csv("tracks_features.csv")

# Convert all factor columns of the data frame to character columns
complete[] <- lapply(complete, function(x) if(is.factor(x)) as.character(x) else x)
data_csv[] <- lapply(data_csv, function(x) if(is.factor(x)) as.character(x) else x)

# Compare the two data frames
difference <- all.equal(complete, data_csv)

# Print the differences
print(difference)