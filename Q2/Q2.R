#Q2
# Initialize variables to store word counts
analytics_num <- 0
insight_num <- 0
of_num <- 0

# Function to count words in a given text from file
count_words <- function(text) {
  # Convert text to lowercase for case-insensitive matching
  text <- tolower(text)
  
  # Tokenise the text into words
  words <- unlist(strsplit(text, "\\W+"))
  
  # Count occurrences of specified words
  analytics_num <<- analytics_num + sum(words == "analytics")
  insight_num <<- insight_num + sum(words == "insight")
  of_num <<- of_num + sum(words == "of")
}

# Loop through all the text file
for (i in 1:10) {
  # Read the content of the file
  file_path <- paste("Q2/Q2_Part_", sprintf("%02d", i), ".txt", sep="")
  file_content <- tolower(readLines(file_path, warn = FALSE))
  
  # Combine lines into a single string
  file_text <- paste(file_content, collapse = " ")
  
  # Count words in the text
  count_words(file_text)
}

#Print the word count of "analytics", "insight", "of" 
cat("Total number count of 'analytics':", analytics_num, "\n")
cat("Total number count of 'insight':", insight_num, "\n")
cat("Total number count of 'of':", of_num, "\n")