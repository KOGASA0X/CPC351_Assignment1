# Define a function to check whether there are duplicate numbers or letters in a row
check_row <- function(row) {
  n <- length(row)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (row[i] == row[j] && row[i] != "C") {
        return(FALSE)  # Returns FALSE if a duplicate (excluding "C") is found in the row
      }
    }
  }
  return(TRUE)  # Returns TRUE if no duplicates are found in the row
}

# Define a function to check whether there are duplicate numbers or letters in a column
check_column <- function(column) {
  n <- length(column)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (column[i] == column[j] && column[i] != "C") {
        return(FALSE)  # Returns FALSE if a duplicate (excluding "C") is found in the column
      }
    }
  }
  return(TRUE)  # Returns TRUE if no duplicates are found in the column
}

# Define a function to check if there are duplicate numbers or letters in a 3x4 block
check_block <- function(block) {
  n <- length(block)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (block[i] == block[j] && block[i] != "C") {
        return(FALSE)  # Returns FALSE if a duplicate (excluding "C") is found in the block
      }
    }
  }
  return(TRUE)  # Returns TRUE if no duplicates are found in the block
}

# Function to read the input files and check Sudoku feasibility
check_sudoku <- function(file_path) {
  input_data <- read.table(file_path, header = FALSE, sep = ";")
  sudoku_matrix <- matrix(nrow = 12, ncol = 12)
  
  # Map values to the 12x12 matrix
  for (i in 1:nrow(input_data)) {
    for (j in 1:ncol(input_data)) {
      sudoku_matrix[i, j] <- input_data[i, j]
    }
  }
  
  # Check rows, columns, and blocks
  for (i in 1:nrow(sudoku_matrix)) {
    if (!check_row(sudoku_matrix[i, ])) {
      cat("Row violation in row:", i, "\n")
      return(FALSE)
    }
  }
  
  for (j in 1:ncol(sudoku_matrix)) {
    if (!check_column(sudoku_matrix[, j])) {
      cat("Column violation in column:", j, "\n")
      return(FALSE)
    }
  }
  
  for (i in seq(1, nrow(sudoku_matrix), by = 3)) {
    for (j in seq(1, ncol(sudoku_matrix), by = 4)) {
      block <- c(sudoku_matrix[i:(i+2), j:(j+3)])
      if (!check_block(block)) {
        cat("Block violation in block starting at row:", i, "and column:", j, "\n")
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}

# Call the function to check the feasibility of Input File 1
if (check_sudoku("Q6/Q6_Input_01.txt")) {
  cat("Input File 1 is a feasible puzzle.")
} else {
  cat("Input File 1 is an infeasible puzzle.")
}

# Call the function to check the feasibility of Input File 2
if (check_sudoku("Q6/Q6_Input_02.txt")) {
  cat("Input File 2 is a feasible puzzle.")
} else {
  cat("Input File 2 is an infeasible puzzle.")
}
