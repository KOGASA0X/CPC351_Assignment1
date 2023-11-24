# Define a function to check whether there are duplicate numbers or letters in a row
check_row <- function(row) {
  n <- length(row)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (row[i] == row[j] && row[i] != "C")  {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Define a function to check whether there are duplicate numbers or letters in a column
check_column <- function(column) {
  n <- length(column)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (column[i] == column[j] && column[i] != "C") {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Define a function to check if there are duplicate numbers or letters in a 3x4 block
check_block <- function(block) {
  n <- length(block)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (block[i] == block[j] && block[i] != "C") {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Read the input file 1, map numbers and letters to the matrix
input1 <- read.table("Q6/Q6_Input_01.txt", header = FALSE, sep = ";")
matrix1 <- matrix(nrow = 12, ncol = 12)
for (i in 1:nrow(input1)) {
  for (j in 1:ncol(input1)) {
    matrix1[i, j] <- input1[i, j]
  }
}

# Read the input file 2 and check whether Sudoku is feasible
input2 <- read.table("Q6/Q6_Input_02.txt", header = FALSE, sep = ";")  
for (i in 1:nrow(input2)) {  
  row <- input2[i, ]  
  if (!check_row(matrix1[i, ])) {  
    cat("Row violation:", i, "\n")  
    return(FALSE)  
  }  
  if (!check_column(matrix1[, i])) {  
    cat("Column violation:", i, "\n")  
    return(FALSE)  
  }  
  if (!check_block(matrix1[(i-3)*4+1:(i-3)*4+4, ])) {  
    cat("Block violation:", i, "\n")  
    return(FALSE)  
  }  
}  

# Check rows, columns, and blocks of a matrix
check_matrix <- function(matrix) {
  for (i in 1:nrow(matrix)) {
    if (!check_row(matrix[i, ])) {
      return(FALSE)
    }
  }
  for (j in 1:ncol(matrix)) {
    if (!check_column(matrix[, j])) {
      return(FALSE)
    }
  }
  for (i in seq(1, nrow(matrix), by=3)) {
    for (j in seq(1, ncol(matrix), by=4)) {
      block <- c(matrix[i:(i+2), j:(j+3)])
      if (!check_block(block)) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Call the function to check the matrix
if (check_matrix(matrix1)) {
  cat("Matrix 1 is valid.")
} else {
  cat("Matrix 1 is invalid.")
}