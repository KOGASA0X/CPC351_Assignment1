# Input solution vector
solution <- c(2, 4, 6, 8, 10, 1, 3, 5, 7, 9)

# Create a 10x10 matrix to represent the chessboard
chessboard <- matrix(0, nrow = 10, ncol = 10)

# Map the input vector to the chessboard
for (i in 1:length(solution)) {
    chessboard[i, solution[i]] <- 1
}

# Check if any two queens are in the same row, same column, or same diagonal
is_feasible <- TRUE
for (i in 1:(length(solution) - 1)) {
    for (j in (i + 1):length(solution)) {
        # If two queens are in the same column or same diagonal, the solution is infeasible
        if (solution[i] == solution[j] || abs(i - j) == abs(solution[i] - solution[j])) {
            is_feasible <- FALSE
            break
        }
    }
    if (!is_feasible) {
        break
    }
}

# Print the result
if (is_feasible) {
    print("The solution is feasible.")
} else {
    print("The solution is infeasible.")
}