# Define all the points
points <- list(c(60, 200), c(180, 200), c(80, 180), c(140, 180), c(20, 160), c(100, 160), c(200, 160), c(140, 140), c(40, 120), c(100, 120))

# Calculate the distances between all points
distances <- matrix(nrow = length(points), ncol = length(points))
for (i in 1:length(points)) {
    for (j in 1:length(points)) {
        distances[i, j] <- sqrt(sum((points[[i]] - points[[j]])^2))
    }
}

# Create a data frame to store the distances
distances_df <- data.frame(distances)

# Set the row names and column names of the data frame
rownames(distances_df) <- paste("Point", 1:length(points))
colnames(distances_df) <- paste("Point", 1:length(points))

# Print the data frame
print(distances_df)

# Use the Sorted Edges Algorithm to find the Hamiltonian circuit
edges <- which(lower.tri(distances), arr.ind = TRUE)
edges <- cbind(edges, distances[edges])
edges <- edges[order(edges[,3]),]
tour <- c()
for(i in 1:nrow(edges)) {
    edge <- edges[i, 1:2]
    if (!(edge[1] %in% tour && edge[2] %in% tour && length(unique(c(tour, edge))) < length(tour) + 1)) {
        tour <- unique(c(tour, edge))
    }
    if (length(tour) == length(points)) {
        break
    }
}
tour <- c(tour, tour[1])

# Calculate the total distance of the circuit
total_distance <- sum(distances[tour[-length(tour)], tour[-1]])

# Output the results
print(paste("The tour is:", paste(tour, collapse = " -> ")))
print(paste("The total distance is:", total_distance))

# Create a PNG graphics device
png("Q3/output.png")

# Visualize the Hamiltonian circuit
plot(unlist(lapply(points, function(x) x[1])), unlist(lapply(points, function(x) x[2])), type = "n", las = 2, xlim = c(min(unlist(lapply(points, function(x) x[1]))) - 20, max(unlist(lapply(points, function(x) x[1]))) + 20))

# Mark the points with black dots
points(unlist(lapply(points, function(x) x[1])), unlist(lapply(points, function(x) x[2])), pch = 19, col = "black")

# Add the point numbers next to the points
text(unlist(lapply(points, function(x) x[1])) + 10, unlist(lapply(points, function(x) x[2])), labels = 1:length(points), cex = 1)

# Draw the lines between the points
lines(unlist(lapply(points[tour], function(x) x[1])), unlist(lapply(points[tour], function(x) x[2])))

# Close the graphics device and save the image
dev.off()