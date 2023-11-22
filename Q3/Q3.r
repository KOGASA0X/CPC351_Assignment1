# Define all the points
points <- list(c(60, 200), c(180, 200), c(80, 180), c(140, 180), c(20, 160), c(100, 160), c(200, 160), c(140, 140), c(40, 120), c(100, 120))

# Calculate the distances between all points
distances <- matrix(nrow = length(points), ncol = length(points))
for (i in 1:length(points)) {
    for (j in 1:length(points)) {
        distances[i, j] <- sqrt(sum((points[[i]] - points[[j]])^2))
    }
}

# Use the Nearest Neighbor Algorithm to find the Hamiltonian circuit
tour <- c(1)
while (length(tour) < length(points)) {
    last_point <- tour[length(tour)]
    distances[last_point, tour] <- Inf
    next_point <- which.min(distances[last_point, ])
    tour <- c(tour, next_point)
}
tour <- c(tour, 1)

# Calculate the total distance of the circuit
total_distance <- sum(distances[tour[-length(tour)], tour[-1]])

# Output the results
print(paste("The tour is:", paste(tour, collapse = " -> ")))
print(paste("The total distance is:", total_distance))

# Create a PNG graphics device
png("output.png")

# Visualize the Hamiltonian circuit
plot(unlist(lapply(points, function(x) x[1])), unlist(lapply(points, function(x) x[2])), type = "n", las = 2)
text(unlist(lapply(points, function(x) x[1])), unlist(lapply(points, function(x) x[2])), labels = 1:length(points), cex = 0.7)
lines(unlist(lapply(points[tour], function(x) x[1])), unlist(lapply(points[tour], function(x) x[2])))

# Close the graphics device and save the image
dev.off()