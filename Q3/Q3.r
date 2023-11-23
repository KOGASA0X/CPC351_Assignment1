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

# Initialize tour
tour <- c(edges[1, 1:2])

# Initialize the head and tail of tour
head <- tour[1]
tail <- tour[2]

# Initialize a variable to track if tour is updated in the loop
updated <- TRUE

while (updated) {
    # Assume tour won't be updated at the beginning of each loop
    updated <- FALSE
    
    for(i in 1:nrow(edges)) {
        edge <- edges[i, 1:2]
        
        # If the value in the first column of edge equals the head of tour and the value in the second column is not in tour
        if (edge[1] == head && !(edge[2] %in% tour)) {
            # Insert the value in the second column of edge at the head of tour
            tour <- c(edge[2], tour)
            # Update the head of tour
            head <- edge[2]
            # Mark that tour has been updated
            updated <- TRUE
            break
        }
        
        # If the value in the second column of edge equals the head of tour and the value in the first column is not in tour
        if (edge[2] == head && !(edge[1] %in% tour)) {
            # Insert the value in the first column of edge at the head of tour
            tour <- c(edge[1], tour)
            # Update the head of tour
            head <- edge[1]
            # Mark that tour has been updated
            updated <- TRUE
            break
        }
        
        # If the value in the first column of edge equals the tail of tour and the value in the second column is not in tour
        if (edge[1] == tail && !(edge[2] %in% tour)) {
            # Insert the value in the second column of edge at the tail of tour
            tour <- c(tour, edge[2])
            # Update the tail of tour
            tail <- edge[2]
            # Mark that tour has been updated
            updated <- TRUE
            break
        }
        
        # If the value in the second column of edge equals the tail of tour and the value in the first column is not in tour
        if (edge[2] == tail && !(edge[1] %in% tour)) {
            # Insert the value in the first column of edge at the tail of tour
            tour <- c(tour, edge[1])
            # Update the tail of tour
            tail <- edge[1]
            # Mark that tour has been updated
            updated <- TRUE
            break
        }
    }
}
tour <- c(tour, tour[1])

# Initialize the total distance to 0
total_distance <- 0

# Iterate through each point in the traveler's path
for (i in 1:(length(tour) - 1)) {
    # Calculate the distance between the current point and the next point
    distance <- distances[tour[i], tour[i + 1]]
    
    # Add the current distance to the total distance
    total_distance <- total_distance + distance
}

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