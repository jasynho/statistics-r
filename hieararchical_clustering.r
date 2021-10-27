# Create dataset from the lecture

weight = c(89, 73, 65, 100)
size = c(188, 175, 168, 198)

example = cbind(weight, size)

# calculate euclidian distance matrix
euclidian_distances = dist(example, method = 'euclidian')
matrix = round(as.matrix(euclidian_distances), 1)

# do the HAC 
result_hac = hclust(euclidian_distances, method = 'complete')

# Plot results
plot(result_hac)

# Be aware of the fact that this is a really small sample and this code is just to compare it to the HAC we did by hand in the lecture.