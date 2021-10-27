# Create dataset from the lecture
weight = c(89, 73, 65, 100)
size = c(188, 175, 168, 198)

example = cbind(weight, size)

# Do the actual PCA

pca = prcomp(example, scale = TRUE)
plot(pca$x[,1], pca$x[,2])

pca_var_example = pca$sdev^2
pca_var_rel_example = round(pca_var_example/sum(pca_var_example)*100, 1)

# Plot results
barplot(pca_var_rel_example, main = "Barplot of Variation", xlab = "PC", ylab ="Variation in percent")

# Be aware of the fact that this is a really small sample and this code is just to compare it to the PCA we did by hand in the lecture.