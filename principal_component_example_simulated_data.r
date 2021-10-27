# Create data using a random sample
set.seed(42)
# create empty matrix with 100 rows and 10 columns
data = matrix(nrow = 100, ncol = 10)
# change names of columns and rows (not really necessary here but now you now how to deal with that for your later studies)
# Hint: there could be more rules that's why we use 'c()' but we do not need it here
colnames(data) = c(paste('var', 1:10, sep='')) 
rownames(data) = paste('obs', 1:100, sep='')
# Fill the matrix with values 
for (i in 1:100){
  # Poisson distrubution for the sample
  # var.values = rpois(10, lambda = sample(x=10:1000, size = 1))
  # Normal distribution for the sample
  var.values = rnorm(10, mean = 0, sd = 1)
  data[i,] = c(var.values)
}

# Hint: You might need: 't()' which is transposing, since prcomp expects samples to be rows and vars to be columns
# Do the actual PCA Analysis
pca = prcomp(data, scale = TRUE)

# plot x which are the principal components - here we plot the first two principal components in a 2D plot
plot(pca$x[,1], pca$x[,2])

# By construction PC1 accounts for the most variation, PC2 for second most, PC3 for third most..

# Now we calculate the absolute variation and relative variation
pca_var = pca$sdev^2
pca_var_rel = round(pca_var/sum(pca_var)*100, 1)

# Create the Barplot
barplot(pca_var_rel, main = "Barplot of Variation", xlab = "PC", ylab ="Variation in percent")

# Interpretation: PC1 Accounts for almost all of the variation if we use Poisson, but if we use standard normal the varioten is more equally distributed

# Lets see the loading scores: 
loading_scores = pca$rotation[,1]