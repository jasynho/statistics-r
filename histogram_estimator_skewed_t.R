#install.packages("skewt")
library(skewt)
set.seed(73)

# draw sample from skewed t disrtibution
draw_skewed_t = rskt(n = 50, df = 6, gamma = 0.4)

# creation vector of true densities for the sample
true_density = dskt(draw_skewed_t, df = 6, gamma = 0.4)
plot(function(x)dskt(x, df = 6, gamma = 0.4),-10,4, main = "Plot of true pdf and our random draws", xlab = "", ylab = "")
points(draw_skewed_t, true_density)

# Plotting the histogram estimate, the true density and our sample values 
hist(draw_skewed_t, probability = T, xlab = "", main = "Random sample, histogram estimator, true density")
plot(function(x)dskt(x, df = 6,gamma = 0.4),-10,4, main = "", xlab = "", ylab = "", add = TRUE)
points(draw_skewed_t, true_density)
legend("topleft", legend = c("True density", "Histrogram estimate", "Random sample"),
       col=c("black", "gray", "black"),lty=c(1,1,3), cex = 0.7)

