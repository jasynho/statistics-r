# Kernel density estimation
library(skewt)

# draw sample from skewed t distribution
set.seed(73)
draw_skewed_t = rskt(n = 50, df = 6, gamma = 0.4)
true_density = dskt(draw_skewed_t, df = 6, gamma = 0.4)

######## kde using density command from R ###########
estimated_density_gauss = density(draw_skewed_t, kernel = "gaussian")
estimated_density_epan = density(draw_skewed_t, kernel = "epanechnikov")
estimated_density_trian = density(draw_skewed_t, kernel = "triangular")
hist(draw_skewed_t, probability = T, xlab = "", main = "Different estimates and true data")
points(draw_skewed_t, true_density)
plot(function(x)dskt(x, df=6,gamma=0.4),-10,4, main = "", xlab = "", ylab = "", add = TRUE)
lines(estimated_density_gauss, lwd = 2, col = "red")
lines(estimated_density_epan, lwd = 2, col = "blue")
lines(estimated_density_trian, lwd = 2, col = "green")
legend("topleft", legend = c("True density", "Epanechnikov kernel", "Gaussian kernel", "Triangular kernel"),
       col=c("black", "red", "blue", "green"),lty=c(1,1,1,1), cex = 0.5)


########### manual kernel density estimation ############
# Define the kernel functions you intend to use: 
# Epanechnikov Kernel
Epan = function(u){
  f = 3/4 * (1-u^2) * (abs(u)<=1)
  return(f)
}

# Gaussian Kernel
Gaussian = function(u){
  f = 1/sqrt(2*pi)*exp(-0.5*u^2)
  return(f)
}

# Triangular Kernel
Triangular = function(u){
  f = (1 - abs(u)) * (abs(u)<=1)
}

# function that performs kernel density estimation
kernel_estimation = function(data, h, x0, kernel_type){
  # data is a data vector
  # h is the bandwidth
  # x0 is the value we subtract inside the kernel function
  # kernel_type tells us which kernel function to use
  n = length(data)
  if(kernel_type == "Gaussian"){
    f = sum(1/n * 1/h * Gaussian((data-x0)/h))
    return(f)
  } else if(kernel_type == "Epanechnikov"){
    f = sum(1/n * 1/h * Epan((data-x0)/h))
    return(f)
  } else if(kernel_type == "Triangular"){
    f = sum(1/n * 1/h * Triangular((data-x0)/h))
    return(f)
  } else {
    return("Warning: The kernel type is invalid")
  }
}

# Simulate data set for testing the function
test_data_kernel_estimation = rnorm(10,1,4)
test_data_kernel_estimation
# See function output
kernel_estimation(test_data_kernel_estimation, 0.5,0, "Gaussian")
kernel_estimation(test_data_kernel_estimation, 0.5,0, "Epanechnikov")
kernel_estimation(test_data_kernel_estimation, 0.5,0, "Triangular")

####### plotting the different kernels and a histogram estimator for comparison
# grid is a set of x0 which will be subtracted in "kernel_estimation"
grid = seq(from=-10, to = 4, by = 0.1)
# to store the kernel estimations:
f_hat_gauss = rep(0, times = length(grid))
f_hat_epan = rep(0, times = length(grid))
f_hat_trian = rep(0, times = length(grid))

# Epanechnikov Kernel
for(i in 1:length(grid)){
  f_hat_gauss[i] = kernel_estimation(draw_skewed_t, 0.8, grid[i], "Epanechnikov")
}

# Gaussian Kernel
for(i in 1:length(grid)){
  f_hat_epan[i] = kernel_estimation(draw_skewed_t, 0.8, grid[i], "Gaussian")
}

# Triangular Kernel
for(i in 1:length(grid)){
  f_hat_trian[i] = kernel_estimation(draw_skewed_t, 0.8, grid[i], "Triangular")
}

# create plot
hist(draw_skewed_t, probability = T, xlab = "", main = "Different estimates and true data")
plot(function(x)dskt(x, df=6,gamma=0.4),-10,4, main = "", xlab = "", ylab = "", add = TRUE)
lines(grid, f_hat_gauss, col = "red", lty = 4, lwd = 2)
lines(grid, f_hat_epan, col = "blue", lty = 4, lwd = 2)
lines(grid, f_hat_trian, col = "green", lty = 4, lwd = 2)
points(draw_skewed_t, true_density)
legend("topleft", legend = c("True density", "Epanechnikov kernel", "Gaussian kernel", "Triangular Kernel"),
       col=c("black", "red", "blue", "green"),lty=c(1,2,2,2), cex = 0.5)

# note that the shape of the manual estimators might look different due to different parameters

