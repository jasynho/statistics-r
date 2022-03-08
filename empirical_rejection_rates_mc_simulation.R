set.seed(73)
runs = 10000
vector_mu = seq(0, 4, by = 0.25)

# in this case the mean mu is unknown and the variance sigma is known

# sample size: 10
rates_10 = c()
n = 10
alpha = 0.05
for (i in vector_mu){
  accepted = 0
  rejected = 0
  for(k in 1:runs){
    simulation = rnorm(n, mean = i, sd = sqrt(6))
    t_stat = (mean(simulation) - 0) / (sqrt(6) / sqrt(n))
    # calculate critical value of right-tailed test 
    critical_value = qnorm(1-alpha)
    # test decision
    if(t_stat > critical_value){
      rejected = rejected + 1
    } else {
      accepted = accepted + 1
    }
  }
  result = rejected / (accepted + rejected)
  rates_10 = c(rates_10, result)
}
print(rates_10)

# sample size: 25
rates_25 = c()
n = 25
alpha = 0.05
for (i in vector_mu){
  accepted = 0
  rejected = 0
  for(k in 1:runs){
    simulation = rnorm(n, mean = i, sd = sqrt(6))
    t_stat = (mean(simulation) - 0) / (sqrt(6) / sqrt(n))
    # calculate critical value of right-tailed test 
    critical_value = qnorm(1-alpha)
    # test decision
    if(t_stat > critical_value){
      rejected = rejected + 1
    } else {
      accepted = accepted + 1
    }
  }
  result = rejected / (accepted + rejected)
  rates_25 = c(rates_25, result)
}
print(rates_25)

# sample size: 50 
rates_50 = c()
n = 50
alpha = 0.05
for (i in vector_mu){
  accepted = 0
  rejected = 0
  for(k in 1:runs){
    simulation = rnorm(n, mean = i, sd = sqrt(6))
    t_stat = (mean(simulation) - 0) / (sqrt(6) / sqrt(n))
    # calculate critical value of right-tailed test 
    critical_value = qnorm(1-alpha)
    # test decision
    if(t_stat > critical_value){
      rejected = rejected + 1
    } else {
      accepted = accepted + 1
    }
  }
  result = rejected / (accepted + rejected)
  rates_50 = c(rates_50, result)
}
print(rates_50)

# plot results
par(mfrow=c(1,1))
y = seq(0.1, 1, by = 0.1)
plot(vector_mu, rates_10, type="l", col = "black", main = "Empirical power function (sigma known)", ylab = "Power", xlab = "mu")
lines(vector_mu, rates_25, type="l", col = "red", lty = 2)
lines(vector_mu, rates_50, type="l", col = "green", lty = 3)
legend("bottomright", title = "Sample size", legend = c(10,25,50),
       col=c("black", "red", "green"),lty=c(1,2,3), cex = 0.5)

# create output table
result = rbind(rates_10, rates_25, rates_50)
out = matrix(as.numeric(format(result, digits=4)), ncol=17)
colnames(out) = vector_mu
rownames(out) = c("n = 10","n = 25", "n = 50")
out
