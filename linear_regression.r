dataset = read.csv("Your dataset")

# Installation of packages
#install.packages("corrplot")
#install.packages("car")
#install.packages("lmtest")
#install.packages("olsrr")
library(lmtest)
library(corrplot)
library(car)
library(olsrr)

# Only run the code below once! We use it to transform the dummy variable 'weekend' from string to a one-zero case. 
# Task: Try to think for yourself why you should not run the code a second time and think about a solution that solves the problem. 
dataset$weekend <- ifelse(dataset$weekend == "yes", 1, 0)
# Calculate correlations
dataset.correlations = cor(dataset[2:8]); 
# Plot correlations
corrplot(dataset.correlations)

# Create first regression model
mod = lm(overall ~ rides + games + wait + clean, data = dataset)

# Summary of the estimated model
summary(mod)

# Multicollinearity (check 1)
car::vif(mod)
# Task: Try to come up with an interpretation for the VIF-Values

# Heteroskedasticty via Breusch-Pagan Test (check 2)
# H0: homoskedasticity vs H1: heteroskedasticity 
bptest(mod)
# Interpretation: p-value smaller than 0.05 -> reject H0 -> heteroskedasticity

# Normally distributed error terms (check 3)
# Shapiro-Wilk: H0: normally distributed error terms vs H1: error terms not normally distributed
ols_test_normality(mod)
# Shapiro-Wilk p-value < 0.05 -> reject H0 -> error terms not normally distributed

# Other possibility to check normality of error terms is via QQ-plot:
ols_plot_resid_qq(mod) 
# Task: Describe what you see in the QQ-Plot: 

# Linearity between DV and IV (check 4)
ols_plot_resid_fit(mod)
# Task: Describe what you see in the resid-fit plot:

# r-squared
print(summary(mod)$r.squared)
print(summary(mod)$adj.r.squared)

# Task: Interpret both r-squared values: 


