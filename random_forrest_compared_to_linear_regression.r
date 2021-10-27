dataset = read.csv("Your dataset")

#install.packages("rsample")
#install.packages("randomForest") 
library(randomForest)
library(rsample)

# set seed
set.seed(42)

#linear model from last week and calculation of MSE
mod_ols = lm(overall ~ rides + games + wait + clean, data = dataset)
summary(mod_ols)
mse_ols = mean(mod_ols$residuals^2)

#create random forest
mod_rf = randomForest(formula = overall ~ rides + games + wait + clean, data = dataset)
print(mod_rf)
plot(mod_rf)
which.min(mod_rf$mse)
mse_rf = mean(mod_rf$mse)

#adjust your models and compare
mod_rf_2 = randomForest ( formula = overall ~ ., data = dataset)
print(mod_rf_2)
plot(mod_rf_2)
which.min(mod_rf_2$mse)
mse_rf_2 = mean(mod_rf_2$mse)


mod_ols_2 = lm(overall ~ ., data = dataset)
summary(mod_ols_2)
mse_ols_2 = mean(mod_ols_2$residuals^2)

# Task: Compare the different models with respect to MSE. Which model would you prefer when you only take 4 variables into account?
# Which model would you prefer when you take all variables into account? 
