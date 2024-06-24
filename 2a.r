#Setting up the subset
library(ISLR)
data(Auto)
Auto_subset <- Auto[1:50, ]
Auto_subset <- subset(Auto_subset, select = -name)

#2a

# Do a 1st iteration with all the variables that aren't the dependable
# variable (which is mpg)

model_1 <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = Auto_subset)

summary(model_1)

# Remove the variable with the highest Pr(>|t|) and try again for each subsequent iteration

model_2 <- lm(mpg ~ cylinders + horsepower + weight + acceleration + year + origin, data = Auto_subset)

summary(model_2)

model_3 <- lm(mpg ~ cylinders + horsepower + weight + acceleration + origin, data = Auto_subset)

summary(model_3)

model_4 <- lm(mpg ~ cylinders + horsepower + weight + acceleration, data = Auto_subset)

summary(model_4)

# For this model there was no variable to remove, as per our significance level of 0.05 for the test
# of significance of an individual variable

model_5 <- lm(mpg ~ cylinders + weight + acceleration, data = Auto_subset)

summary(model_5)