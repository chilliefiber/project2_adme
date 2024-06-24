library(ISLR)
data(Auto)
Auto_subset <- Auto[1:50, ]
Auto_subset <- subset(Auto_subset, select = -name)
model <- lm(mpg ~ cylinders + weight + acceleration, data = Auto_subset)

# calculate the standardized residuals
std_residuals <- resid(model) / sd(resid(model))
# define the threshold above which a standardized residual is considered an outlier
outlier_threshold = 2

regression_outliers = std_residuals[abs(std_residuals) > outlier_threshold]