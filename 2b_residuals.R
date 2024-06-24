library(ISLR)

data(Auto)
Auto_subset <- Auto[1:50, ]
Auto_subset <- subset(Auto_subset, select = -name)
model <- lm(mpg ~ cylinders + weight + acceleration, data = Auto_subset)


# Make a QQ plot
qqnorm(resid(model), main="Q-Q Plot of Residuals", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
qqline(resid(model), col = "red")

# Make a residual plot against the fitted values
plot(model$fitted.values, resid(model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
# create a line at y=0, to better see if the variance is constant
abline(h = 0, col = "red")

# Plot residuals vs. cylinders
plot(Auto_subset$cylinders, resid(model),
     xlab = "Cylinders", ylab = "Residuals",
     main = "Residuals vs Cylinders")
abline(h = 0, col = "red")

# Plot residuals vs. weight
plot(Auto_subset$weight, resid(model),
     xlab = "Weight", ylab = "Residuals",
     main = "Residuals vs Weight")
abline(h = 0, col = "red")

# Plot residuals vs. acceleration
plot(Auto_subset$acceleration, resid(model),
     xlab = "Acceleration", ylab = "Residuals",
     main = "Residuals vs Acceleration")
abline(h = 0, col = "red")


# calculate the man of the residuals, to see if it is close to 0
mean(resid(model))

# Create a histogram of the residuals
#The histogram is useful for assessing the normality and identifying any skewness in the distribution of the residuals.
hist(resid(model),
     main="Histogram of Residuals",
     xlab="Residuals",
     ylab="Frequency",
     col="lightblue",
     border="black")

# Create a boxplot of the residuals
boxplot(resid(model),
        ylab = "Residuals",
        main = "Boxplot of Residuals",
        col = "blue")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red")