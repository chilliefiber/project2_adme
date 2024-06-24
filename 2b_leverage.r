library(ISLR)
data(Auto)
Auto_subset <- Auto[1:50, ]
Auto_subset <- subset(Auto_subset, select = -name)
model <- lm(mpg ~ cylinders + weight + acceleration, data = Auto_subset)

# p should be 4, 3 for cylinders, weight and acceleration plus 1 for the intercept
# This way we calculate p programatically, and if the model changes p changes automatically
p = length(coef(model))

# n is the number of observations. Since we select the subset from observation 1 to 50,
# it should be 50. This way, we calculate it programatically
n = nrow(Auto_subset)

# Calculate hat values
hat_values <- hatvalues(model)

# Define the threshold for high leverage. This is taken from the well defined rule from the
# slides
high_leverage_threshold <- 2 * p / n

# Print the threshold
high_leverage_threshold

# Identify high leverage points
#high_leverage_points <- which(hat_values > high_leverage_threshold)
high_leverage_points = hat_values[hat_values > high_leverage_threshold]

# Output the indexes of high leverage points
high_leverage_points