library(ISLR)
data(Auto)
Auto_subset <- Auto[1:50, ]
Auto_subset <- subset(Auto_subset, select = -name)
model <- lm(mpg ~ cylinders + weight + acceleration, data = Auto_subset)

# Calculate Cook's distances 
cooks_distances <- cooks.distance(model)

# p should be 4, 3 for cylinders, weight and acceleration plus 1 for the intercept
# This way we calculate p programatically, and if the model changes p changes automatically
p = length(coef(model))

# n is the number of observations. Since we select the subset from observation 1 to 50,
# it should be 50. This way, we calculate it programatically
n = nrow(Auto_subset)

# taken from the slides
high_cook_distance_threshold = 4 / (n-p)

influential_points = cooks_distances[cooks_distances > high_cook_distance_threshold]

plot(cooks_distances, type="h", main="Cook's Distance", ylab="Cook's Distance", xlab="Observation Index")
abline(h = high_cook_distance_threshold, col = "red")  # Adding a reference line at 0.5 which is a common threshold.