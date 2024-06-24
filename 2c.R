library(ISLR)
data(Auto)
Auto_subset <- Auto[1:50, ]
Auto_subset <- subset(Auto_subset, select = -name)
model <- lm(mpg ~ cylinders + weight + acceleration, data = Auto_subset)

observation_14 <- Auto_subset[14, ]
observation_31 <- Auto_subset[31, ]


# Calculate the 97.5% confidence interval for the expected response
confidence_interval_14 <- predict(model, newdata = observation_14, interval = "confidence", level = 0.975)
confidence_interval_31 <- predict(model, newdata = observation_31, interval = "confidence", level = 0.975)

confidence_interval_14
confidence_interval_31

prediction_interval_14 <- predict(model, newdata = observation_14, interval = "prediction", level = 0.975)
prediction_interval_31 <- predict(model, newdata = observation_31, interval = "prediction", level = 0.975)
