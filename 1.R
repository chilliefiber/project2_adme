#Setting up the subset
library(ISLR)
data(Auto)
Auto_subset <- Auto[1:50, ]
Auto_subset <- subset(Auto_subset, select = -name)

summary(Auto_subset)

#Box plots for every variable
par(mfrow=c(4, 2)) 
for(i in 1:ncol(Auto_subset)){
  boxplot(Auto_subset[,i], main=names(Auto_subset)[i])
}

#Pairs Graph
library(GGally)
ggpairs(Auto_subset)


#Covar and Correlation Matrices
var_cov_matrix <- cov(Auto_subset)
print(var_cov_matrix)
correlation_matrix <- cor(Auto_subset)
print(correlation_matrix)

  #Winsorized Mean Function for every variable in a dataset 
compute_trimmed_means <- function(dataset, trim_lvl = 0.05) {
  num_cols <- sapply(dataset, is.numeric)
  trimmed_avg <- sapply(dataset[, num_cols], function(y) mean(robustHD::winsorize(y, range = quantile(y, probs = c(trim_lvl, 1 - trim_lvl)))))
  return(data.frame(VarName = names(trimmed_avg), TrimmedMean = trimmed_avg, row.names = NULL))
}

trimmed_means_result <- compute_trimmed_means(Auto_subset, trim_lvl = 0.05)
print(trimmed_means_result)

