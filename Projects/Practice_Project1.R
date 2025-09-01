# Shapiro-Wilk for practice question 6
#number of fish caught and measured
n <- 1000 

#measured in cm
mean_value <- 22

# standard deviation of sample
sd_value <- 7.333 

# Trying to use the Shapiro-Wilk test with synthetic data
# "Error: object 'synthetic_data' not found
set.seed(42)
synthetic_data <- rnorm(n, mean = mean_value, sd = sd_value)
shapiro.test(synthetic_data)
 
hist(synthetic_data)



