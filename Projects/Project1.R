# Shapiro-Wilk for Study 1 Men
n_m <- 4999
mean_value_m <- 1.92
sd_value_m <- 1.11 

m_data <- rnorm(n = n_m, mean = mean_value_m, sd = sd_value_m)
shapiro.test(m_data)

# Shapiro-Wilk for Study 1 Women
n_w <- 4999
mean_value_w <- 2.06
sd_value_w <- 1.26 

w_data <- rnorm(n = n_w, mean = mean_value_w, sd = sd_value_w)
shapiro.test(w_data)

n_overall <-
percentile_97.5 <- quantile(, probs = 0.975)

maximum <- 1.95 + (2 * 1.11)

