#########################################
###Calculate an Odds Ratio (Correctly)###
#########################################

#The numbers here are from the statement "the protective effect against LM development was 1.4 times greater for the daily consumption of kefir (OR: 0.69,CI: 1.18-2.22);" but using the raw numbers instead.
a = 24 #exposed (ate it daily) cases (had LM)
c = 280 #unexposed (didn't eat it daily) cases (had LM)
b = 116 #exposed (ate it daily) non-cases (didn't have LM)
d = 185 #unexposed (didn't eat it daily) non-cases (didn't have LM)


OR <- (a/c)/(b/d) #the odds ratio they should have calculated, but they got .57
1/OR #just to see if it's inverse
squaredSE <- sqrt((1/a)+(1/b)+(1/c)+(1/d)) #standard error for putting into the formula below, runs off of the above variables
CI.upper = exp(log(OR)+1.96*squaredSE) #Upper 95% Confidence Interval
CI.lower = exp(log(OR)-1.96*squaredSE) #Lower 95% Confidence Interval


######################################################################
###Reminder on how to run a t-test on means and standard deviations###
######################################################################
g1 <- rnorm(n = 100, mean = 10, sd = 2) #remember - randomized data won't perfectly match their results.
g2 <- rnorm(n = 100, mean = 10, sd = 2)

t.test(g1, g2)

################################
### Other important functions###
################################
library(dplyr) #load some of those functions
options(scipen = 9999) #turn off scientific notation

#Here is an example of some important functions using the iris dataset and the shapiro wilks test. You don't HAVE to do these, but they will definitely help if you want to do anything in multiple groups without lots of copy pasting.

oh.wow.p.values <- iris %>%
  group_by(Species) %>%
  summarize(p = shapiro.test(Petal.Length)$p.value)

oh.wow.p.values2 <- iris %>%
  group_by(Species) %>%
  summarize(p = t.test(Petal.Length, Sepal.Length)$p.value)

unique(iris$Species)

oh.right.subsets <- iris %>%
  filter(Species == "setosa")

nice.new.groups <- iris %>%
  mutate(NewGroup = ifelse(Species == "setosa", "cool", "yuck")) %>%
  group_by(NewGroup) %>%
  summarize(Mean = mean(Sepal.Length), sd = sd(Sepal.Length))

kefir_matrix <- matrix(data = c(91, 45, 49, 116, 177, 71, 32, 24), nrow = 4, ncol = 2)
#kefir_matrix <- matrix(data = c(30.2, 14.9, 16.3, 38.6, 58.3, 23.3, 10.5, 7.9), nrow = 4, ncol = 2)

chisq.test(x = kefir_matrix)

tarhana_matrix <- matrix(data = c(63, 105, 115, 18, 84, 118, 96, 6), nrow = 4, ncol = 2)


