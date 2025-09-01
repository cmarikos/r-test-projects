# simulated logistic regression data
positive <- data.frame(y = 1, x = rnorm(n = 50, mean = 50, sd = 3))
negative <- data.frame(y = 0, x = rnorm(n = 50, mean = 42, sd = 3))
together <- rbind(positive, negative)

library(ggplot2)

# as.character function tells R that 0 and 1 are categorical
ggplot(together, aes(x = x, fill = as.character(y)))+
  geom_density(alpha = .7)

# plot as bivariate distribution
# for glm you have to specifiy that this is binomial data
ggplot(together, aes(x = x, y = y))+
  geom_point()+
  stat_smooth(method="glm", method.args=list(family="binomial"))

# build model to test our assumption that this is binomial data
model <- glm(y~x, data = together, family=binomial)
summary(model)

# use predict to determine accuracy
# for logistic models we need to include type = "response"
together$predict <- predict.glm(model, newdata=together, type = "response")

# round the predict values to 0 or 1
# you can also do this in place in the above function
together$predict2 <- round(together$predict)

# time for a confusion matrix bb
install.packages("caret")
installed.packages("e1071")
install.packages("lattice")
library(lattice)
library(caret)
library(e1071)

confusionMatrix(data=as.factor(together$predict2),
                reference=as.factor(together$y))
#           Reference
# Prediction  0  1
#          0 43  4
#          1  7 46
# Accuracy : 0.89

# using real data on release of pet reptiles

# using max_life_yr as x
ggplot(reptile_release, aes(x = max_life_yr, y = kraus_c_edd_1999))+
  geom_point()+
  stat_smooth(method="glm", method.args=list(family="binomial"))

maxlifeyrmodel <- glm(kraus_c_edd_1999 ~ max_life_yr, data = reptile_release, family=binomial)
summary(maxlifeyrmodel)

reptile_release$predict1 <- round(predict.glm(maxlifeyrmodel, newdata=reptile_release, type = "response"))


confusionMatrix(data=as.factor(reptile_release$predict1),
                reference=as.factor(reptile_release$kraus_c_edd_1999))

levels(reptile_release$predict1) 
levels(reptile_release$kraus_c_edd_1999)

unique(reptile_release$predict1)
unique(reptile_release$kraus_c_edd_1999)

  
summary(reptile_release$predict1)     

# could not get my confusion matrix to work
# going to use the dplyr work around
library(dplyr)
library(reshape2)

long <- reptile_release %>%
  filter(!is.na(predict1)) %>%
  mutate(Wrong = ifelse(kraus_c_edd_1999 == predict1, "Right", "Wrong")) %>%
  group_by(kraus_c_edd_1999, predict1, Wrong) %>%
  tally()

wide <- long %>%
  dcast(kraus_c_edd_1999~predict1, value.var = "n")

acc <- reptile_release %>%
  filter(!is.na(predict1)) %>%
  mutate(Wrong = ifelse(kraus_c_edd_1999 == predict1, "Right", "Wrong")) %>%
  group_by(Wrong) %>%
  tally()
AccuracyPercentage <- 100*(acc[1,2]/(acc[1,2]+acc[2,2]))




# using median_price as x
ggplot(reptile_release, aes(x = median_price, y = kraus_c_edd_1999))+
  geom_point()+
  stat_smooth(method="glm", method.args=list(family="binomial"))

medpricemodel <- glm(kraus_c_edd_1999 ~ median_price, data = reptile_release, family=binomial)
summary(medpricemodel)


reptile_release$predict2 <- round(predict.glm(medpricemodel, newdata=reptile_release, type = "response"))
acc <- reptile_release %>%
  filter(!is.na(predict2)) %>%
  mutate(Wrong = ifelse(kraus_c_edd_1999 == reptile_release$predict2, "Right", "Wrong")) %>%
  group_by(Wrong) %>%
  tally()
AccuracyPercentage <- 100*(acc[1,2]/(acc[1,2]+acc[2,2]))

wide <- long %>%
  dcast(kraus_c_edd_1999~reptile_release$predict2, value.var = "n")
