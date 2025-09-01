roll1 = sample(c(1:6), 60, replace=TRUE)
roll2 = sample(c(1:8), 60, replace=TRUE)
roll3 = roll1+roll2
hist(roll3)

Rock_Ptarmigan <- rnorm(n=500, mean=540, sd=2)
Willow_Ptarmigan <- rnorm(n=500, mean=620, sd=10) 

hist(Rock_Ptarmigan)
hist(Willow_Ptarmigan)

var.test(Rock_Ptarmigan, Willow_Ptarmigan, alternative = "two.sided")

data("faithful") 
shapiro.test(faithful$eruptions)
hist(faithful$eruptions)
hist(faithful$waiting)
shapiro.test(faithful$waiting)

var.test(faithful$eruptions, faithful$waiting, alternative = "two.sided")

mean(faithful$eruptions)
median(faithful$eruptions)

data("Titanic")
view(Titanic)
require(graphics)
mosaicplot(Titanic, main = "Survival on the Titanic")

