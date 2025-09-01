dist.15 <- rnorm(n = 50, mean = 15, sd = 2)
dist.25 <- rnorm(n = 50, mean = 25, sd = 2)

hist(dist.15, xlim=c(10,35), ylim=c(0, 15), col="red")
par(new=TRUE)
hist(dist.25, xlim=c(10,35), ylim=c(0, 15))

t.test(dist.15, mu=15) #mean of 15
t.test(dist.25, mu=25) #mean of 25

t.test(dist.15, dist.25)
t.test(dist.15, dist.15)

ks <- X2016_Kickstarter_Projects
ks1 <- subset(ks, goal > 10)

successful <- subset(ks1, state=="successful")
failed <- subset(ks1, state == "failed")

t.test(successful$goal, mu=10000)

mean(successful$goal)

t.test(failed$goal, mu=!10000)
t.test(failed$goal, mu = 10000, alternative = "two.sided")

t.test(successful$goal, mu<=10000)
t.test(successful$goal, mu = 10000, alternative = "less")

hist(successful$goal, xlim = c(0,50000), breaks = 5000)

options(scipen=999)
par(mfrow=c(2,1))
hist(successful$goal, xlim = c(0, 1000000), ylim= c(0, 5000), breaks=1000)
hist(failed$goal, xlim = c(0, 1000000),  ylim= c(0, 5000), breaks=1000, col = "red")

sd(successful$goal)
sd(failed$goal)

t.test(successful$goal, failed$goal, alternative = "two.sided", var.equal = FALSE)

t.test(successful$goal, failed$goal, mu = 5000)
