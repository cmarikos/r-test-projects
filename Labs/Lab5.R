# part 1: create two different normally distributed datasets
data.50 <- rnorm(n = 150, mean = 50, sd = 5)
data.55 <- rnorm(n = 150, mean = 55, sd = 5)

?t.test

t.test(data.50, data.55, alternative = "two.sided")
t.test(data.50, data.55, alternative = "greater")
t.test(data.50, data.55, alternative = "less")

test1 <- t.test(data.50, data.55, alternative = "greater")

test1$p.value
test1$statistic
test1$alternative
test1$method

t.test(data.50, data.55, alternative = "greater")$p.value
t.test(data.50, data.55, alternative = "greater")$statistic
t.test(data.50, data.55, alternative = "greater")$alternative
t.test(data.50, data.55, alternative = "greater")$method
t.test(data.50, data.55, alternative = "greater")$parameter

t.test(data.50, data.55, paired = TRUE, var.equal = FALSE)$statistic
t.test(data.50, data.55, paired = TRUE, var.equal = FALSE)$parameter
t.test(data.50, data.55, paired = TRUE, var.equal = FALSE)$p.value

t.test(data.50, data.55, paired = TRUE, var.equal = TRUE)$statistic
t.test(data.50, data.55, paired = TRUE, var.equal = TRUE)$parameter
t.test(data.50, data.55, paired = TRUE, var.equal = TRUE)$p.value

t.test(data.50, data.55, paired = FALSE, var.equal = TRUE)$statistic
t.test(data.50, data.55, paired = FALSE, var.equal = TRUE)$parameter
t.test(data.50, data.55, paired = FALSE, var.equal = TRUE)$p.value

colnames(penguin)

A <- penguin$AMASS
B <- penguin$BMASS

t.test(x=A, y=B, alternative = "less", paired = TRUE)
t.test(x=A, y=B, alternative = "less", paired = FALSE)$p.value
t.test(x=A, y=B, alternative = "two.sided", paired = TRUE)$p.value


mean(A)/mean(B) #A is .81 times the size of B
mean(B)/mean(A) #B is 1.23 times the size of A
mean(A)-mean(B) #A is 22 grams smaller on average than B
mean(B)-mean(A) #B is 22 grams bigger on average than A

var.test(A,B, alternative="two.sided")
