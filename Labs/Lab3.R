# creating a normal distribution using simulated data
NormalData <- rnorm(n = 150, mean = 25, sd = 2)
hist(NormalData)

# creating non-normal distributions
mode1 <- rnorm(n = 50, mean = 25, sd = 2)
mode2 <- rnorm(n = 150, mean = 35, sd = 2)
BimodalData <- c(mode1, mode2)
hist(BimodalData)

# central tendency functions
median(NormalData)
mean(NormalData)
median(BimodalData)
mean(BimodalData)

max(NormalData)
min(BimodalData)
sd(BimodalData)

shapiro.test(NormalData)
shapiro.test(BimodalData)

fs <- Faculty_Salaries_FY_2020

hist(fs$NewSalary, breaks = 50, xlim = c(0,200000))

mean(fs$NewSalary)
median(fs$NewSalary)
sd(fs$Annual.at.Actual.FTE)
min(fs$NewSalary)
max(fs$NewSalary)

shapiro.test(fs$Annual.at.Full.FTE)
n <- sample(x=fs$Annual.at.Full.FTE, size=10, replace=FALSE)
shapiro.test(n)

max(BimodalData) / min(BimodalData)
max(BimodalData) - min(BimodalData)

#Full FTE
max(fs$Annual.at.Full.FTE)
min(fs$Annual.at.Full.FTE)
max(fs$Annual.at.Full.FTE)/min(fs$Annual.at.Full.FTE)

#Actual FTE
max(fs$Annual.at.Actual.FTE)
min(fs$Annual.at.Actual.FTE)
max(fs$Annual.at.Actual.FTE)/min(fs$Annual.at.Actual.FTE)

#New FTE
max(fs$NewSalary)
min(fs$NewSalary)
max(fs$NewSalary)/min(fs$NewSalary)


