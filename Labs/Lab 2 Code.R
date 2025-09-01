metal <- read.csv("C:/Users/christinamarikos/Documents/ISTA_116/metal.csv")
hist(metal$formed)
hist(metal$split)


hist(metal$formed, col = "purple")
hist(metal$formed, main = "purple")
hist(metal$formed, xlab = "purple")
hist(metal$formed, ylab = "purple")
hist(metal$formed, border = "purple")


## #QUESTION 2 CODE
hist(metal$formed, col = "boogers")
hist(metal$formed, main = "boogers")
hist(metal$formed, xlab = "boogers")
hist(metal$formed, ylab = "boogers")
hist(metal$formed, border = "boogers")



## # QUESTION 3 CODE
hist(metal$formed, col = "olivedrab2")
hist(metal$formed, main = "olivedrab2")
hist(metal$formed, xlab = "olivedrab2")
hist(metal$formed, ylab = "olivedrab2")
hist(metal$formed, border = "olivedrab2")

## hist(metal$formed, col = violetred2) #This Code Gives You An Error!

oohsopretty <- "violetred2"
hist(metal$formed, col = oohsopretty)

## #Watch out, this code doesn't work!
## pretty <- "thistle

hist(metal$formed, col = "thistle", border = "tomato")

knitr::include_graphics("blue plus sign-01.png")

pretty <- "thistle"
hist(metal$formed)
## 2+2
## metal2 <- metal


## #QUESTION 5 CODE
## hist(metal$)
## hist((metal$formed)
## hist(metal$formed, col = "thistle"")
## hist(metal$formed, col = "thistle)
## hist(metal$formed, col = thistle)

hist(metal$formed, breaks = 5)
hist(metal$formed, breaks = c(1950, 1970, 1971, 1972, 2010,2020))
hist(metal$formed, xlim = c(2000, 2010))
hist(metal$formed, xlim = c(2000, 2010), breaks = 45)
hist(metal$formed, xlim = c(2000, 2010), breaks = c(1950, 2000, 2000.5, 2001, 2001.5, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2020))
hist(metal$formed, ylim = c(3, 45))


## #QUESTION 6 CODE
hist(metal$formed, breaks = 35)
hist(metal$formed, breaks = c(35))
## hist(metal$formed, xlim = 35)
## hist(metal$formed, xlim = c(35))


## #QUESTION 7 CODE
hist(metal$formed, breaks = 5)
## hist(metal$formed, breaks = c(1920, 1990, 2000, 2005, 2010, 2015, 2020))
## hist(metal$formed, ylim = c(0, 100))
 hist(metal$formed, xlim = c(1900, 2020))

## ?hist

metal$split - metal$formed

totaltime <- metal$split - metal$formed

metal$totaltime <- metal$split - metal$formed


## #QUESTION 8 CODE
metal$days <- metal$totaltime * 365
## days <- metal$totaltime * 365
## metal$totaltime * 365

plot(metal$totaltime, metal$formed)

plot(metal$totaltime~metal$formed)

plot(metal$totaltime~metal$formed, col = "purple")
plot(metal$totaltime~metal$formed, pch = 16)
plot(metal$totaltime~metal$formed, pch = 17)
plot(metal$totaltime~metal$formed, xlim = c(1990, 2010), ylim = c(0,10))


## #QUESTION 11 CODE
## plot(metal$totaltime~metal$formed, col = "red", pch = 0)
 plot(metal$totaltime~metal$formed, col = "red", pch = 17)
plot(metal$totaltime~metal$formed, fill = "red", pch = 17)
## plot(metal$totaltime~metal$formed, col = "red", xlim = 15)

hist(metal$split)
abline(h = 300, lty = 2)
abline(v = 2000, lwd = 5, col = "blue")

plot(metal$totaltime~metal$split)
abline(h = 10, lty = 2)
abline(v = 2000, lwd = 5, col = "blue")
abline(a = -200, b = .1, lwd = 2,  col = "red")

plot(metal$totaltime~metal$split)
text(x = 1975, y = 20, "Sad times for metal fans")
text(x = 1999, y = 40, "yay metal", col = "red")
