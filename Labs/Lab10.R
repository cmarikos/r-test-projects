library("ggplot2")
library("reshape2")

x <- rnorm(n = 50, mean = 25, sd = 5)
y1 <- jitter(x, factor = 2000)
y2 <- -1*jitter(x, factor = 2000)
y3 <- rnorm(n = 50, mean = 25, sd = 5)
y4 <- jitter(x/2, factor = 2000) + 22

#combine above into a dataframe
AYT0 <- data.frame(x,y1,y2,y3,y4)

AYT1 <- melt(AYTO, id.vars="x")

ggplot(AYT1, aes(x = x, y = value, col = variable)) + geom_point() + geom_smooth(method="lm")

#geom_smooth()
#geom_smooth(method="lm")
#geom_smooth(se=FALSE)
#facet_grid(~variable)

#same as lm(y4~x)
lm(AYT0$y4~AYT0$x)


lm(y1~x)
lm(y2~x)
lm(y3~x)
lm(y4~x)

summary(lm(y1~x))
summary(lm(y2~x))
summary(lm(y3~x))
summary(lm(y4~x))

#model when all the y values are combined
combined_y <- c(y1,y2,y3,y4)
AYT0 <- data.frame(x,combined_y)
AYT1 <- melt(AYTO, id.vars="x")
ggplot(AYT1, aes(x = x, y = combined_y)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

lm(combined_y ~ x, data =AYT0)
summary(lm(combined_y~x, data = AYT0))


#cowfightclub
ggplot(cowfightclub, aes(x = cowfightclub$Body_weight, y = cowfightclub$Fighting_ability)) + geom_point() + geom_smooth(method="lm")
summary(lm(cowfightclub$Fighting_ability~cowfightclub$Body_weight))

length(cowfightclub$Animal)
