#Slide 2 Plot
library(ggplot2)
library(dplyr)

ggplot(iris, aes(x=Petal.Length, y = Petal.Width))+
  geom_point() +
  theme_bw()

ggplot(iris, aes(x=Petal.Length, y = Petal.Width, col = Species))+
  geom_point() +
  theme_bw()+
  guides(col = FALSE)

versicolor <- iris %>% filter(Species == "versicolor")

ggplot(versicolor, aes(x=Petal.Length, y = Petal.Width))+
  geom_point(col = "#7CAE00") +
  theme_bw()

ggplot(versicolor, aes(x=Petal.Length, y = Petal.Width))+
  geom_point(col = "#7CAE00") +
  theme_bw()+
  scale_x_continuous(limits = c(2, 7))+
  scale_y_continuous(limits = c(0, 3))

ggplot(versicolor, aes(x=Petal.Length, y = Petal.Width))+
  geom_point(col = "#7CAE00") +
  theme_bw()+
  scale_x_continuous(limits = c(3, 5))+
  scale_y_continuous(limits = c(1, 2))

cor(versicolor$Petal.Length, versicolor$Petal.Width)

x <- rnorm(n = 25, mean = 32, sd = 5)
y = x
y1 = -1*x
y2 = jitter(-1*x, factor = 1000)
ggplot() + geom_point(aes(x=x, y=y))
ggplot() + geom_point(aes(x=x, y=y1))
ggplot() + geom_point(aes(x=x, y=y2))


s1 <- data.frame(x = c(1:7), y = c(2:8))
cor(s1$x, s1$y)
s2 <- data.frame(x = c(1:7), y2 = c(2:7,NA))
cor(s2$x, s2$y2)
cor(s2$x, s2$y2, use = "complete.obs" )
s3 <- data.frame(x = c(1:7), y3 =jitter(c(1:7)))
cor(s3$x, s3$y3)
                 
x = rnorm(n = 50, mean = 30, sd = 3)
y = jitter(x, factor = 1)
y1 = jitter(x, factor = 1000)
y2 = jitter(x, factor = 10000)

t <- rbind(data.frame(x=x, y = y, lab = "Jitter Factor 1"),data.frame(x=x, y = y1, lab = "Jitter Factor 1,000"),data.frame(x=x, y = y2, lab = "Jitter Factor 10,000"))


ggplot(t, aes(x = x, y = y, col = lab, shape = lab))+
  geom_point(alpha = .75, size = 3)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("black", "blue", "green"))+
  scale_shape_manual(values=c(16,15,17))





x = rnorm(n = 7, mean = 30, sd = 3)
y1 = jitter(x, factor = 1)
y2 = jitter(x, factor = 1000)
y3 = jitter(x, factor = 10000)
t3 <- data.frame(x,y1,y2,y3)
write.csv(t3, "t3.csv", row.names=FALSE)

m <- cor(data.frame( x , y1 , y2 , y3))
install.packages("corrplot")
library(corrplot)
corrplot(m, method = "circle")
corrplot(m, method = "color")
