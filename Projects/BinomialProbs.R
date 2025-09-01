#I flipped a quarter 10 times and got heads 9 times. 
#I am going to save my results as a variable named Heads and a variable named Tails.
Heads = 9
Tails = 1

#I would expect normally with a coin that has 50-50 odds for either side (so, 5 Tails, 5 Heads). 
#To do that I’m going to make a data frame using the data.frame() function. 
#This function will have three columns: 
#Type (whether it was expected or what I actually measured), 
#Heads (the number of heads expected and measured), 
#Tails (the number of tails expected and measured). 
#Use the c()function to make these columns.
mycoin <- data.frame(Type= c("Expected", "Got"),                 
                     Heads = c(5, Heads),
                     Tails = c(5, Tails))

install.packages("reshape2")
library("reshape2")

#To turn your data from wide format into long format, you need to use the melt() function.
#The results of this function is you have a long format data set that has two new column names: variable and value.
longcoins <- melt(mycoin)

library("ggplot2")

#Use the stat = "identity" and position= "dodge" arguments to separate out your groups nicely.
ggplot(longcoins, aes(x = variable, y = value, fill = Type))+
  geom_bar(stat = "identity", position = "dodge")

#This gives you the exact probability of getting 9/10 as heads.
dbinom(x = Heads, size = 10, prob = .5)

#Instead of giving it a single value, we use a colon (:) to specify a range of values
#Here, from 0 to 10.
dbinom(x=0:10, size=10, prob = .5)

#We would like to be able to plot this in ggplot2,
#We will need to make it into a data frame
coin.prob <- data.frame(x = 0:10,
                        y = dbinom(x=0:10, size=10, prob = .5))

#Now we can make a probability distribution use bar and/or line
ggplot(coin.prob, aes(x = x, y = y))+
  geom_bar(stat="identity")+
  geom_line(col = "dodgerblue")

#It can be helpful to see where your results lie on this distribution,
#Which is easiest with the geom_vline()
ggplot(coin.prob, aes(x = x, y = y))+
  geom_bar(stat="identity")+
  geom_line(col = "dodgerblue")+
  geom_vline(xintercept = Heads, col = "red")

#This gives you the exact probability of getting 5/15 as heads.
dbinom(x = 5, size = 15, prob = .5)

#Probability of 25/150 heads
dbinom(x = 25, size = 150, prob = .5

#Full prob distribution       
ggplot(data.frame(x=0:150, y = dbinom(0:150, 150, prob = .5)), 
       aes(x = x, y = y))+ geom_line()

#Two partial range distributions
ggplot(data.frame(x=1:150, y = dbinom(1:150, 150, prob = .5)), 
       aes(x = x, y = y))+ geom_line()
ggplot(data.frame(x=0:15, y = dbinom(0:15, 150, prob = .5)), 
       aes(x = x, y = y))+ geom_line()

#Probability  of 13/30 heads for my tums anti-acid "coin"
dbinom(x = 13, size = 30, prob = .5)

tums.prob <- data.frame(x = 0:30,
                        y = dbinom(x=0:30, size=30, prob = .5))

ggplot(tums.prob, aes(x = x, y = y))+
  geom_bar(stat="identity")+
  geom_line(col = "dodgerblue")
