library(dplyr)
library(ggplot2)
library(reshape2)

# add a total wasp count column
wasps1 <- wasps %>%  mutate(Total.Wasps = `Side Room - Live`+`Main Room - Live`)

# see how R is reading our time column
class(wasps1$Time)

# 24 hour time conversion
wasps2 <- wasps1 %>%
  mutate(TimeNumber = as.numeric(gsub(":00", "", Time))) %>%
  mutate(Hour24 = ifelse(grepl("P", Time) & TimeNumber != 12, TimeNumber + 12, TimeNumber))

ggplot(wasps2, aes( x = `Temperature Inside`, y = Total.Wasps))+ 
  geom_point() + 
  geom_smooth() +
  geom_smooth(method = "lm", col = "purple", fill = "pink")

waspmelt<- wasps2 %>%
  select(Date,Time, Hour24, Total.Wasps, `Temperature Inside`, `Temperature Outside`) %>%
  melt(id.var = c("Date", "Time", "Total.Wasps"))

ggplot(waspmelt, aes (x = value, y = Total.Wasps))+
  geom_point() +
  facet_wrap(~variable, scales = "free") +
  geom_smooth()

wasps2 <- wasps2 %>%
  mutate(HourAdjusted = abs(Hour24 - 12))


ggplot(wasps2, aes(x = HourAdjusted, y = Total.Wasps))+
  geom_point()+
  geom_smooth()

ggplot(wasps2, aes(x = HourAdjusted, y = Total.Wasps, col = Hour24))+
  geom_point(shape = ifelse(wasps2$Hour24 > 12, 17, 16), size = 3)+
  geom_smooth()+
  scale_color_gradient(low = "red", high = "blue")

# linear model
model1 <- lm(Total.Wasps ~ `Temperature Outside` + `Temperature Inside` + HourAdjusted, data = wasps2)

plot(model1)

summary(model1)

temp1 <- data.frame(`Temperature.Inside` = 76,   `Temperature.Outside` = 45, HourAdjusted = 1)

# make the column names match
colnames(temp1) <- c("Temperature Inside", "Temperature Outside", "HourAdjusted")

predict(model1, newdata = temp1)
predict(model1, newdata = temp1, interval = "confidence")

# _____

# add a total wasp kill count column
# there is a sneaky double space in the Main Room column which gave me a long and painful headache
wasps2 <- wasps2 %>%  mutate(Total.Killed = `Side Room - I killed` + `Main Room  - I killed`)

# I excluded temp outside, because wasp kills only happen inside
KillTempModel <- lm(Total.Killed ~ `Temperature Inside` + HourAdjusted + Total.Wasps, data = wasps2)

plot(KillTempModel)
summary(KillTempModel)

min(wasps2$`Temperature Inside`)

ggplot(wasps2, aes(x = `Temperature Inside`, y = Total.Killed))+
  geom_point()+
  geom_smooth()


ggplot(wasps2, aes(x = Total.Wasps, y = Total.Killed))+
  geom_point()+
  geom_smooth()


