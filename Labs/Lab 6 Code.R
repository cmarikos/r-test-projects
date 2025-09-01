install.packages("dplyr")
install.packages("ggplot2")

library("dplyr")
library("ggplot2")



ggplot(IFa, aes(x=NA_Sales)) + geom_histogram()

ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(col="blue")
ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(fill="green")
ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(alpha=.5)
ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(binwidth=.02)

ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(fill="green", col = "blue", alpha = .5)
ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(fill="green", col = "blue", alpha = 1)
ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(col="green", border = "blue", alpha = .5)
ggplot(IFa, aes(x=NA_Sales, fill="green", col = "blue")) + geom_histogram(alpha = .5)

ggplot(IFa, aes(x = NA_Sales)) + geom_histogram(fill="blue")
ggplot(IFa, aes(x = NA_Sales)) + geom_histogram(aes(fill="blue"))
ggplot(IFa, aes(x = NA_Sales), fill = "blue") + geom_histogram()
ggplot(IFa, aes(x = NA_Sales, fill = "blue")) + geom_histogram()

ggplot(IFa, aes(x=NA_Sales)) + geom_density()
ggplot(IFa, aes(x=NA_Sales)) + geom_dotplot()

ggplot(IFa, aes(x=NA_Sales, y = Year)) + geom_point()
ggplot(IFa, aes(x=NA_Sales, y = Genre)) + geom_boxplot()
ggplot(IFa, aes(x=NA_Sales, y = Genre)) + geom_violin()

ggplot(IFa, aes(x=NA_Sales, fill = Genre)) + geom_density() 

ggplot(IFa, aes(x=NA_Sales, fill = Genre)) + geom_density(alpha = .5) 

ggplot(IFa, aes(x=NA_Sales, fill = Genre)) + 
  geom_density(alpha = .5) + 
  facet_wrap(~Genre)

ggplot(IFa, aes(x=Genre)) + geom_bar()

ggplot(data = VG, aes(x = Genre, fill = Publisher))+
  geom_bar()

ggplot(data = VG, aes(x = Genre, fill = Publisher))+
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(Nin, aes(x=NA_Sales, fill = Genre)) + 
  geom_density(alpha = .5)+
  scale_x_continuous(limits = c(1, 55))

ggplot(IFa, aes(x=NA_Sales, fill = Genre, col = Platform)) + 
  geom_density(alpha = .5)+
  scale_fill_manual(values = c("red", "blue", "goldenrod", "black", "purple"))+
  scale_color_manual(values = c("mediumseagreen", "violet", "yellowgreen", "darkorchid1", "pink", "yellow", "dodgerblue"))

Table1 <- VG %>%
  group_by(Genre) %>%
  summarize(Mean = mean(NA_Sales))

Table2 <- VG %>%
  group_by(Genre, Publisher) %>%
  summarize(Mean = mean(NA_Sales))

Table3 <- VG %>%
  group_by(Genre, Publisher) %>%
  summarize(Mean = mean(NA_Sales), Maximum = max(NA_Sales))

VG %>% group_by(Genre, Publisher) %>%  summarize(Mean = median(NA_Sales))
VG %>% group_by(Genre) %>%  summarize(Median = median(NA_Sales, Publisher))
VG %>% summarize(Median = median(NA_Sales, Publisher, Genre))
VG %>% group_by(Genre, Publisher) %>%  summarize(Median = mean(NA_Sales))

Table4 <- VG %>%
  group_by(Publisher) %>%
  summarize(p = shapiro.test(NA_Sales)$p.value)


Table5 <- VG %>%
  group_by(Genre) %>%
  summarize(p = shapiro.test(NA_Sales)$p.value)
