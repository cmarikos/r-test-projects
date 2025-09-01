vgsales <- read_csv("Documents/ISTA_116/vgsales.csv")
?subset

install.packages("dplyr")
install.packages("ggplot2")

library("dplyr")
library("ggplot2")

IFa <- subset(vgsales, Publisher == "Idea Factory")
Nin <- subset(vgsales, Publisher == "Nintendo")
VG <- subset(vgsales, Publisher %in% c("Nintendo", "Idea Factory"))

ggplot(IFa, aes(x=NA_Sales)) + geom_histogram()

ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(col="blue") 
ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(fill="green") 
ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(alpha=.5)
ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(binwidth=.02)

ggplot(IFa, aes(x=NA_Sales)) + geom_histogram(fill="green", col = "blue", alpha = .5) 

ggplot(IFa, aes(x = NA_Sales)) + geom_histogram(fill="blue")
ggplot(IFa, aes(x = NA_Sales)) + geom_histogram(aes(fill="blue"))
ggplot(IFa, aes(x = NA_Sales), fill = "blue") + geom_histogram()
ggplot(IFa, aes(x = NA_Sales, fill = "blue")) + geom_histogram()

ggplot(IFa, aes(x=NA_Sales, fill="blue")) + geom_histogram(alpha = .5)

ggplot(IFa, aes(x=NA_Sales)) + geom_density()

ggplot(IFa, aes(x=NA_Sales)) + geom_dotplot()

ggplot(IFa, aes(x=NA_Sales, y = Year)) + geom_point()

ggplot(IFa, aes(x=NA_Sales, y = Genre)) + geom_boxplot()

ggplot(IFa, aes(x=NA_Sales, y = Genre)) + geom_violin()

ggplot(IFa, aes(x=NA_Sales, fill = Genre)) + geom_density(alpha = .5)

ggplot(IFa, aes(x=NA_Sales, fill = Genre)) + 
  geom_density(alpha = .5) + 
  facet_wrap(~Genre)

ggplot(IFa, aes(x=Genre)) + geom_bar()

ggplot(data = VG, aes(x = Genre, fill = Publisher))+
  geom_bar()

ggplot(data = VG, aes(x = Platform, fill = Publisher))+
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(Nin, aes(x=NA_Sales, fill = Genre)) + 
  geom_density(alpha = .5)+
  scale_x_continuous(limits = c(1, 55))


Table1 <- VG %>%
  group_by(Genre) %>%
  summarize(Mean = mean(NA_Sales))

Table2 <- VG %>%
  group_by(Genre, Publisher) %>%
  summarize(Mean = mean(NA_Sales))

Table3 <- VG %>%
  group_by(Genre, Publisher) %>%
  summarize(Mean = mean(NA_Sales), Maximum = max(NA_Sales))

MedSales <- VG %>% group_by(Genre, Publisher) %>% summarize(Median = mean(NA_Sales))

Table4 <- VG %>%
  group_by(Publisher) %>%
  summarize(p = shapiro.test(NA_Sales)$p.value)

Table5 <- VG %>%
  group_by(Genre) %>%
  summarize(p = shapiro.test(NA_Sales)$p.value)

ggplot(data = VG, aes(x = Platform, fill = Publisher))+
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(VG, aes(x = Platform, fill = Publisher)) + 
  geom_dotplot(position = position_dodge(preserve = "single"))

ggplot(VG, aes(x= NA_Sales, y = Platform, fill = Publisher)) + geom_boxplot()

PlaformReleases <- VG %>%
  group_by(Publisher, Platform) %>%
  summarize(Mean = mean(NA_Sales))

ggplot(VG, aes(x=Publisher, fill = Publisher)) + 
  geom_histogram(stat="count", position = position_dodge(preserve = "single")) + 
  facet_wrap(~Genre) 

ggplot(VG, aes(x = NA_Sales)) + 
  geom_bar()

NewTable <- VG %>% group_by(Genre, Publisher) %>%  summarize(Median = mean(NA_Sales))


