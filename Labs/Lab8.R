library(ggplot2)
library(dplyr)
library(reshape2)

#cars matrix
cars <- data.frame(Color = c("White", "Black", "Everything Else"),
                   Accidents = c(252, 109, 450), TotalCars = c(2023, 4005, 12124))
poopoo <- data.frame(DevilsCoffeeStatus = c("Butt Barfing", "Enjoying
Life"),
                     Control = c(35, 78), Medication = c(53, 13))

#total number of cars
all <- sum(cars$TotalCars)

#percentage of cars in each category
cars <- cars %>% mutate(Proportion.Of.Cars = TotalCars/all)

#cars goodness of fit chi-squared
chisq.test(x= cars$Accidents, p = cars$Proportion.Of.Cars)

#effect size = 811
total.accidents <- sum(cars$Accidents)
cars <- cars %>% mutate(ExpectedAccidents = total.accidents*Proportion.Of.Cars) %>%
  mutate(EffectSize = Accidents/ExpectedAccidents)

#diarhhea matrix
muddymatrix <- poopoo %>%
  select(Control, Medication) %>%
  as.matrix()

#diarhhea independence chi-squared
chisq.test(x= muddymatrix)

#poop meds effect size
poopoo <- poopoo %>%
  mutate(EffectSize = Medication/Control)

#kaggle jet engine birdies data
birds <- read_csv("Documents/ISTA_116/angrybirds.csv")

#filtering to Tucson and Phoenix airports
birbs1 <- birds %>%
  filter(Airport %in% c("TUCSON INTL","PHOENIX SKY HARBOR INTL ARPT"))

#total species count by airport
birbs1 <- birbs1 %>%
  group_by(Species.Name, Airport) %>%
  tally()

#make it into wide form for chi-squared
widebirbs <- birbs1 %>%
  dcast(Species.Name~Airport, value.var = "n", fill = 0)

#filter down widebirbs
somewidebirbs <- widebirbs %>%
  mutate(Total =`TUCSON INTL`+`PHOENIX SKY HARBOR INTL ARPT` ) %>%
  #add species together
  filter(`TUCSON INTL` != 0 & `PHOENIX SKY HARBOR INTL ARPT`!= 0) %>%
  #Remove 0-observation species 
  filter(Total >5) #filter to only be reasonably common animals

#make somebirbs long for ggplot2
somelongbirbs <- somewidebirbs %>%
  select(-Total) %>%
  melt()

#plot somelongbirbs
ggplot(somelongbirbs, aes(y = Species.Name, x = value, fill
                          =variable ))+
  geom_bar(stat="identity", position = "fill")

#birbs martix for chi-squared
matrixbirbs <- somewidebirbs %>%
  select(`TUCSON INTL`,`PHOENIX SKY HARBOR INTL ARPT` ) %>%
  as.matrix()
chisq.test(x= matrixbirbs)

#goodness of fit collision birbs
P.birbs <- sum(widebirbs$`PHOENIX SKY HARBOR INTL ARPT`)
T.birbs <- sum(widebirbs$`TUCSON INTL`)
deadbirbs <- c(P.birbs, T.birbs)
birbs.p <- c(.5, .5)
chisq.test(x = deadbirbs, p = birbs.p)

#updated airport probability data
P.p <-310000/(124000 + 310000) #phoenix probability
T.p <- 124000/(124000 + 310000) #tucson probability
planes.p <- c(P.p, T.p)
chisq.test(deadbirbs, p=planes.p)

#effect size
(P.birbs/(P.birbs + T.birbs))/P.p


#____part 4 from here down____
#filter to Dulles and Ronald Regan airports
GovernmentSpyBirbs <- birds %>%
  filter(Airport %in% c("WASHINGTON DULLES INTL ARPT","RONALD REAGAN WASHINGTON NATIONAL ARPT"))

#total species by DC airport
GovernmentSpyBirbs <- GovernmentSpyBirbs %>%
  group_by(Species.Name, Airport) %>%
  tally()

#make it into wide form for chi-squared
WideSpyBirbs <- GovernmentSpyBirbs %>%
  dcast(Species.Name~Airport, value.var = "n", fill = 0)

#filter down WideSpyBirbs to remove any zeros or birds that have less than 5 deaths total
FilteredWideSpyBirbs <- WideSpyBirbs %>%
  mutate(Total =`WASHINGTON DULLES INTL ARPT`+`RONALD REAGAN WASHINGTON NATIONAL ARPT` ) %>%
  #add species together
  filter(`WASHINGTON DULLES INTL ARPT` != 0 & `RONALD REAGAN WASHINGTON NATIONAL ARPT`!= 0) %>%
  #Remove 0-observation species 
  filter(Total >5) 
  #filter to only be reasonably common animals

#make somebirbs long for ggplot2
FilteredLongSpyBirbs <- FilteredWideSpyBirbs %>%
  select(-Total) %>%
  melt()

#plot FilteredLongSpyBirbs
ggplot(FilteredLongSpyBirbs, aes(y = Species.Name, x = value, fill
                          =variable ))+
  geom_bar(stat="identity", position = "fill")

#spybirbs martix for chi-squared
MatrixSpyBirbs <- FilteredWideSpyBirbs %>%
  select(`WASHINGTON DULLES INTL ARPT`,`RONALD REAGAN WASHINGTON NATIONAL ARPT` ) %>%
  as.matrix()

#chi-squared test for DC area airport dead bird species 
chisq.test(x= MatrixSpyBirbs)

#assuming 50/50 probs
D.spybirbs <- sum(WideSpyBirbs$`WASHINGTON DULLES INTL ARPT`)
R.spybirbs <- sum(WideSpyBirbs$`RONALD REAGAN WASHINGTON NATIONAL ARPT`)
deadspybirbs <- c(P.spybirbs, T.spybirbs)
spybirbs.p <- c(.5, .5)
chisq.test(deadspybirbs, p=spybirbs.p)

# if 50/50 probs spybirb effect size
#Ronald Regan Airport
(R.spybirbs/(R.spybirbs + D.spybirbs))/spybirbs.p
#Dulles Airport
(D.spybirbs/(R.spybirbs + D.spybirbs))/spybirbs.p


#goodness of fit for actual probs
R.p <-293674/(232972 + 293674) #Ronald Regan Airport
D.p <- 232972/(232972 + 293674) #Washington Dulles Airport
dcplanes.p <- c(R.p, D.p)
chisq.test(deadspybirbs, p=dcplanes.p)

#actual spybirb effect size
#Ronald Regan Airport
(R.spybirbs/(R.spybirbs + D.spybirbs))/R.p
#Dulles Airport
(D.spybirbs/(R.spybirbs + D.spybirbs))/D.p
