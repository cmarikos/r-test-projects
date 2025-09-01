DeLong <- read_csv("Documents/ISTA_116/Project 4 DeLong 2018 Full Data.csv")

unique(DeLong$RHQ131)
unique(DeLong$IMQ040)
unique(DeLong_Filtered$RIDAGEYR)
range(DeLong_Filtered$INDFMPIR)

library(dplyr)

# filtering to get to n = 700 
DeLong_Filtered <- DeLong %>%
  filter(RHQ131 %in% c(1,2)  
         & IMQ040 %in% c(1,2) 
         & !is.na(INDFMPIR)) %>%
  # remember to mutate ^ to 1/0 instead of 1/2
  mutate(
    Hispanic = as.numeric(RIDRETH1 %in% c(1,2)),
    NH_White = as.numeric(RIDRETH1 == 3),
    NH_Black = as.numeric(RIDRETH1 == 4),
    Other_Race = as.numeric(RIDRETH1 == 5) 
  ) %>%
  mutate(
    DMDEDUC2 = ifelse(DMDEDUC2 == 1, 1, 0),
    RHQ131 = ifelse(RHQ131 == 1, 1, 0),
    IMQ040 = ifelse(IMQ040 == 1, 1, 0)
  )

DeLong_Filtered2 <- DeLong %>%
  filter(RHQ131 %in% c(1,2)  
         & IMQ040 %in% c(1,2) 
         & SXD021 != 2
         & RHD442 != 1
         & RHD280 != 1
         & !is.na(INDFMPIR)) %>%
  # remember to mutate ^ to 1/0 instead of 1/2
  mutate(
    Hispanic = as.numeric(RIDRETH1 %in% c(1,2)),
    NH_White = as.numeric(RIDRETH1 == 3),
    NH_Black = as.numeric(RIDRETH1 == 4),
    Other_Race = as.numeric(RIDRETH1 == 5) 
  ) %>%
  mutate(
    DMDEDUC2 = ifelse(DMDEDUC2 == 1, 1, 0),
    RHQ131 = ifelse(RHQ131 == 1, 1, 0),
    IMQ040 = ifelse(IMQ040 == 1, 1, 0)
  )

# use as.factor function to create binary flags

# n=700, very good
length(DeLong_Filtered$SEQN)

# multivariate logistic regression
# glm(y ~ x1 + x2 + x3…, data = dataframe, family = binomial)
# then use the summary function


model700all <- glm(RHQ131 ~ DMDEDUC2 + DMDMARTL + IMQ040 + Hispanic + NH_White + NH_Black + Other_Race + RIDAGEYR + INDFMPIR,
    data = DeLong_Filtered, family = binomial)
summary(model700all)

model700lim <- glm(RHQ131 ~ DMDEDUC2 + DMDMARTL + RIDAGEYR + INDFMPIR,
                   data = DeLong_Filtered, family = binomial)
summary(model700lim)

plot(model700all)
plot(model700lim)



#IMQ040                  -1.259e+00  3.876e-01  -3.248  0.00116 **

modelIMQ1 <- glm(RHQ131 ~ IMQ040, data = DeLong_Filtered, family = quasibinomial(link = "logit"))
summary(modelIMQ1)

modelIMQ2 <- lm(RHQ131 ~ IMQ040, data = DeLong_Filtered2)
summary(modelIMQ2)

?glm()
