# ANOVA  of Sex and Country
# Question: how would I use the "Sex and Country" column? I feel like the data needs to be separated unless there is a way to mark delimiters?
SexCountryAOV <- aov(P2_Binturong$`Popcorn Chemical Concentration pg/mL` ~ P2_Binturong$`Sex and Country`, data = P2_Binturong)
summary(SexCountryAOV)

# Tukey test of "Sex and Country"
TukeyHSD(SexCountryAOV)

?TukeyHSD

# T-test by "Sex" column 
male_df <- subset(P2_Binturong, Sex == "Male", select = `Popcorn Chemical Concentration pg/mL`)
female_df <- subset(P2_Binturong, Sex == "Female", select = `Popcorn Chemical Concentration pg/mL`)
t.test(x = male_df$`Popcorn Chemical Concentration pg/mL`, y = female_df$`Popcorn Chemical Concentration pg/mL`, alternative = "two.sided")

# ANOVA of "Country"
CountryAOV <- aov(`Popcorn Chemical Concentration pg/mL` ~ `Country`, data = P2_Binturong)
summary(CountryAOV)

# Tukey of "Country"
# Question: why is my Country Tukey pairwise comparison different than the Country portion of the one above?
# Answer: because I'm probably doing this wrong lol
TukeyHSD(CountryAOV)

# Shapiro-Wilk test, which tells us that this data is not normally distributed
shapiro.test(P2_Binturong$`Popcorn Chemical Concentration pg/mL`)

#Create country dfs
thailand_df <- subset(P2_Binturong, Country == "Thailand", select = `Popcorn Chemical Concentration pg/mL`)
indonesia_df <- subset(P2_Binturong, Country == "Indonesia", select = `Popcorn Chemical Concentration pg/mL`)
malaysia_df <- subset(P2_Binturong, Country == "Malaysia", select = `Popcorn Chemical Concentration pg/mL`)

# Shapiro-Wilk test on each subset
shapiro.test(male_df$`Popcorn Chemical Concentration pg/mL`)
shapiro.test(female_df$`Popcorn Chemical Concentration pg/mL`)
shapiro.test(thailand_df$`Popcorn Chemical Concentration pg/mL`)
shapiro.test(indonesia_df$`Popcorn Chemical Concentration pg/mL`)
shapiro.test(malaysia_df$`Popcorn Chemical Concentration pg/mL`)

# Mean, median, sd, and sample number, by country and sex
# males
mean(male_df$`Popcorn Chemical Concentration pg/mL`)
median(male_df$`Popcorn Chemical Concentration pg/mL`)
sd(male_df$`Popcorn Chemical Concentration pg/mL`)
length(male_df$`Popcorn Chemical Concentration pg/mL`)

#females
mean(female_df$`Popcorn Chemical Concentration pg/mL`)
median(female_df$`Popcorn Chemical Concentration pg/mL`)
sd(female_df$`Popcorn Chemical Concentration pg/mL`)
length(female_df$`Popcorn Chemical Concentration pg/mL`)

#Thailand
mean(thailand_df$`Popcorn Chemical Concentration pg/mL`)
median(thailand_df$`Popcorn Chemical Concentration pg/mL`)
sd(thailand_df$`Popcorn Chemical Concentration pg/mL`)
length(thailand_df$`Popcorn Chemical Concentration pg/mL`)

#Indonesia
mean(indonesia_df$`Popcorn Chemical Concentration pg/mL`)
median(indonesia_df$`Popcorn Chemical Concentration pg/mL`)
sd(indonesia_df$`Popcorn Chemical Concentration pg/mL`)
length(indonesia_df$`Popcorn Chemical Concentration pg/mL`)

#Malaysia
mean(malaysia_df$`Popcorn Chemical Concentration pg/mL`)
median(malaysia_df$`Popcorn Chemical Concentration pg/mL`)
sd(malaysia_df$`Popcorn Chemical Concentration pg/mL`)
length(malaysia_df$`Popcorn Chemical Concentration pg/mL`)


