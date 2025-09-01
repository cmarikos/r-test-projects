bell <- Project_2_Bell_et_al_2014

# Immediately we can already see that sample sizes are quite different
sum(bell$`Male n`)
sum(bell$`Female N`)
shapiro.test(bell$`Female Mean`)
shapiro.test(bell$`Male Mean`)


# Distribution of rank is also pretty different
female_rank <- aggregate(bell$`Female N`, by = list(bell$Rank), FUN = sum)
male_rank <- aggregate(bell$`Male n`, by = list(bell$Rank), FUN = sum)
# Visualizing ranking distributions
f_bar <- barplot(female_rank$x, names.arg = female_rank$Group.1)
m_bar <- barplot(male_rank$x, names.arg = male_rank$Group.1)
?barplot()

# Economic theory suggests that scarcity of female business faculty 
# should be associated with a premium for their services. 
# The ratio of male to female faculty is approximately 3 to 1 in most collegiate schools of business.
# Bell p 41

aggregate(bell$`Female N`, by = list(bell$Rank), FUN = sum)
aggregate(bell$`Male n`, by = list(bell$Rank), FUN = sum)
aggregate(bell$`Male Mean`, by = list(bell$Rank), FUN = sum)
aggregate(bell$`Female Mean`, by = list(bell$Rank), FUN = sum)


bell$gender_rank <- paste(bell$)


# Two sample t-test: If the first sample is not different that the second sample p > 0.05

# H3: Means for faculty salaries do not differ between male and female faculty members.
t.test(x = bell$`Female Mean`, y = bell$`Male Mean`, alternative = "two.sided")
aov(bell)

# H4: Means for faculty salaries do not differ among the academic ranks of instructor/lecturer, assistant professor, associate professor and full professor.
# As far as I can tell, we can't calculate this with the .xlsx. The paper has overall mean, we only have gender means.

# H5: Means for faculty salaries do not differ among the four Carnegie Classifications of Research Universities-Very High Research Activity, Research Universities-High Research Activity, Doctoral Research Universities, Master's Colleges and Universities, and Baccalaureate Colleges-Diverse Fields.
# H6: Means for faculty salaries do not differ between male and female faculty members among the academic ranks of instructor/lecturer, assistant professor, associate professor and full professor.
# H7: Means for faculty salaries do not differ between male and female faculty members among the four Carnegie Classifications of Research Universities-Very High Research Activity, Research Universities-High Research Activity, Doctoral Research Universities, Master's Colleges and Universities, and Baccalaureate Colleges-Diverse Fields.
# H8: Means for faculty salaries do not differ among the academic ranks of instructor/lecturer, assistant professor, associate professor and full professor on the four Carnegie Classifications of Research Universities-Very High Research Activity, Research Universities-High Research Activity, Doctoral Research Universities, Master's Colleges and Universities, and Baccalaureate Colleges-Diverse Fields.
# H9: Means for the magnitude of faculty salaries do not differ between male and female faculty regardless of their rank as instructor/lecturer, assistant professor, associate professor and full professor on any of the four Carnegie Classifications of Research Universities-Very High Research Activity, Research Universities-High Research Activity, Doctoral Research Universities, Master's Colleges and Universities, and Baccalaureate Colleges-Diverse Fields.


