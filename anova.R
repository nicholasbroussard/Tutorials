#https://www.guru99.com/r-anova-tutorial.html
#https://www.graphpad.com/guides/prism/7/statistics/f_ratio_and_anova_table_(one-way_anova).htm?toc=0&printWindow
#https://www.technologynetworks.com/informatics/articles/one-way-vs-two-way-anova-definition-differences-assumptions-and-hypotheses-306553

library(tidyverse)

#Purpose: Compare the means between multiple groups
#F-Stat = (Between Group Variability)/(Within Group Variability)
#Reject the null if the F-stat is significant.
#If the F-value is close to 1, you generally fail to reject the null hypothesis. 

PATH <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/poisons.csv"
df <- read.csv(PATH) %>%
  select(-X) %>% 
  mutate(poison = factor(poison, ordered = TRUE))
glimpse(df)

#Is there a difference in the mean survival time based on the poison (1, 2, or 3)?
#Get the count of each poison group, the mean time and the sd.

levels(df$poison)

#Check the numbers
df %>%
  group_by(poison) %>%
  summarise( 
    count_poison = n(),
    mean_time = mean(time),
    sd_time = sd(time))

#One Way ANOVA: Examines the effect of three  or more levels of one indpendent variable on the dependent variable
#H0: There is no difference in the means of any factor.
#HA: At least one factor has a different mean from the other factors.
# Assumptions: Each factor is independent, randomly sampled, and normally distributed with equal amounts of variation both within and between groups. 


#Run the ANOVA
anova_one_way <- aov(time~poison, data=df)
summary(anova_one_way)

#Two Way ANOVA: Examines the effect of two independent variables on the dependent variable

#H0a: There is no difference in the means of any level of the first variable on the dependent variable.
#HAa: There is a difference in the means of at least one level of the first variable on the dependent variable.
#H0b: There is no difference in the means of any level of the second variable on the dependent variable.
#HAb: There is a difference in the means of at least one level of the second variable on the dependent variable.
#H0c: There is no interaction between the two independent variables.
#HAc: There is interaction between the two independent variables.
# Assumptions:

anova_two_way <- aov(time ~ poison + treat, data = df)
summary(anova_two_way)