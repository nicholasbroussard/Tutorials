library(tidyverse)
library(MASS) #Use polr() ("Propotional Odds" Logistic Regression. aka Ordinal Logistic Regression/Ordered Logit Model/Ordered Logistic Regression)
library(carData) #COntains WVS data
library(effects) #Contains visualizing functions
library(car) #Contains Anova(), which allows us to check coefs for significance.
library(jtools)
library(interactions)

data(WVS)

#Run an ordinal regression without interactions. 

wvs.1 <- polr(poverty ~ gender + religion + degree + age, data = WVS, Hess = T)
summary(wvs.1)

#The intercept has two thresholds since there are three levels in the dependent variable.
#The first threshold is going from level 1 (Too little) to level 2 (about right).
#The second threshold is going from level 2 (about right) to level 3 (too much).

#Test for significance using ANOVA, even tho we can already tell that gender and age are signficant bc their t-values are >2
Anova(wvs.1)

#Exponentiate the coefs so we can read them in odds. 
exp(coef(wvs.1))

#The coefficients are read as the odds that a one unit increase corresponds to choosing the next response level.
#Male responses, as opposed to female responses, are associated with a 16% increase in the odds of choosing About Right instead of Too Little, or Too Much instead of About Right.  
#An increase in age of one year is associated with a 1% increase in the odds of choosing About Right instead of Too Little, or Too Much instead of About Right.

#Plot the coefs with plot_summs. This function won't work if you want to plot predictions. 
plot_summs(wvs.1, scale = T, exp = T) +
  labs(x = "Likelihood")
#Age, having a degree, and being a male are all associated with increased odds of choosing level 2 instead of 1, or level 3 instead of 2.
#Being religious is associated with decreased odds of choosing level 2 instead of 1, or level 3 instead of 2. AKA - religious folks are more likely to want the go to do more. 

#Plot predictions so that we can visualize the effect of across all three levels.
plot(Effect(focal.predictors = c("age"), wvs.1), rug = F) 
#As age increases, an individual is more likely to pick Too Much or About Right, and less likely to pick Too Little. 
#AKA - The older you get, the less you want government intervention in poverty measures. 


#Now lets run an ordinal logistic model with the same data + interactions.
wvs.2 <- polr(poverty ~ country*(gender + religion + age), data = WVS)
summary(wvs.2)

#Check for significance. 
Anova(wvs.2)
#Everything is significant except interactions between country and gender.

#Look at the coefficients in terms of odds. 
plot_summs(wvs.2, scale = T, exp =  T)
#We see decreased odds of selecting a higher level on the survey from Swedish and Norweigan relious people, but high odds from all other predictors. 
#AKA - Religious people from Sweden and Norway are likely than religious Australians to want less government intervention (ie more likely to want intervention).

#Now lets look at how predictions from the country and age variables compare regarding government interventon in poverty preferences. 
plot(Effect(focal.predictors = c("age", "country"), wvs.2), rug = F, xlab = "Age") 
#Across all countries except Norway, the probability of selecting "Too Much" or "About Right" increases with age.
#Across all countries except Norway, the probability of selecting "Too Little" decreases with age.  

#If we want to see predictions for a specific set of reference categories, use given.values. 
#For example, I want to know how the story about age and country of origin change for religious men without a college degree. 
plot(Effect(focal.predictors = c("age", "country"), mod = wvs.2, given.values = c(gendermale = 1, religionyes = 1)), rug = F)

#We can also narrow in the range of the focal predictors using xlevels.
plot(Effect(focal.predictors = c("age", "country"), mod = wvs.2, xlevels = list(age = seq(20,80, by = 10))), rug = F)


#We can also see teh results stacked on eachother like an area chart using style = "stacked". 
plot(Effect(focal.predictors = c("age", "country"), mod = wvs.2), rug = F, style = "stacked")

#Sources
#https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5
#https://www.r-bloggers.com/how-to-perform-ordinal-logistic-regression-in-r/
#https://data.library.virginia.edu/visualizing-the-effects-of-proportional-odds-logistic-regression/
#https://www.statisticssolutions.com/how-to-interpret-an-ordinal-logistic-regression/