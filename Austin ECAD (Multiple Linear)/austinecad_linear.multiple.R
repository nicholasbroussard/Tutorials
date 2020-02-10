library(tidyverse)
library(inspectdf) #inspect_na(), inspect_num()
library(scales) #percent()
library(data.table)
library(summarytools) #descr() - for summaries of numerical variables
library(Metrics) #rmse()
library(jtools) #summ(), export_summs()
library(interactions) #cat_plot(), interact_plot()

#A) EDA
#1) Read in the df.
df <- fread("https://raw.githubusercontent.com/nicholasbroussard/tutorials/master/Austin%20ECAD%20(Multiple%20Linear)/austinecad_linear.multiple.csv", stringsAsFactors = F)

#2) Select variables.
df <- df %>%
  select("Average Monthly kWh", "ECAD Year Built", "Average Apt Size", "ECAD R Value", "ECAD Number of Floors", "ECAD Percent Duct Leakage") %>%
  #Let's the variables easier-to-read names.
  setnames(old = c("Average Monthly kWh", "ECAD Year Built", "Average Apt Size", "ECAD R Value", "ECAD Number of Floors","ECAD Percent Duct Leakage"),
           new = c("Energy",  "YearBuilt", "Size", "RValue", "Floors", "DuctLeakage")) %>%
  mutate(EnergyUsePerSft = Energy/Size)

#3) Look at the df's structure and convert vars as needed.
glimpse(df)

df$YearBuilt <- as.numeric(as.character(df$YearBuilt))
df$Floors <- as.factor(as.numeric(df$Floors))
df$YearBuilt[df$YearBuilt==0] <- NA

#4) Categoricals
df %>%
  inspect_cat() %>%
  show_plot()

ggplot(df, aes(x = Floors)) +
  geom_bar(color = "midnightblue", fill = "cadetblue", alpha = .7)

df <- df %>%
  filter(Floors == "1" | Floors == "2" | Floors == "3" |  is.na(Floors))

#5) Numerics.
df %>%
  descr(transpose = T,
        stats = "fivenum") 

a <- ggplot(df, aes(Energy)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30) 
b <- ggplot(df, aes(YearBuilt)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 40) +
  scale_x_continuous(limits = c(1887, 2019)) 
c <- ggplot(df, aes(Size)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30)
d <- ggplot(df, aes(RValue)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30)
e <- ggplot(df, aes(DuctLeakage)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30)
gridExtra::grid.arrange(a,b,c,d,e)

par(mfrow = c(2,2))
energy_outliers <- boxplot(df$Energy)$out
yearbuilt_outliers <- boxplot(df$YearBuilt)$out
size_outliers <- boxplot(df$Size)$out
rvalue_outliers <- boxplot(df$RValue)$out
ductleakage_outliers <- boxplot(df$DuctLeakage)$out

df[which(df$Energy %in% energy_outliers),] <- NA
df[which(df$YearBuilt %in% yearbuilt_outliers),] <- NA
df[which(df$Size %in% size_outliers),] <- NA
df[which(df$RValue %in% rvalue_outliers),] <- NA
df[which(df %>% ductleakage_outliers),] <- NA

#6) Missingness. Impute as necessary.
df %>%
  inspect_na() 
library(mice)
set.seed(101)
imp <- mice(df, m = 1, maxit = 1, print = F)
df <- complete(imp)

#8) Correlation.
df %>%
  GGally::ggcorr(label = TRUE, 
         label_alpha = TRUE,
         hjust=.9,
         size=3.5,
         layout.exp=3,
         method = c("pairwise.complete.obs", "spearman")) +
  labs(title="Correlation Matrix")

#7) Variance.
#Wide variances violate homoscedasticity. Scale coefs in model and add vifs.
df %>%
  summarise(Energy = var(Energy), YearBuilt = var(YearBuilt), Size = var(Size), RValue = var(RValue), DuctLeakage = var(DuctLeakage))



#B) MODELING

#1) Split the data. 

library(caTools)
set.seed(101)
sample <- sample.split(df, SplitRatio = .7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

#2) Run model and interpret.

set.seed(101)
model <- lm(Energy ~ ., data = train)
summ(model,
     confint = T,
     scale = T, 
     vifs = T) #Variable Inflation Factors

#y=556+36.5*YearBuilt+88*Size+3*RValue+6*2ndFloor+32*3rdFloor

# 1st floor unit energy use is $\ 547kWh \over month$ ($\ y=547$). 
# 1 year decrease in age is associated with increased energy use of $\ 26kWh \over month$. Specifically, a 1st floor unit that's 1 year newer than another unit is expected to use $\ 573kWh \over month$ ($\ y=547+26*1$). 
# 1 sft increase in size is associated with increased energy use of $\ 95kWh \over month$ after controlling for all other variables. Specifically, a 1st floor unit that's 1 sft bigger than the average unit is expected to use $\ 642kWh \over month$  ($\ y=547+95*1$).
# 1 unit increase in R-value thickness is associated with decreased energy use of $\ 3.5kWh \over month$ after controlling for all other variables. Specifically, a 1st floor unit that has 1-unit thicker R-value than the average unit would use $\  550.5kWh \over month$ ($\ y=547+3.5*1$).
# Going from the 1st floor to the second floor is associated with increased energy use of $\ 5kWh \over month$ after controlling for all other variables. Specifically, a unit that's on the 2nd floor but otherwise has the same characteristics as the average unit would use $\ 552kWh \over month$ ($\ y=547+5*1$).
# Going from the 1st floor to the 3rd floor is associated with increased energy use of $\ 44kWh \over month$ after controlling for all other variables. Specifically, a unit that's on the 3rd floor but otherwise has the same characteristics as the average unit would use $\ 591kWh \over month$ ($\ y=547+44*1$).

#3) Graphical analysis. Coefficients, predictor/outcome relationships, and influential points.

plot_summs(model, plot.distributions = T, scale = T)

a <- effect_plot(model, pred = YearBuilt, interval = T, plot.points = T, rug = T)
b <- effect_plot(model, pred = Size, interval = T, plot.points = T, rug = T)
c <- effect_plot(model, pred = RValue, interval = T, plot.points = T, rug = T)
d <- effect_plot(model, pred = Floors, interval = T, cat.geom = "line") #Categorical predictor
gridExtra::grid.arrange(a,b,c,d)

library(olsrr)
ols_plot_cooksd_chart(model) 
cooksd <- cooks.distance(model)
influential <- as.numeric(names(cooksd))[(cooksd > 4*mean(cooksd, na.rm = T))]
head(train[influential, ], 10)

#4) Residuals. Look for a mean near 0, normality, and even/centered spread against predicted points.
train$Residuals <- model$residuals
train$Fitted <- model$fitted.values
mean(train$Residuals)
ggplot(train, aes(x = model$residuals)) +
  geom_histogram(bins = 35, color = "black", fill = "cadetblue", alpha = .7) +
  theme_bw()
ggplot(train, aes(x=Fitted, y=Residuals)) +
  geom_point() +
  geom_smooth(method = "lm")

#5) Analyze predicted (fitted) values by querying the model.

#What's predicted energy use for homes built in 1981? 
train %>% 
  group_by(YearBuilt) %>% 
  filter(YearBuilt==1981) %>% 
  summarise(mean(Fitted))

#What's the predicted energy use of a 2nd floor unit larger than 600 sft? 
train %>% 
  filter(Floors=="2"&Size>600) %>% 
  summarize(mean(Fitted))

#6) Goodness of Fit

percent(summary(model)$r.squared) 
percent(summary(model)$adj.r.squared)
round(rmse(train$Energy, train$Fitted), digits=0) 

#7) Run predictions on test data and compare RMSEs for overfitting (smaller is better). 

test$Fitted <- predict(model, newdata = test)
df %>%
  summarise("Train Set RMSE" = round(rmse(train$Energy, train$Fitted), digits = 2), 
            "Test Set RMSE" = round(rmse(test$Energy, test$Fitted), digits=2))



#C) COMPARISONS

#Analyze the effects of predictor varibales on each other. Here, we focus on Size.

a <- interact_plot(model, pred = Size, modx = YearBuilt, plot.points = T, interval = T)
b <- interact_plot(model, pred = Size, modx = RValue, plot.points = T, interval = T)
c <- interact_plot(model, pred = Size, modx = Floors, plot.points = T, interval = T) #Categorical variable.
d <- interact_plot(model, pred = Size, modx = RValue, mod2 = Floors) #3-way interaction.

gridExtra::grid.arrange(a,b,c,d)
#Parallel lines indicate no interaction.

#Build and interpret model. Main effects go away for interactive model.
set.seed(101)
model2 <- lm(Energy ~ YearBuilt + Size + RValue + Floors + Size:YearBuilt, data = train)
summ(model2, scale = T, confint = T, digits = 2, vifs = T)

#y = 554 + 29*YearBuilt + 96*Size - 4*RValue + 3.5*2ndFloor + 44*3rdFloor - 15.87*Size:DuctLeakage$
#The average energy use is $\ 554kWh \over month$ ($\ y=554 $).    
#The effect of YearBuilt on Energy decreases by $\ 15.87kWh \over month $  for every 1-unit increase in RValue.
#Likewise, the effect of YearBuilt on Energy decreases by $\ 15.87kWh \over month $ for every 1 additional year. 


#Build a model w/o duct leakage.
set.seed(101)
model3 <- lm(Energy ~ YearBuilt + Size + RValue + Floors, data = train)
summ(model3, scale = T, confint = T, digits = 2, vifs = T)

#Build models pre1990 and 1990topresent.
train_pre1990 <- train %>%
  filter(YearBuilt<1990)
train_1990topresent <- train %>%
  filter(YearBuilt>=1990)
set.seed(101)
model4 <- lm(EnergyUsePerSft ~ YearBuilt + Size + RValue + Floors, data = train_pre1990)
summ(model4, scale = T, confint = T, digits = 2, vifs = T)
set.seed(101)
model5 <- lm(EnergyUsePerSft ~ YearBuilt + Size + RValue + Floors, data = train_1990topresent)
summ(model5, scale = T, confint = T, digits = 2, vifs = T)

#Model with dv as EnergyUsePerSft
set.seed(101)
model6 <- lm(EnergyUsePerSft ~ YearBuilt + Size + RValue + Floors + DuctLeakage, data = train)
summ(model6, scale = T, confint = T, digits = 4, vifs = T)

#Compare all models.
export_summs(model, model2, model3, model4, model5,model6,
             exp = T,
             scale = T, 
             digits = 4,
             error_format = "[{conf.low},{conf.high}]") #Include confidence intervals for each variable.
plot_summs(model, model2, scale = T, plot.distributions = T, model.names = c("Without Interation", "With Interaction"))



#D) FINDINGS

#Newer units consume more energy, even after controlling for size, rvalue, number of floors, and duct leakage.
#Larger units are significantly more likely to consumer far more energy, even after controlling for age, rvalue, floors, and duct leakage.


a <- ggplot(df %>% filter(YearBuilt>1955), aes(YearBuilt, Energy)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Energy Use Per Unit Over Time", y = "kWh")
b <- ggplot(df %>% filter(YearBuilt>1955), aes(YearBuilt, EnergyUsePerSft)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Energy Use Per Sft Over Time", y = "kWh")
gridExtra::grid.arrange(a,b, ncol=2)

ggplot(df, aes(Size, Energy)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(df, aes(Size, EnergyUsePerSft)) +
  geom_point() +
  geom_smooth(method = "loess")

#What's the average energy use for a 400sft unit v 800sft unit?
train %>%
  filter(Size >= 400 & Size <= 500) %>%
  summarize(mean(Fitted))

train %>%
  filter(Size >= 800 & Size <= 900) %>%
  summarize(mean(Energy))

#E) SOURCES
#Tools for Summarizing and Vizualising Regression Models: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
#Removing Outliers: https://rpubs.com/Mentors_Ubiqum/removing_outliers
#Cook's D: https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html
#Outlier Treatment: http://r-statistics.co/Outlier-Treatment-With-R.html
#Visualizing Residuals: https://drsimonj.svbtle.com/visualising-residuals 
#Visualizing Predictions: https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html 
#Interactions with Continuous Predictors: https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html
#Interactions with Categorical Predictors: https://cran.r-project.org/web/packages/interactions/vignettes/categorical.html
#DataCamp's LR Tutorial: https://www.datacamp.com/community/tutorials/linear-regression-R
#Another Tutorial: http://r-statistics.co/Linear-Regression.html
#Interpreting Interactions in Regression: https://www.theanalysisfactor.com/interpreting-interactions-in-regression/