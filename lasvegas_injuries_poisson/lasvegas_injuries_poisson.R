library(tidyverse)
library(inspectdf)
library(summarytools)
library(scales) #percent
library(data.table)
library(jtools) #summ(), export_summs()
library(interactions) #cat_plot(), interact_plot()


#A) EXPLORE AND CLEAN

#1) Read in df.

df <- fread("https://raw.githubusercontent.com/nicholasbroussard/tutorials/master/Las%20Vegas%20Injuries%20(Poisson)/lasvegasinjuries_poisson.csv",
            stringsAsFactors = F)

#2) Select variables.

df <- df %>%
  select(Injured, Crash_Date, Crash_Time, Weather)

#3) Analyze structure.

glimpse(df)

library(lubridate)
df$Crash_Date <- ymd_hms(df$Crash_Date)
df$Crash_Time <- ymd_hms(df$Crash_Time)
df <- df %>%
  #Make a column for month and hour
  mutate(Month = month(Crash_Date),
         Hour = as.numeric(paste0(hour(Crash_Time), "00"))) %>%
  #Remove original time cols.
  select(-Crash_Date, -Crash_Time)

#4) Categoricals

df %>%
  inspectdf::inspect_cat() %>%
  show_plot()

a <- ggplot(df, aes(x = Weather)) +
  geom_bar(color = "midnightblue", fill = "cadetblue", alpha = .7) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 5))
b <- ggplot(df, aes(Month)) +
  geom_bar(color = "midnightblue", fill = "cadetblue", alpha = .7) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 5))
gridExtra::grid.arrange(a,b, ncol = 2)

df$Weather[df$Weather == "UNKNOWN"] <- NA
df <- df %>%
  filter(Weather == "CLEAR" | Weather == "CLOUDY" | Weather == "RAIN" | is.na(Weather))

#5) Numerics

df %>%
  summarytools::descr(transpose = T,
        stats = "fivenum") 

a <- ggplot(df, aes(Injured)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30) 
b <- ggplot(df, aes(Hour)) +
  geom_bar(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30) +
  scale_x_continuous(breaks = seq(0,2400,by = 200))
gridExtra::grid.arrange(a,b)

par(mfrow = c(1,2))
injured_outliers <- boxplot(df$Injured)$out
hour_outliers <- boxplot(df$Hour)$out

#6) Missingness. Impute if needed.

df %>%
  inspect_na()

library(mice)
df$Weather <- as.factor(as.character(df$Weather)) #Cannot impute character variables.
imp <- mice(df, m = 1, maxit = 1, print = F)
df <- complete(imp)
df$Weather <- as.character(df$Weather) #COnvert from type impute.

#7) Correlation.

df %>%
  GGally::ggcorr(label = TRUE, 
         label_alpha = TRUE,
         hjust=.9,
         size=3.5,
         layout.exp=3,
         method = c("pairwise.complete.obs", "spearman")) +
  labs(title="Correlation Matrix")


#B) MODEL

#1) Split the data.

library(caTools)
set.seed(101)
sample <- sample.split(df, SplitRatio = .7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

#2) Build and interpret the model.
set.seed(101)
model <- glm(Injured ~ ., data = train, family = poisson(link = "log"))
summ(model, exp = T, digits = 4)

#y=0.59+1.02*Cloudy+0.80*Rainy+1*Month+1*Hour
#The likelihood of injury given a traffic accident is 59% after controlling for all other variables ($\ y=0.59$). 
#Going from clear to cloudy weather results in an increase in the number of injured passengers per accident. 
#Going from clear to rainy weather results in a decrease in the number of injured passengers per accident. Another way of saying this is that on a rainy weather day, the number of injuries in car accidents will fall by `r percent(1-.8)` after controlling for all other variables.
#Each additional hour in the day, starting at midnight, results in an increase in the number of injured persons per accident.  Another way of saying this is that the number of passengers injured per car accident increases infinitessmaly as the day progresses after controlling for all other variables. 

#3) Graphical analysis. 

plot_summs(model, plot.distributions = T, exp = T)

a <- effect_plot(model, pred = Weather, interval=T, cat.geom = "line") #Categorical predictor.
b <- effect_plot(model, pred = Month, interval = T, partial.residuals = T, rug = T)
c <- effect_plot(model, pred = Hour, interval = T, partial.residuals = T, rug = T)
gridExtra::grid.arrange(a,b,c, ncol = 2)

#4) Analyze residuals.

train$Fitted <- model$fitted.values
train$Residuals <- model$residuals


#5) Query the model based on predictions.

#What is the average number of injured passengers per accident at 1600 each day? 
train %>% 
  filter(Hour == "1500") %>% 
  summarise(mean(Fitted))`
 

#6) Goodness of Fit

model$deviance/model$df.residual #If the ratio of the null deviance to residual deviance degrees of freedom is > 1, overdispersion.
AER::dispersiontest(model, trafo = 1) #If the y var mean and var aren't equal, then overdispersion.
pchisq(model$deviance, df = model$df.residual, lower.tail = F)


#C) COMPARE

#1) Run alternate models.

#Quasipoisson accounts for overfitting.
set.seed(101)
model_quasi <- glm(Injured ~ Weather + Month + Hour, data = train, family = quasipoisson(link = "log"))
summ(model_quasi, exp = T, digits = 4)
#Goodness of Fit
AER::dispersiontest(model, trafo = 0)

#Negative binomial.
set.seed(101)
model_nb <- MASS::glm.nb(Injured ~ Weather + Month + Hour, data = train)
summ(model_nb, exp = T, digits = 4)
#Goodness of fit for negative binomial.
pscl::odTest(model_nb)

#3) Compare all the models.

export_summs(model, model_quasi, model_nb, digits = 4)
anova(model, model_quasi, model_nb)




#D) EXTRAPOLATE



#E) SOURCES

#Negative Binomial Regression: https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
#Deviance Goodness of Fit: https://thestatsgeek.com/2014/04/26/deviance-goodness-of-fit-test-for-poisson-regression/
#Overdispersion Tests: https://stats.stackexchange.com/questions/66586/is-there-a-test-to-determine-whether-glm-overdispersion-is-significant

