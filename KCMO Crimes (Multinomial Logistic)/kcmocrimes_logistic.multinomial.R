library(tidyverse)
library(kableExtra)
library(inspectdf)
library(summarytools)
library(scales) #percent
library(data.table)
library(jtools) #summ(), export_summs()
library(interactions) #cat_plot(), interact_plot()


#A) EXPLORE AND CLEAN

#1) Read in the df. https://data.kcmo.org/Crime/KCPD-Crime-Data-2018/dmjw-d28i

df <- read.csv("logistic.multinomial_crimesbyrace_kcmo.csv", stringsAsFactors = F)

#2) View structure.

glimpse(df)
df %>%
  group_by(Description) %>%
  count() %>%
  arrange(desc(n))
table(df$Description)
df <- df %>%
  filter(Description=="Property Damage" | Description=="Stealing From Auto" | Description=="Non Agg Assault Dome")

#3) Select vars.

df <- df %>%
  select(Description, Race, Age, Sex, Reported_Time, Reported_Date)

#4) Categoricals, including distribution. Restrict/convert as necessary. 

df %>%
  inspect_cat() %>%
  show_plot()

library(lubridate)
df$dtg <- paste(df$Reported_Date, df$Reported_Time)
df$dtg <- mdy_hm(df$dtg)
df$Hour <- as.numeric(paste0(hour(df$dtg), "00"))
df$Month <- month(df$dtg)

df <- df %>%
  select(-dtg, -Reported_Time, -Reported_Date)

table(df$Race)
df$Race[df$Race == "U" | df$Race == ""] <- NA
df <- df %>%
  filter(Race != "I" & Race != "P")

table(df$Sex)
df$Sex[df$Sex == "U"] <- NA

#5) Numericals... 5-num-sum, histograms, and outliers. Restrict/eliminate/recode as necessary.

df %>% 
  descr(transpose = T, stats = "fivenum")

ggplot(df, aes(x=Age)) +
  geom_bar()
ggplot(df, aes(x=Hour)) +
  geom_bar()
ggplot(df, aes(x=Month)) +
  geom_bar()

age_outliers <- boxplot(df$Age)$out
df[which(df$Age %in% age_outliers),] <- NA

#6) Missingness. Recode to implicit NAs. Impute if needed. 

df %>%
  inspect_na() 
summary(df)
df$Sex <- as.factor(as.character(df$Sex))
set.seed(101)
library(mice)
imp <- mice(df, m = 1, maxit = 1, print = F)
summary(complete(imp))
df <- complete(imp)
df$Age <- as.numeric(df$Age)
df$Sex <- as.character(df$Sex)
glimpse(df)

#7) Correlation. Eliminate predictors >= 70% to control multicolinearity.

df %>%
  GGally::ggcorr(label = TRUE, 
                 label_alpha = TRUE,
                 hjust=.9,
                 size=3.5,
                 layout.exp=3,
                 method = c("pairwise.complete.obs", "spearman"))

#8) Variance. Scale if necessary. Can also scale in the model. In logistic regressions, variance is not important with regard to scale.

var(df$Age)
var(df$Hour)
var(df$Month)




#B) MODEL

#1) Split the data.

library(caTools)
set.seed(101)
sample <- sample.split(df, SplitRatio = .7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

#2) Build model and interpret results. Scale vars or exp coefs when necessary.

df$Race <- relevel(as.factor(df$Race), ref = "W")
df$Description <- relevel(as.factor(df$Description), ref = "Non Agg Assault Dome")
set.seed(101)
library(nnet)
model <- multinom(Description ~ ., data = train)
summary(model)
odds <- exp(summary(model)$coefficients)

#Model: y=0.59+1.02*Cloudy+0.80*Rainy+1*Month+1*Hour

#Odds of being arrested for property damage as opposed to domestic assault...
#70% higher for Asians than Whites.
#25% lower for Blacks than Whites. 
#1% higher for every increased year of age.
#24% higher for men than women.
#Slight decrease hour to hour.
#slight decrease month to month. 

#Odds of being arrested for auto theft as opposed to domestic assault...
#145% higher for Asians than whites.
#72 lower for blacks than whites.
#1% higher for each additional year of age.
#40% higher for men than women. 
#Slight decrease hour to hour.
#Slight decrease month to month. 

zvalues <- summary(model)$coefficients/summary(model)$standard.errors
pvalues <- pnorm(abs(zvalues), lower.tail = F)*2
#All predictors are significant except month related to auto theft.

#3) Graphical analysis. View the coefficient effects.

plot_summs(model, exp = T)

#4) Analyze residuals. Use binned residual plot (better for logistic).

train$Residuals <- model$residuals
arm::binnedplot(fitted(model), residuals(model, type="response"))

#5) Analyze predicted (fitted) probabilities by querying the model. 

train$Fitted <- model$fitted.values
#What are the odds of arrest for auto theft as opposed to domestic violence for a man over 30?
train %>%
  filter(Description=="Stealing From Auto" & Age>=30 & Sex=="M") %>%
  summarize(mean(Fitted)*100)

#6) Evaluate goodness of fit.
#For multinomial log reg, use AIC, residual deviance, the Likelihood Ratio Test from ANOVA, and 3 PseudoR^2 values.

str(model) #Tells what attributes I can pull from summary.
summary(model)$"AIC"
summary(model)$"deviance"
car::Anova(model)
DescTools::PseudoR2(model) #McFadden's
DescTools:::PseudoR2(model, which = "CoxSnell") 
DescTools::PseudoR2(model, which = "Nagelkerke")




#C) COMPARE

#1) Build a second model without the Race category.
model2 <- multinom(Description ~ Age + Sex + Hour + Month, data = train)
summary(model2)

#2) Goodness of Fit.
summary(model2)$"AIC"
summary(model2)$"deviance"
DescTools::PseudoR2(model2) #McFadden's
DescTools:::PseudoR2(model2, which = "CoxSnell") 
DescTools::PseudoR2(model2, which = "Nagelkerke")

#3) Compare to first model.
export_summs(model, model2, exp = T)



#D) ANALYZE AND INTERPRET



#E) SOURCES

#Logistic Regression in R: https://rpubs.com/rslbliss/r_logistic_ws
#Multinomial Logistic Regression: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
#Using logistic regression: http://rstudio-pubs-static.s3.amazonaws.com/74431_8cbd662559f6451f9cd411545f28107f.html
#Interpretation of AIC: https://stats.stackexchange.com/questions/187373/interpretation-of-aic-value