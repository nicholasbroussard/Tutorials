library(tidyverse)
library(inspectdf) #inspect_na(), inspect_num()
library(summarytools)
library(scales) #percent
library(data.table)
library(jtools) #summ(), export_summs()
library(interactions) #cat_plot(), interact_plot()

#A) EDA

#1) Read in the data.

df <- fread("https://raw.githubusercontent.com/nicholasbroussard/tutorials/master/Austin%20Short%20Term%20Rentals%20(Binomial%20Logistic)/austinrentals_logistic.binomial.csv",
            stringsAsFactors = F)
#https://data.austintexas.gov/City-Government/Short-Term-Rental-Legal-Outcomes/xfmy-m22z

#2) Select variables.

df <- df %>%
  select(-CASENUMBER, -CITATIONTYPE, -CITATIONNUMBER, -DEFICIENCYCODE, -FEEAMOUNT)

#3) Look at the df's structure and convert vars as needed.

glimpse(df)
library(lubridate)
df$VIOLATIONOPENDATE <- mdy_hms(df$VIOLATIONOPENDATE)
df$LEGALOUTCOMEDATE <- mdy_hms(df$LEGALOUTCOMEDATE)
#Find the number of months it takes between opening a violation and legally closing it.
df <- df %>% 
  mutate(MONTHSVIOLATIONOPEN = interval(VIOLATIONOPENDATE, LEGALOUTCOMEDATE)%/%months(1)) %>%
  select(-VIOLATIONOPENDATE, -LEGALOUTCOMEDATE)
#Consolidate types of deficiencies.
df$DEFICIENCYTEXT <- as.character(df$DEFICIENCYTEXT)
df <- df %>%
  mutate(DEFICIENCYTEXT = case_when(str_detect(DEFICIENCYTEXT, "must obtain a license") | str_detect(DEFICIENCYTEXT, "is not licensed") ~ "Licensing Violation",
                                    str_detect(DEFICIENCYTEXT, "may not advertise or promote") ~ "Incomplete Registration Violation",
                                    str_detect(DEFICIENCYTEXT, "may not be used by more than") | str_detect(DEFICIENCYTEXT, "may not include the rental of less than") | str_detect(DEFICIENCYTEXT, "16 people") ~ "Occupancy Limit Violation",
                                    str_detect(DEFICIENCYTEXT, "The advertisement required a three night minimum stay") ~ "Length of Stay Violation"))
#Build binary outcome variable.
df <- df %>%
  mutate(LEGALOUTCOME = ifelse(LEGALOUTCOME=="Liable" | LEGALOUTCOME=="Closed due to Judicial / Admin Action", 0, 1))
#0 is "not penalized", 1 is penalized. I have to code the outcome var as 0/1 for the glm().
df <- df %>%
  select(LEGALOUTCOME, DEFICIENCYTEXT, OUTSTANDINGFEE, MONTHSVIOLATIONOPEN)

#4) Categoricals.

df %>%
  inspect_cat() %>%
  show_plot()

ggplot(df, aes(x = DEFICIENCYTEXT)) +
  geom_bar(color = "midnightblue", fill = "cadetblue", alpha = .7) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggplot(df, aes(LEGALOUTCOME)) +
  geom_bar(color = "midnightblue", fill = "cadetblue", alpha = .7) 

df <- df %>%
  filter(DEFICIENCYTEXT != "Length of Stay Violation")

#5) Numerics. Call the 5-number-summary, histograms, and boxplots for outliers.

df %>%
  summarytools::descr(transpose = T,
        stats = "fivenum")

ggplot(df, aes(MONTHSVIOLATIONOPEN)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30) 
ggplot(df, aes(OUTSTANDINGFEE)) +
  geom_bar(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30) 

df$OUTSTANDINGFEE[df$OUTSTANDINGFEE<=0] <- NA

monthsopen_outliers <- boxplot(df$MONTHSVIOLATIONOPEN)$out
fee_outliers <- boxplot(df$OUTSTANDINGFEE)$out

#6) Missingness. Impute as needed.

df %>%
  inspectdf::inspect_na() %>%
  show_plot()

library(mice)
set.seed(101)
imp <- mice(df, m = 1, maxit = 1, print = F)
df <- complete(imp)
df$OUTSTANDINGFEE <- as.numeric(df$OUTSTANDINGFEE)

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

#1) Split data.

library(caTools)
set.seed(101) #The seed number does not matter. Any number will do.
sample <- sample.split(df, SplitRatio = .6)
test <- subset(df, sample == TRUE)
train <- subset(df, sample == FALSE)

#2) Build and interpret the model.

set.seed(101)
model <- glm(LEGALOUTCOME ~ ., data = train, family = binomial(link="logit")) 
summ(model, exp = T, vifs = T, digits = 4)

#y=907+1.67*LicensingViolation+10.98*OccupancyViolation+0.98*OutstandingFee+0.81*MonthsOpen

#For the outcome variable, 0 is not penalized and 1 is penalized.
#Reference category for DEFICIENCYTEXT is RegistrationViolation.

#A licensing violation is 66% more likely to result in a penalty than an incomplete registration violation controlling for all other variables. Not staistically significant.
#An occupancy limit violation is 11x (1,098%) more likely to result in a penalty than an incomplete registration violation.
#For every $1 increase in outstanding fee the likelihood of penalty decreases by 1% (1-.989).
#For every month that the violation is open the likelihood of penalty decreases by 19% (1-.81).

#3) Graphical analysis. Analyze coefficients the relationship between predictors and the outcome.

plot_summs(model, plot.distributions = TRUE, exp = T)

effect_plot(model, pred = DEFICIENCYTEXT, interval = T, cat.geom = "line")
effect_plot(model, pred = OUTSTANDINGFEE, interval = T, rug = T)
effect_plot(model, pred = MONTHSVIOLATIONOPEN, interval = T, rug = T)

#4) Analyze residuals.

train$residuals <- model$residuals
train$fitted <- model$fitted.values
arm::binnedplot(fitted(model), residuals(model, type="response"))

#5) Analyze predicted (fitted) values by querying the model.

#What are the odds of penalty for cases open two months? 
train %>% filter(MONTHSVIOLATIONOPEN == 2) %>% summarise(percent(mean(fitted)))
#What are the odds of penalty for any given licensing violation? 
train %>% filter(DEFICIENCYTEXT=="Licensing Violation") %>% summarise(percent(mean(fitted)))

#6) Evaluate goodness of fit.

anova <- stats::anova(model, test = "Chisq")
anova$"Resid. Dev" 
model$null.deviance
summary(model)$aic
table(train$LEGALOUTCOME, train$fitted > 0.5) #confusion matrix
library(ROCR)
ROCRpred <- prediction(train$fitted, train$LEGALOUTCOME)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = T) #ROC Curve
DescTools::PseudoR2(model)
DescTools::PseudoR2(model, which = "CoxSnell")
DescTools::PseudoR2(model, which = "Nagelkerke")

#7) Run predictions on test data and compare with train's confusion matrix.  

test$fitted <- predict(model, newdata = test, type = "response")
table(test$LEGALOUTCOME, test$fitted > .5)



#c) COMPARE

#Model with fewer predictors.

set.seed(101)
model2 <- glm(LEGALOUTCOME ~ OUTSTANDINGFEE + MONTHSVIOLATIONOPEN, data = train, family = binomial(link = "logit"))
summ(model2, exp = T, digits = 4, vifs = T)

#Goodness of fit.

DescTools::PseudoR2(model2) #McFadden's
DescTools:::PseudoR2(model2, which = "CoxSnell") 
DescTools::PseudoR2(model2, which = "Nagelkerke")

train$DEFICIENCYTEXT <- relevel(as.factor(train$DEFICIENCYTEXT), ref = "Occupancy Limit Violation")
set.seed(101)
model3 <- glm(LEGALOUTCOME ~ DEFICIENCYTEXT + OUTSTANDINGFEE + MONTHSVIOLATIONOPEN, 
             data = train, 
             family = binomial(link="logit")) 
summ(model3, exp = T, vifs = T, digits = 4)


#Compare models side-by-side.

export_summs(model, model2, model3, 
             scale = T, 
             error_format = "[{conf.low},{conf.high}]") #Include confidence intervals for each variable.
plot_summs(model, model2, exp = T, partial.distributions = T)



#D) FINDINGS

#If you can drag out the case, you pay less.
#If your fee is higher, youre less likely to be penalized. 

ggplot(train, aes(x=MONTHSVIOLATIONOPEN, y = LEGALOUTCOME)) +
  geom_point() +
  stat_smooth(method = "glm", method.args=list(family="binomial"), se = F)

#Most likely, residential AirBNB violators are being punished while corporations delay and defelect and never pay fines. 
#If it's the big corporations AirBNBing complexes, not the residential customer, then why is the little guy being punished?
#Fines wouldn't matter to big corporations anyway, but they operate with impunity to local ordinances. 
#This encourages further problematic behavior, empowering corporations and businesses to flout local requirements.

#Money potentially lost in uncollected penalties.
df %>%
  filter(LEGALOUTCOME==0) %>%
  summarize(dollar(sum(OUTSTANDINGFEE)))

train <- train %>%
  mutate(FeeLevel = ifelse(OUTSTANDINGFEE<.5*max(OUTSTANDINGFEE), "Low", "High"),
         PendingPeriod = ifelse(MONTHSVIOLATIONOPEN<.5*max(MONTHSVIOLATIONOPEN), "Quick to Court","Dragged Out"))

#Likelihood of penalty for low fee v high fee.
train %>%
  filter()

#Likelihood of penalty if you drag it out. 


#E) SOURCES
#https://datascienceplus.com/perform-logistic-regression-in-r/
#https://www.datacamp.com/community/tutorials/logistic-regression-R
#https://towardsdatascience.com/simply-explained-logistic-regression-with-example-in-r-b919acb1d6b3
#https://www.theanalysisfactor.com/r-glm-model-fit/
#http://r-statistics.co/Logistic-Regression-With-R.html
#https://webfocusinfocenter.informationbuilders.com/wfappent/TLs/TL_rstat/source/LogisticRegression43.htm
#AIC and Model Parsimony: https://www.r-bloggers.com/how-do-i-interpret-the-aic/
#https://www.analyticsvidhya.com/blog/2015/11/beginners-guide-on-logistic-regression-in-r/
#effect_plot in jtools: https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html 
#Beginner's Guide to Logistic Regression: https://www.analyticsvidhya.com/blog/2015/11/beginners-guide-on-logistic-regression-in-r/