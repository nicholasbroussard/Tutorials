---
  title: "Analyzing Austin's ECAD Energy Use"
subtitle: "Multiple Linear Regression in R"
author: "Nick Broussard"
date: "`r format(Sys.time(), '%d %B %Y')`"
output: 
  html_document:
  code_folding: hide
---
  
  ```{r setup, include=FALSE}

library(tidyverse)
library(GGally) #Correlation matrix
library(inspectdf) #inspect_na(), inspect_num()
library(kableExtra)
library(scales)
library(data.table)
library(caTools) #Data splitting
library(summarytools) #descr() - for summaries of numerical variables
library(mice) #mice()
library(Metrics) #rmse()
library(caret) #RMSE (root mean squared error), R2 (r squared)
library(jtools) #summ() for nice model summary tables 
library(huxtable) #export_summs
library(interactions) #cat_plot(), interact_plot()
library(olsrr) #Cook's distance
library(outliers) #outlier(), scores()

knitr::opts_chunk$set(echo = T, 
                      warning = F, 
                      message = F, 
                      error = F,
                      results = "asis",
                      comment = NA,
                      prompt = F,
                      print = "render",
                      scipen = 999)

st_options(bootstrap.css     = FALSE,       
           plain.ascii       = FALSE,       
           style             = "rmarkdown", 
           dfSummary.silent  = TRUE,        
           footnote          = NA,          
           subtitle.emphasis = FALSE)       

```

```{r, echo = FALSE, include=F}
st_css()
```

## Hello!!

Welcome to this tutorial on multiple linear regression!
  
  Linear regression is statistical tool used to test hypothesis about the influence of certain variables (predictors) on another (outcome). The null hypothesis states that variables _do not_ affect on each other, wheareas the alternate hypothesis states that the variables _do_ affect each other. 

$\ h_{0}$: Variables do not affect each other
$\ h_{a}$: Variables do affect each other

If we get significant model predictors then we'll reject the null hypothesis. Otherwise, we fail to reject the null hypothesis. Significance means that a given effect is __super rare__ - _so rare_ in fact that its highly unlikely we'd see that effect if it wasn't real. 

Data comes from the City of Austin's [Open Data Portal](https://data.austintexas.gov/). We're looking at Energy Conservation Audit and Disclosure [(ECAD)](https://austinenergy.com/ae/energy-efficiency/ecad-ordinance/for-multifamily-properties) data [(found here)]([here](https://data.austintexas.gov/Utilities-and-City-Services/2009-2013-ECAD-Residential-Energy-Audit-Data/me4f-48mc). I want to examine how average monthly energy use in kWh is effected by a multifamily residential building's year of construction, size in sft, attic insulation (R-value), and number of floors.  

## Expore and Clean

First, read in the dataset. 

```{r}
#Download from my GitHub folder.
df <- fread("https://raw.githubusercontent.com/nicholasbroussard/tutorials/master/linear.multiple_ecad_austin.csv", stringsAsFactors = F)
```

Second, select the variables of interest and rename.  

```{r}
df <- df %>%
  select("Average Monthly kWh", "ECAD Year Built", "Average Apt Size", "ECAD R Value", "ECAD Number of Floors") %>%
  #Let's the variables easier-to-read names.
  setnames(old = c("Average Monthly kWh", "ECAD Year Built", "Average Apt Size", "ECAD R Value", "ECAD Number of Floors"),
           new = c("Energy",  "YearBuilt", "Size", "RValue", "Floors"))
```

Third, check variable types...

```{r}
str(df)
```

...and convert as needed.

```{r}
df$YearBuilt <- as.numeric(as.character(df$YearBuilt))
df$Floors <- as.factor(as.numeric(df$Floors))
```

Fourth, inspect the categorical.

```{r}
df %>%
  inspect_cat() %>%
  show_plot()
```

Based on this distribution...

```{r}
ggplot(df, aes(x = Floors)) +
  geom_bar(color = "midnightblue", fill = "cadetblue", alpha = .7)
```

...restrict.

```{r}
df <- df %>%
  filter(Floors == "1" | Floors == "2" | Floors == "3" |  is.na(Floors))
```

Fifth, inspect numerics by calling the 5-number-summary...

```{r numerics}
df %>%
  descr(transpose = T,
        stats = "fivenum") %>%
  print(headings = F,
        method = "render") 
```

...and histogram distributions for uniformity...

```{r}
a <- ggplot(df, aes(Energy)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30) 

b <- ggplot(df, aes(YearBuilt)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 40) +
  scale_x_continuous(limits = c(1887, 2019)) 

c <- ggplot(df, aes(Size)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30)

d <- ggplot(df, aes(RValue)) +
  geom_histogram(color = "midnightblue", fill = "cadetblue", alpha = .7, bins = 30)

gridExtra::grid.arrange(a,b,c,d)
```

...and for outliers (any value > 1.5*IQR)...

```{r}
par(mfrow = c(2,2))
energy_outliers <- boxplot(df$Energy)$out
yearbuilt_outliers <- boxplot(df$YearBuilt)$out
size_outliers <- boxplot(df$Size)$out
rvalue_outliers <- boxplot(df$RValue)$out
```

...and reclassify outliers as NAs.

```{r}
df[which(df$Energy %in% energy_outliers),] <- NA
df[which(df$YearBuilt %in% yearbuilt_outliers),] <- NA
df[which(df$Size %in% size_outliers),] <- NA
df[which(df$RValue %in% rvalue_outliers),] <- NA
```

Sixth, check missingness. 

```{r}
df %>%
  inspect_na() %>%
  show_plot()
```

Impute all values, even if they're below the 5% threshold.

```{r}
set.seed(101)
imp <- mice(df, m = 1, maxit = 1, print = F)
df <- complete(imp)
```

Seventh, check correlation.

```{r correlation}
df %>%
  ggcorr(label = TRUE, 
         label_alpha = TRUE,
         hjust=.9,
         size=3.5,
         layout.exp=3,
         method = c("pairwise.complete.obs", "spearman")) +
  labs(title="Correlation Matrix")
```

No correlation is above 70%, so no multicolinearity. Read more [here](https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/).

Eighth, and finally, analyze variance. 

```{r}
df %>%
  summarise(Energy = var(Energy), YearBuilt = var(YearBuilt), Size = var(Size), RValue = var(RValue))
```

Lot's of variance, which violates the assumption of homoscedasticity. Read more [here](http://davidmlane.com/hyperstat/A121947.html). 

Scale variables in the model.


## Model

First, split the data. 

```{r}
set.seed(101)
sample <- sample.split(df, SplitRatio = .7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)
```

Some folks disagree with data-splitting. Check out [this Quora thread](https://www.quora.com/In-machine-learning-what-s-the-purpose-of-splitting-data-up-into-test-sets-and-training-sets) for more info.

Second, run and interpret the regression. 

```{r}
set.seed(101)
model <- lm(Energy ~ ., data = train)
summ(model,
     confint = T,
     scale = T, 
     vifs = T) #Variable Inflation Factors
```

Model: $\ y=547+26*YearBuilt+95*Size-3.5*RValue+5*2ndFloor+44*3rdFloor$.

Interpretation:
  
  * Dependent Variable
+ 1st floor unit energy use is $\ 547kWh \over month$ ($\ y=547$). 
* Independent Variables
+ 1 year decrease in age is associated with increased energy use of $\ 26kWh \over month$. Specifically, a 1st floor unit that's 1 year newer than another unit is expected to use $\ 573kWh \over month$ ($\ y=547+26*1$). 
  + 1 sft increase in size is associated with increased energy use of $\ 95kWh \over month$ after controlling for all other variables. Specifically, a 1st floor unit that's 1 sft bigger than the average unit is expected to use $\ 642kWh \over month$  ($\ y=547+95*1$).
+ 1 unit increase in R-value thickness is associated with decreased energy use of $\ 3.5kWh \over month$ after controlling for all other variables. Specifically, a 1st floor unit that has 1-unit thicker R-value than the average unit would use $\  550.5kWh \over month$ ($\ y=547+3.5*1$).
+ Going from the 1st floor to the second floor is associated with increased energy use of $\ 5kWh \over month$ after controlling for all other variables. Specifically, a unit that's on the 2nd floor but otherwise has the same characteristics as the average unit would use $\ 552kWh \over month$ ($\ y=547+5*1$).
  + Going from the 1st floor to the 3rd floor is associated with increased energy use of $\ 44kWh \over month$ after controlling for all other variables. Specifically, a unit that's on the 3rd floor but otherwise has the same characteristics as the average unit would use $\ 591kWh \over month$ ($\ y=547+44*1$).

Third, conduct graphical analysis. Analyze the coefficient effects...

```{r}
plot_summs(model, plot.distributions = T, scale = T)
```

...then visualize the relationship between each predictor and the dependent variable...

```{r}
aa <- effect_plot(model, pred = YearBuilt, interval = T, plot.points = T, rug = T)
bb <- effect_plot(model, pred = Size, interval = T, plot.points = T, rug = T)
cc <- effect_plot(model, pred = RValue, interval = T, plot.points = T, rug = T)
dd <- effect_plot(model, pred = Floors, interval = T, cat.geom = "line") #Categorical predictor
gridExtra::grid.arrange(aa,bb,cc,dd)
```

...and look at influential values.

```{r}
ols_plot_cooksd_chart(model) 
```

Examine influential rows individually and remove as needed.

```{r}
cooksd <- cooks.distance(model)
influential <- as.numeric(names(cooksd))[(cooksd > 4*mean(cooksd, na.rm = T))]
head(train[influential, ], 10)
```

Fourth, pull in fitted and residual values...

```{r}
train$Residuals <- model$residuals
train$Fitted <- model$fitted.values
```

...and query the model.

* What's predicted energy use for homes built in 1981? `r train %>% group_by(YearBuilt) %>% filter(YearBuilt==1981) %>% summarise(mean(Fitted))` kWh/month.
* What's the predicted energy use of a 2nd floor unit larger than 600 sft? `r train %>% filter(Floors=="2"&Size>600) %>% summarize(mean(Fitted))` kWh/month

_But_... answers are only as reliable as the model. 

Fifth, evaluate goodness of fit.

Start with the baseline metrics...

* R Squared: `r percent(summary(model)$r.squared)` 
* Adjusted R Squared: `r  percent(summary(model)$adj.r.squared) `
* Root Mean Squared Error (RMSE): `r round(rmse(train$Energy, train$Fitted), digits=0) ` 

...then look at residuals. Check for normality...

```{r}
ggplot(train, aes(x = model$residuals)) +
  geom_histogram(bins = 35, color = "black", fill = "cadetblue", alpha = .7) +
  theme_bw()
```

...and the scatter of residuals against predictions.

```{r}
ggplot(train, aes(x=Fitted, y=Residuals)) +
  geom_point() +
  geom_smooth(method = "lm")
```

The distribution shows (1) centering on 0, and (2) a ["degree of scattering [that] is the same for all fitted values"](https://statisticsbyjim.com/regression/check-residual-plots-regression-analysis/). Therefore there's no indication of autocorrelation.

Sixth, and last, check the model's performance with test data. Compare RMSEs.

```{r}
test$Fitted <- predict(model, newdata = test)
df %>%
  summarise("Train Set RMSE" = round(rmse(train$Energy, train$Fitted), digits = 2), "Test Set RMSE" = round(rmse(test$Energy, test$Fitted), digits=2))
```

The train RMSE is lower the test RMSE. No indication of overfitting.


## Compare

Run an interactive model to compare against the first model.

First, graphically analyze the effects of predictor varibales on each other. Here, we focus on Size.

```{r}
a <- interact_plot(model, pred = Size, modx = YearBuilt, plot.points = T, interval = T)
b <- interact_plot(model, pred = Size, modx = RValue, plot.points = T, interval = T)
c <- interact_plot(model, pred = Size, modx = Floors, plot.points = T, interval = T) #Categorical variable.
d <- interact_plot(model, pred = Size, modx = RValue, mod2 = Floors) #3-way interaction.

gridExtra::grid.arrange(a,b,c,d)
```

["Parallel lines indicate that there is no interaction effect while different slopes suggest that one might be present."](https://statisticsbyjim.com/regression/interaction-effects/)  

Second, run and interpret a model to test the graphical output.

```{r}
set.seed(101)
model2 <- lm(Energy ~ YearBuilt + Size + RValue + Floors + Size:YearBuilt, data = train)
summ(model2, scale = T, confint = T, digits = 2, vifs = T)
```

Model: $\ y = 554 + 29*YearBuilt + 96*Size - 4*RValue + 3.5*2ndFloor + 44*3rdFloor - 15.87*Size:DuctLeakage$
  
  The coefficients of the individual predictors are called "main effects." For interaction models, ["a significant interaction term means uncertainty about the relative importance of main effects.](https://stattrek.com/multiple-regression/interaction.aspx) 

Interpretation:

* Dependent Variable
  + The average energy use is $\ 554kWh \over month$ ($\ y=554 $).    
* Interaction
variables.
  + The effect of YearBuilt on Energy decreases by $\ 15.87kWh \over month $  for every 1-unit increase in RValue.
  + Likewise, the effect of YearBuilt on Energy decreases by $\ 15.87kWh \over month $ for every 1 additional year. 
  
Third, compare the new model to the original by coefficients...

```{r}
export_summs(model, model2, 
             scale = T, 
             error_format = "[{conf.low},{conf.high}]") #Include confidence intervals for each variable.
```

...and graphically.

```{r}
plot_summs(model, model2, scale = T, plot.distributions = T, model.names = c("Without Interation", "With Interaction"))
```

Overall, the models look pretty similar. This further solidifies that the variables are not interacting. 

Stick with the first model, even with the low goodness of fit measures. For now.



## Sources

I heavily relied on the following online blogs and tutorials:

* Tools for Summarizing and Vizualising Regression Models: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
* Removing Outliers: https://rpubs.com/Mentors_Ubiqum/removing_outliers
* Cook's D: https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html
* Outlier Treatment: http://r-statistics.co/Outlier-Treatment-With-R.html
* Visualizing Residuals: https://drsimonj.svbtle.com/visualising-residuals 
* Visualizing Predictions: https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html 
* Interactions with Continuous Predictors: https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html
* Interactions with Categorical Predictors: https://cran.r-project.org/web/packages/interactions/vignettes/categorical.html
* DataCamp's LR Tutorial: https://www.datacamp.com/community/tutorials/linear-regression-R
* Another Tutorial: http://r-statistics.co/Linear-Regression.html
* Interpreting Interactions in Regression: https://www.theanalysisfactor.com/interpreting-interactions-in-regression/