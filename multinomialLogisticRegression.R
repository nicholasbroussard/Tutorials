library(tidyverse)
library(foreign)
library(nnet) #contains multinom()

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

#Desciptives
# See frequency of ses by program:
with(ml, table(ses, prog))
# or...
table(ml$ses, ml$prog)

#Get sd and mean:
with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))
#or...
ml %>%
  group_by(prog) %>%
  summarise(M = mean(write), SD = sd(write))

#set the reference level of the predictor variable to academic, the other two being general and vocational.
ml$prog2 <- relevel(ml$prog, ref = "academic")

test <- multinom(prog2 ~ ses + write, data = ml)
summary(test)

#Get the odds (relative risk ratios) by exponentiating the coef 
odds <- exp(summary(test)$coefficients)

#A one unit increase in writing score is associated with a 6% decrease in the odds of choosing the general program instead of the academic program,
#and a .11% decrease in the odds of choosing the general program instead of the academic program. 

#A move from ses low to high is associated with a nearly 70% decrease in the odds of choosing the general over the academic program, and a 63% decrese in the odds
#of choosing the vocational over the academic program. 

#A move from ses low to middle is associated with a 41% decrease in the odds of choosing the general program over the academic program, and a 33% increase in the 
#odds of choosing the vocational program over the academic program. 


# Sources
# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/