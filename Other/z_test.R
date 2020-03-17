library(tidyverse)
library(data.table)

# http://www.sthda.com/english/wiki/one-proportion-z-test-in-r
# https://www.ztable.net/

#A 1-Prop Z Test compares an observed proportion to a theoretical proportion when there are only two categories.
#A 2-Prop Z Test compares 
#Use a sample size large enough that n*p0 >= 5. For example, if p0=0.1, then n>=50.
#Hypothesis Testing:
##H0: p.observed = p.expected
##HA: p.observed != p.expected
#prop.test() is the best function. 
##correct = TRUE: With continuity correction. correct = FALSE: Without continuity correction.
##alternative = "less": Test if proportion is less than p. alternative = "greater": Test if proportion is greater than p. 


#Example 1
#In a population (n = 160) of mice 50/50 male/female (p = 0.5 = 50%), some developed cancer: 95 males and 65 females.
#Does cancer affect males more than females?
res <- prop.test(x=95, n=160, p=.5, correct = FALSE)
res
#p-value is < 0.05: Proportion of males with cancer (x) is significantly different from p. Reject H0.