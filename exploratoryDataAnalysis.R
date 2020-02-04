library(tidyverse)
library(readr)

#https://www.littlemissdata.com/blog/simple-eda

#Download the data set
df= read_csv('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv', col_names = TRUE)

head(df)
dim(df)
glimpse(df)
summary(df)

#Plussed-up version of summary().
library(skimr)
skim(df)

#Viz the missing data broken down by category (continuous or categorical)
install.packages("devtools")
library(devtools)
devtools::install_github("ropensci/visdat")
library(visdat)
vis_miss(df)
vis_dat(df)

# Get a massive html document that inlcudes correlation analysis and priniciple component analysis. 
install.packages("DataExplorer")
library(DataExplorer)
DataExplorer::create_report(df)

devtools::install_github("alastairrushworth/inspectdf")
library(inspectdf)


#https://www.littlemissdata.com/blog/inspectdf
#https://alastairrushworth.github.io/Exploring-categorical-data-with-inspectdf/

#Summary of count and percent by var type
starwars %>%
  inspect_types()

#Get a count and prct of the most commonly occuring level per variable.
star_cat <- starwars %>%
  inspect_cat()

#Find breakdown of all the levels in a variable.
star_cat$levels$eye_color

#Viz the freq all categorical variables. Gray bars are NAs.
star_cat %>%
  show_plot()
#Combine all the entries that happen only once into a single bar called High Cardinality.
star_cat %>%
  show_plot(
    high_cardinality = 1,
    col_palette = 2) #Add a cool 80s themed color palette.

#All the inspectdf functions
inspect_types() %>% #summary of column types
  show_plot()
inspect_na() %>% #columnwise prevalence of missing values
  show_plot()
inspect_cor() %>% #correlation coefficients of numeric columns
  show_plot()
inspect_imb() %>% #feature imbalance of categorical columns
  show_plot()
inspect_num() %>% #summaries of numeric columns
  show_plot()
inspect_cat() %>% #summaries of categorical columns
  show_plot()