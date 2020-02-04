library(lubridate)
library(tidyverse)
library(data.table)
library(ggrepel)
library(bit64)

incidents <- fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/ggmap/i2Sample.csv', stringsAsFactors = FALSE)
str(incidents) 
attach(incidents)

custGrey = "#A9A9A9"

#Make time columns
incidents$ymd <- mdy_hms(Event.Clearance.Date)
incidents$month <- month(incidents$ymd) 
incidents$year <- year(incidents$ymd)
incidents$wday <- wday(incidents$ymd, label=TRUE)
incidents$hour <- hour(incidents$ymd)

#Subset for year 2017
i2 <- incidents[year>=2017, ]

i2[complete.cases(i2),]

attach(i2)
head(i2)

groupSummaries <- i2 %>%
  group_by(month, Event.Clearance.SubGroup) %>%
  summarise(N=length(Event.Clearance.SubGroup))

ggplot(groupSummaries, aes(x=month, y=N, color=Event.Clearance.SubGroup)) +
  geom_line() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_blank()) +
  scale_x_discrete(name="Month", limits = c(3,6,9,12))

groupSummariesF <- groupSummaries %>%
  group_by(Event.Clearance.SubGroup) %>%
  filter(N>=95) %>%
  ungroup()

#First method involved layering two datasets - the first full df in gray and the second subset in color. 
ggplot() +
  geom_line(aes(month, N, group = Event.Clearance.SubGroup), data = groupSummaries, colour = alpha("grey", 0.7)) +
  geom_line(aes(month, N, group = Event.Clearance.SubGroup, colour = Event.Clearance.SubGroup), data = groupSummariesF) +  
  scale_x_discrete(name ="Month", limits=c(3,6,9,12)) +
  theme(
    legend.position="bottom",
    legend.text=element_text(size=7),
    legend.title = element_blank())

install.packages("gghighlight")
#Second method highlights a subset without layering dfs. 
library(gghighlight)  
ggplot(groupSummaries, aes(month, N, color=Event.Clearance.SubGroup)) +
  geom_line() +
  gghighlight(max(N)>95, label_key = Event.Clearance.SubGroup) +
  scale_x_discrete("Month", limits = c(3,6,9,12))