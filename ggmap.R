
#The packagage documentation... so many arguments!! https://cran.r-project.org/web/packages/ggmap/ggmap.pdf
#This tutorial is from https://www.littlemissdata.com/blog/maps
#Cheatsheet: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf


library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(plotly)

incidents <- read.csv('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/ggmap/i2Sample.csv', stringsAsFactors = FALSE)
n <- read.csv('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/ggmap/n.csv', stringsAsFactors = FALSE)

attach(incidents)
attach(n)

col1 = "#011f4b"
col2 = "#6497b1"
col3 = "#b3cde0"
col4 = "#CC0000"


#Add year to incidents df. 
incidents$ymd <- mdy_hms(incidents$Event.Clearance.Date)
incidents$year <- year(incidents$ymd)

#Subset for 2017 and 2018.
i2 <- incidents %>%
  filter(year >= 2017 & year <= 2018)

#Filter for rows that are complete.
i2[complete.cases(i2),] 

#Create a display label for n df.
n$label <- paste(n$Rank, n$Location, sep="-")

#Create a new variable thats the factor version of location. 
n$Neighborhood <- as.factor(n$Location)

#Install and load ggmap
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
library("ggmap")
ggmap::register_google(key="AIzaSyD_hm6rjYTbJiGKQlxw6p09aK5fWj1SnUE")

#Base map of Seattle
p <- ggmap(get_googlemap(center = c(lon = -122.335167, lat = 47.608013), #Get lon/lat with these instructions: https://support.google.com/maps/answer/18539?co=GENIE.Platform%3DDesktop&hl=en
                         zoom = 11, #0 to 21, with 0 the furthest out and 21 the furthest in
                         scale = 2, #1,2... changes the pixelation and size of labels
                         maptype ="terrain", #Can also be "roadmap", "hybrid", or "satellite"
                         color = "color")) #Also "black-and-white"

#(Also, not for this tutorial, but a base map of Texas... centered just east of Eden, TX)
tx <- ggmap(get_googlemap(center=c(lon = -100.176662, lat = 31.343133),
                         zoom = 6,
                         scale=2,
                         maptype = "terrain",
                         color = "color")) +
  theme_void()
#(And a map of Austin)
atx <- ggmap(get_googlemap(center=c(lon = -97.749412, lat = 30.263280),
                          zoom = 12,
                          scale=2,
                          maptype = "terrain",
                          color = "color")) +
  theme_void()

#(..and Rochester)
rny <- ggmap(get_googlemap(center = c(lon = -77.614411, lat = 43.155466),
                           zoom = 12,
                           scale = 2,
                           maptype = "terrain",
                           color="color"))

#Map 1: Incident occurrences color coded by group
p + geom_point(aes(x = Longitude, y = Latitude,  colour = Initial.Type.Group), data = i2, size = 0.5) + 
  theme(legend.position="bottom")


#Map 2: Incident occurrences using one color with transparency
p +   geom_point(aes(x = Longitude, y = Latitude), colour = col1, data = i2, alpha=0.25, size = 0.5) + 
  theme(legend.position="none")

#Map 3: Incident occurrences  + layer of "most dangerous neighborhood" location markers
a <- p + geom_point(data = i2, aes(x = Longitude, y = Latitude,  colour = Initial.Type.Group), color=col1, size = 0.5, alpha=.25) + 
  geom_point(data=n, aes(x=x, y=y, shape=Neighborhood, stroke = 2), color=col4, size=3) +
  scale_shape_manual(values = 1:nlevels(n$Neighborhood)) +
  theme_void()
#Make it interactive
ggplotly(a)

#Map 4: Incident occurrences + layer of "most dangerous neighbourhood" with labels (vs shapes)
p + geom_point(data = i2, aes(x = Longitude, y = Latitude,  colour = Initial.Type.Group), color=col1, size = 0.5, alpha=.25) + 
  theme(legend.position = "bottom") + 
  geom_point(data=n, aes(x=x, y=y, stroke = 2), color=col4, size=2.5) +
  geom_label_repel(data = n, aes(x=x, y=y, label=label), size=4, box.padding = .2, point.padding = .3, segment.colour = "grey50")

#Map 5: Re-do above plot to only show the most dangerous crimes
unique(i2$Event.Clearance.Group) #FInd unique var level names
i2Dangerous <- i2 %>%
  filter(Event.Clearance.Group %in% #Filter within variable Event.Clearance.Group for the following levels:
           c('TRESPASS', 'ASSAULTS', 'SUSPICIOUS CIRCUMSTANCES', 
                                      'BURGLARY', 'PROWLER', 'ASSAULTS', 'PROPERTY DAMAGE', 
                                      'ARREST', 'NARCOTICS COMPLAINTS','THREATS', 'HARASSMENT', 'WEAPONS CALLS',
                                      'PROSTITUTION' , 'ROBBERY', 'FAILURE TO REGISTER (SEX OFFENDER)', 'LEWD CONDUCT', 
                                      'HOMICIDE'))

attach(i2Dangerous)

p + geom_point(data=i2Dangerous, aes(x = Longitude, y = Latitude), colour = col1, alpha=0.25, size = 0.5) + 
  theme(legend.position="bottom")  +
  geom_point(data=n, aes(x = x, y = y, stroke = 2), colour=col4, size =1.5) + 
  geom_label_repel(data=n, aes(x, y, label = label), size = 3, box.padding = 0.2, point.padding = 0.3, segment.color = 'grey50') 

#Map 6:  Incident occurrence density plot + layer of "most dangerous neighborhood" with labels 
p + stat_density2d(data=i2Dangerous, aes(Longitude, Latitude, fill=..level.., alpha=0.25), geom="polygon", size=0.01, bins=30) +
  geom_point(data=n, aes(x = x, y = y, stroke = 2), colour=col4, size =1.5) + 
  geom_label_repel(data=n, aes(x, y, label = label), size = 3, box.padding = 0.2, point.padding = 0.3, segment.color = 'grey50') 

#Map 7: Incident occurrence density plot + density lines + layer of "most dangerous neighborhood" with labels 
p + stat_density2d(data=i2Dangerous, aes(Longitude, Latitude, fill=..level.., alpha=0.25), geom="polygon", size=0.01, bins=30) +
  geom_density2d(data = i2Dangerous, aes(Longitude, Latitude), size=.3) +
  geom_point(data=n, aes(x = x, y = y, stroke = 2), colour=col4, size =1.5) + 
  geom_label_repel(data=n, aes(x, y, label = label), size = 3, box.padding = 0.2, point.padding = 0.3, segment.color = 'grey50') 

#Map 8: Incident occurrence density plot + density lines + facet wrap for the highest occurring incident types
#Create a subset df fro 4 most commonly occuring groups of crime. 
i2Sub <- i2 %>%
  filter(Event.Clearance.Group %in% c('TRAFFIC RELATED CALLS', 'DISTURBANCES', 'SUSPICIOUS CIRCUMSTANCES', 'MOTOR VEHICLE COLLISION INVESTIGATION'))
attach(i2Sub)

p + stat_density2d(data = i2Sub, aes(Longitude, Latitude, fill=..level.., alpha=..level..), geom="polygon", size=.2, bins=30) +
  geom_density2d(data = i2Sub, aes(Longitude, Latitude), size=.3) +
  facet_wrap(~Event.Clearance.Group, nrow=2)

#Map 9: Change maptype and style
#9a) roadmap + black geom_point
ggmap(get_googlemap(center = c(lon = -122.335167, lat = 47.608013), #Get lon/lat with these instructions: https://support.google.com/maps/answer/18539?co=GENIE.Platform%3DDesktop&hl=en
                    zoom = 13, 
                    scale = 2,
                    maptype ="roadmap",
                    color = "color")) + 
  geom_point(data = i2, aes(Longitude, Latitude), color = "black", alpha = .3, size=.5) +
  theme(legend.position = "none") +
  theme_void()

#9b) satellite + red geom_point
ggmap(get_googlemap(center = c(lon = -122.335167, lat = 47.608013), #Get lon/lat with these instructions: https://support.google.com/maps/answer/18539?co=GENIE.Platform%3DDesktop&hl=en
                    zoom = 13, 
                    scale = 2,
                    maptype ="satellite",
                    color = "color")) + 
  geom_point(data = i2, aes(Longitude, Latitude), color = col4, alpha = .3, size=.5) +
  theme(legend.position = "none") +
  theme_void()

#Map 10: Change map provider and type
#10a) terrain-lines + blue density 
center <- c(lon = -122.335167, lat = 47.608013) #Center of Seattle
qmap(center, zoom = 11, source = "stamen", maptype = "terrain-lines") +
  stat_density2d(data = i2, aes(x = Longitude, y = Latitude, fill = ..level..), alpha = 0.25, size = 0.2, bins = 30, geom = "polygon")  + 
  geom_density2d(data = i2, aes(x = Longitude, y = Latitude), size = 0.3) + scale_fill_gradient(low = "light blue", high= "dark blue") 

#10b) watercolor + red geom_point
qmap(center, zoom = 11, source = "stamen", maptype = "watercolor") +
  geom_point(data=i2, aes(x = Longitude, y = Latitude), colour = col4, alpha=0.3, size = 0.5) + 
  theme(legend.position="none")