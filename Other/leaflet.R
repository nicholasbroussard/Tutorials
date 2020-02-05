library(tidyverse)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)

#Download data from https://data.austintexas.gov/Building-and-Development/Issued-Tree-Permits/ac2h-ha3r
#Upload.
trees <- read.csv("C:\\Users\\Nbroussard\\OneDrive\\Nick\\Data\\Tutorials\\Maps\\Issued_Tree_Permits.csv")
#Recode empty cells as NA.
trees[trees == ""] <- NA  
#Filter out the NAs in PERMIT_CLASS, which is my color variable. 
trees <- trees %>%
  filter(!is.na(PERMIT_CLASS))
#Take a fraction of the original df, since it's huge (overplotted).
set.seed(1)
trees <- sample_frac(trees, size=.01)
#Use attach() to call variables directly, without needing to do trees$variable. 
attach(trees)

#Build a color palette for color-dividing the icons based on the levels of PERMIT_CLASS: Public, Residential, Commercial.
pal <- colorFactor(c("navy", "green", "red"), 
                   domain = unique(trees$PERMIT_CLASS)) 

#Build the map.
tree_map <- leaflet(trees) %>% #Call the target df
  addTiles() %>% 
  #addCircleMarkers is just one of many options, to include addMarkers, addAwesomeMarkers.
  addCircleMarkers(
    #PLug in the x and y coordinates, similar to geom_point. 
    lng = ~LONGITUDE,  
    lat = ~LATITUDE, 
    #Color-divide the markers according to a categorical variable, using the color palette we built above.
    color = ~pal(PERMIT_CLASS),
    #Labels are info boxes that showup just from hovering over each marker. 
    label = ~paste0("Address: ", PERMIT_ADDRESS),
    labelOptions = labelOptions(textsize = "15px", #Change the size of teh text 
                                direction = "top"), #Change where the box shows up. Can also be "bottom", "right", "left"
    stroke = FALSE,
    fillOpacity = .5,
    #Popups show up when you click on markers.
    popup = ~paste0("City: ", ISSUED_DATE, #Break the text into two lines using html's <br/>
                    "<br/>County: ", EXPIRES_DATE)) %>%
  #Center the map when it opens on the centerpoint of your markers, which in this case is ATX.
  setView(lng = -97.755497, 
          lat = 30.265803, 
          zoom = 12) %>% #Zoom in or out. The bigger the number, the closer the zoom.
  addLegend("topright", pal = pal, values = ~PERMIT_CLASS, title = htmltools::HTML("Permit <br/>Class"), opacity = 1) 
tree_map



  


#Sources
https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/leaflet-r/
https://rpubs.com/bhaskarvk/leaflet-labels
https://www.rdocumentation.org/packages/leafletCN/versions/0.2.2/topics/addTitle
  