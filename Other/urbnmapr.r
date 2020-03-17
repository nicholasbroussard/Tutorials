#library(devtools)
#Urbnmapr <- devtools::install_github("UrbanInstitute/urbnmapr")
#Urbnthemes <- devtools::install_github("UI-Research/urbnthemes", build_vignettes = TRUE)
library(urbnmapr)
library(tidyverse)
library(maps)
library(urbnthemes)

countydata <- countydata


#Urbnmapr on Github: <https://github.com/UrbanInstitute/urbnmapr>
#How To: <https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2>



#MAPS

#US w/ FIPS
ggplot() +
  geom_sf(data=get_urbn_map("states", sf=TRUE), fill="lightgrey", color="white")

#US w/ LatLong
ggplot() +
  geom_polygon(data=get_urbn_map("states"), aes(long, lat, group=group), fill="lightgrey", color="white") +
  coord_map()

#US & Territories Counties w/ FIPS
ggplot() +
  geom_sf(data=get_urbn_map("territories_counties", sf=TRUE), fill="lightgrey", color = "white")

#US & Territories Counties w/ LatLong
ggplot() +
  geom_polygon(data=get_urbn_map("territories_counties"), aes(long, lat, group=group), fill="lightgrey", color="white") +
  coord_map()

#TX w/ FIPS
ggplot() +
  geom_sf(data=get_urbn_map("states", sf=TRUE) %>% filter(state_name=="Texas"), fill="lightgrey", color = "white")

#TX w/ LatLong
ggplot() +
  geom_polygon(data=get_urbn_map("states") %>% filter(state_name=="Texas"), aes(long, lat, group=group), fill="lightgrey", color="white") +
  theme_void() +
  coord_map()

#US Counties w/ FIPS
ggplot() +
  geom_sf(data=get_urbn_map("counties", sf=TRUE), fill="lightgrey", color = "#white")

#US Counties w/ LatLong
ggplot() +
  geom_polygon(data=get_urbn_map("counties"), aes(long, lat, group=group), fill="lightgrey", color="white") +
  coord_map()

#TX Counties w/ FIPS
ggplot() +
  geom_sf(data=get_urbn_map("counties", sf=TRUE) %>% filter(state_name=="Texas"), fill="lightgrey", color="white") +
  theme_void()

#TX Counties w/ LatLong
ggplot() +
  geom_polygon(data=get_urbn_map("counties") %>% filter(state_name=="Texas"), aes(long, lat, group=group), fill="skyblue4", color="white") +
  theme_void() +
  coord_map()

#US w/ State Labels.
ggplot() +
  geom_sf(data=get_urbn_map("states", sf=TRUE), fill="lightgrey", color="white", size=0.25) +
  geom_sf_text(data=get_urbn_labels("states", sf=TRUE), aes(label=state_abbv), size=3)

#TX Counties + City Names w/ LatLong
library(ggrepel)
ggplot() +
  geom_polygon(data=get_urbn_map("counties") %>% filter(state_name=="Texas"), aes(long, lat, group=group), fill="snow3", color="white") +
  geom_label_repel(data=us.cities %>% filter(country.etc=="TX" & pop>=300000), aes(long, lat, label=name), size=2, check_overlap=TRUE, nudge_y=.3) +
  geom_point(data=us.cities %>% filter(country.etc=="TX" & pop>=300000), aes(long, lat)) +
  coord_map() +
  theme_void()




#CHOROPLETHS  

#us Continuous Data w/ FIPS
ggplot() +
  geom_sf(data = left_join(statedata, get_urbn_map(map="states", sf=TRUE), by = "state_fips"), 
          mapping = aes(fill=horate),
          color="white", 
          size=0.25) +
  labs(fill = "Homeownership Rate") 

#us Counties Continuous Data w/ FIPS
ggplot() +
  geom_sf(data = left_join(countydata, get_urbn_map(map="counties", sf=TRUE), by = "county_fips"), 
          mapping = aes(fill=horate),
          color="white", 
          size=0.25) +
  labs(fill = "Homeownership Rate") 

#US Continuous Data w/ LatLong
ggplot() +
  geom_polygon(data=left_join(statedata, get_urbn_map("states"), by="state_fips"),
               mapping=aes(long, lat, group=group, fill=horate),
               color="white") +
  labs(fill= "Homeownership Rate") +
  coord_map()

#us Counties Continuous Data w/ LatLong
ggplot() +
  geom_polygon(data=left_join(countydata, get_urbn_map("counties"), by="county_fips"), 
               mapping=aes(long, lat, group=group, fill=medhhincome)) +
  labs(fill="Median Household Income") + 
  coord_map()

#TX Counties Continuous Data w/ FIPS
ggplot() +
  geom_sf(data = left_join(countydata, get_urbn_map(map="counties", sf=TRUE), by = "county_fips") %>% filter(state_name=="Texas"), 
          mapping = aes(fill=horate),
          color="white", 
          size=0.25) +
  labs(fill = "Homeownership Rate") +
  theme_void()

#TX Counties Continuous Data w/ LatLong
ggplot() +
  geom_polygon(data=left_join(countydata, get_urbn_map("counties"), by="county_fips") %>% filter(state_name=="Texas"),
               mapping = aes(long, lat, group = group, fill=horate),
               color="white") +
  labs(fill = "Homeownership Rate") +
  coord_map() +
  theme_void()


#Style 
set_urbn_defaults(style="map")
statedata %>% 
  left_join(states_sf, by = "state_name") %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = horate), color = "#ffffff", size = 0.25) +
  scale_fill_gradientn(labels = scales::percent, colours=c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB","#1696D2","#12719E","#0A4C6A","#062635")) +
  labs(fill = "Homeownership Rate") +
  coord_sf(datum = NA)
