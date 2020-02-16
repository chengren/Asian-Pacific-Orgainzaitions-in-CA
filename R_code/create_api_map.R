library(tidycensus)
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(rgdal)
library(stringr)

Y18 <- load_variables(2018, "acs5", cache = TRUE)
head(Y18)
#acquire data from acs about asian population in each tract
as_pop <- get_acs(geography = "tract", 
              variables = c(asian_pop = "B01001D_001"), 
              state = "CA", 
              year = 2018)

#load shapefile
setwd("C:/Users/cheng/OneDrive/Desktop/map")
tract_ca <- readOGR("cb_2018_06_tract_500k.shp")
api_org_stm_geo <- read.csv('api_org_stm_geo.csv')

#api get rid of post box address
noneed <- grep('^[PO BOX ]', api_org_stm_geo$add12,ignore.case = FALSE)
api_org_geo <- api_org_stm_geo[-noneed,]
# m is the base map here
m<-leaflet() %>%
  addTiles() %>%
  setView(lng=-120,lat=37.8,zoom=5.5)%>%
  addProviderTiles(providers$CartoDB.Positron) #show the blank area
m#print m


# add tract_ca to the map
m_tract_ca <-m %>%
  addPolygons(data = tract_ca,
              color = '#66000',
              weight = 1,
              smoothFactor = 0.5)
m_tract_ca


#check shapefile and new group name
table(is.element(tract_ca$GEOID,as_pop$GEOID))
ca_as_pop <- as_pop[order(match(as_pop$GEOID,tract$GEOID)),]
ca_as_pop $variable <- NULL
#assign color and labels
summary(ca_as_pop $estimate)
bins <- c(0,400,800,1200,1600,2000, Inf)
pal <- colorBin('YlOrRd',domain = ca_as_pop $estimate, bins = bins)
labels <- paste("<p>", ca_as_pop $NAME,"</p>",
                "<p>", "Asian Pop:",round(ca_as_pop $estimate,digits = 2),"</p>",
                sep='')
org_labels = paste("<p>","Name:", api_org_geo$name15,"</p>",
                 "<p>", "Address:",api_org_geo$addree,"</p>",
                 sep='')
#establish map
m_tract_ca_pop <-m %>%
  addPolygons(data = tract_ca,
              color = 'grey',
              weight = 1,
              smoothFactor = 0.5,
              fillOpacity = 0.5,
              fillColor = pal(ca_as_pop $estimate),
              label = lapply(labels, HTML)) %>%
  addLegend(pal = pal,
            values =ca_as_pop$estimate,
            opacity = 0.7,
            position = "topright")%>%
  addMarkers(data = api_org_geo,
             lng = ~long,
             lat = ~lat,
             label = lapply(org_labels, HTML),
             clusterOptions = markerClusterOptions())
m_tract_ca_pop

