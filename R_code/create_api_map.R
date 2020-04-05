library(tidycensus)
library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(rgdal)
library(stringr)
library(readxl)

Y10 <- as.data.frame(load_variables(year = 2010, dataset = "acs5"))
#acquire data from acs about asian population in each tract
years <- lst(2011,2012,2013,2014,2015)

multi_year <-
  map(
    years,
    ~ get_acs(
      geography = "tract",
      variables = c(asian_pop = "B01001D_001",
                    paci_pop = "B01001E_001",
                    median_income = "B19013_001",
                    total_pop = "B01003_001"),
      state = "CA",
      year = .x,
      geometry = TRUE
    )
  ) %>%
  map2(years, ~ mutate(.x, id = .y))
as_pop_11 <- multi_year$`2011`
as_pop_12 <- multi_year$`2012`
as_pop_13 <- multi_year$`2013`
as_pop_14 <- multi_year$`2014`
as_pop_15 <- multi_year$`2015`

#as_pop <- get_acs(geography = "tract", 
              #variables = c(asian_pop = "B01001D_001",
                            #paci_pop = "B01001E_001",
                            #median_income = "B19013_001",
                            #total_pop = "B01003_001"), 
              #state = "CA", 
              #year = c(2011,2012))


#load shapefile
setwd("C:/Users/cheng/OneDrive/Desktop/map")
tract_ca <- readOGR("cb_2018_06_tract_500k.shp")
api_org_geo <- read_excel("API Org List with contact information updated for recruitment.xlsx",sheet=1)

#api get rid of post box address
#noneed <- grep('^[PO BOX ]', api_org_stm_geo$add12,ignore.case = FALSE)
#api_org_geo <- api_org_stm_geo[-noneed,]
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

for_map <- as_pop_12 %>%
  filter(variable == "asian_pop")
#check shapefile and new group name
table(is.element(tract_ca$GEOID,for_map$GEOID))
ca_as_pop <- for_map[order(match(for_map$GEOID,tract_ca$GEOID)),]
#ca_as_pop $variable <- NULL
#assign color and labels
summary(ca_as_pop$estimate)
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


##########2014#########
for_map <- as_pop_14 %>%
  filter(variable == "asian_pop")
#check shapefile and new group name
table(is.element(tract_ca$GEOID,for_map$GEOID))
ca_as_pop <- for_map[order(match(for_map$GEOID,tract_ca$GEOID)),]
#ca_as_pop $variable <- NULL
#assign color and labels
summary(ca_as_pop$estimate)
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

#########median income#######

for_map <- as_pop_14 %>%
  filter(variable == "median_income")
#check shapefile and new group name
table(is.element(tract_ca$GEOID,for_map$GEOID))
ca_as_pop <- for_map[order(match(for_map$GEOID,tract_ca$GEOID)),]
#ca_as_pop $variable <- NULL
#assign color and labels
summary(ca_as_pop$estimate)
bins <- c(0,20000,40000,60000,80000,100000,150000,Inf)
pal <- colorBin('YlOrRd',domain = ca_as_pop $estimate, bins = bins)
labels <- paste("<p>", ca_as_pop $NAME,"</p>",
                "<p>", "Median Household Income:",round(ca_as_pop $estimate,digits = 2),"</p>",
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

