##############################
#  Produce a study site map  #
##############################


library(tidyverse)
library(maps)
library(mapdata)
library(ggmap)
library(readr)
library(raster)
library(fields)
library(GISTools)


pdf("../figures/map_field.pdf")
## Carte d'Espagne
expe_site <- unlist(geocode(c("Cabezo de la plata spain")))
maps::map(database="worldHires", interior=FALSE, 
    xlim =  c(expe_site["lon"] - 11, expe_site["lon"] + 11),
    ylim =  c(expe_site["lat"] - 11, expe_site["lat"] + 11), 
    fill = TRUE, col = "white",
    bg = "light blue") 
maps::map.scale(x = expe_site["lon"] + 5, 
    y = expe_site["lat"] - 10,
    relwidth = 0.15, ratio = FALSE)
map.axes()
north.arrow(xb = expe_site["lon"] + 7, 
    yb = expe_site["lat"] - 9, 
    len = 0.25, 
    lab = "N")

## Cities
cities <- geocode(c("Madrid spain","Alicante spain", "Murcia spain"))
cities <- cbind(cities, lwd = c(1,1,1), pch = c(19, 1, 1), label = c("Madrid", "Alicante", "Murcia"))  
cities <- filter(cities, label == "Madrid")
points(cities, lwd = cities[, "lwd"], pch = cities[, "pch"])
text(cities, labels = cities[, "label"], pos = 4)

## Field Site
points(geocode(c("Cabezo de la plata spain")), lwd = 4, pch = 19, col = "red")

## Sea
ocean <- tibble(
    lon = c(-8.100338, 4.861122), 
    lat = c(46.14820, 37.7),
    labels = c("ATLANTIC \n OCEAN", "Mediterranean \n sea")
)
text(ocean , labels = ocean$labels, col = "blue", font = 3)

## Countries
countries <- tibble(
   lon = c(2.805560, -3.332576, -8.071788, -6.187523, 2.491516), 
   lat = c(46.68896, 39.50128, 39.97445, 32.08827, 30.75889),
   labels = c("FRANCE", "SPAIN", "PORTUGAL", "MOROCCO", "ALGERIA")
    )
text(countries, labels = countries$labels, font = 2)
countries 
dev.off()

