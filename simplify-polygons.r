library(geojsonio)
library(rmapshaper)
library(rgdal)
library(sp)

counties <- geojson_read("county-data.geojson",what="sp")

counties_simp <- rmapshaper::ms_simplify(counties,keep=0.01)

pop <- read.csv("population-data.csv")
m <- merge(counties_simp,pop,by='County')

head(m)

rgdal::writeOGR(m,layer="counties","county-data-simplified.geojson",driver="GeoJSON")



