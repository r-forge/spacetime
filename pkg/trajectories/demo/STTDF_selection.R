library(rgdal)
library(rgeos)


##Create a toy SpatialPolygon and two SpatialLines (trajs1 & trajs2) to test over()

long_max <- 116.3942
long_min <- 116.1451
lat_max <- 40.07611
lat_min <- 39.88710

xpol <- c(long_min, 
         long_max, 
         long_max, 
         long_min, 
         long_min)

ypol <- c(lat_min, 
         lat_max, 
         lat_max, 
         lat_min, 
         lat_min)

pol <- SpatialPolygons(list(Polygons(list(Polygon(cbind(xpol,ypol))), ID="x1")))
pol@proj4string <- CRS("+proj=longlat +datum=WGS84")

trajs1 <- list()
for(i in 1: 7){
  aline <- Lines(Line(sttdf@traj[[i]]), toString(i))
  trajs1 <- append(trajs1, aline) 
}
trajs1 <- SpatialLines(trajs1)
trajs1@proj4string <- CRS("+proj=longlat +datum=WGS84")

trajs2 <- list()
for(i in 8: 15){
  aline <- Lines(Line(sttdf@traj[[i]]), toString(i))
  trajs2 <- append(trajs2, aline) 
}
trajs2 <- SpatialLines(trajs2)
trajs2@proj4string <- CRS("+proj=longlat +datum=WGS84")


##Test over()
over(pol, trajs1)
over(trajs1, pol)

over(pol, trajs2)
over(trajs2, pol, returnList = TRUE)

