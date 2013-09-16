##Load libraries
library(spacetime)
library(trajectories)

##Load sample data
sttdf <- data(geolife_sample)

##Create a SpatialPolygons object
lat_min <- min(sttdf@traj[[1]]@sp$lat)
lat_max <- max(sttdf@traj[[1]]@sp$lat)
long_min <- min(sttdf@traj[[1]]@sp$long)
long_max <- max(sttdf@traj[[1]]@sp$long)

xpol <- c(long_min, 
          long_max, 
          long_max, 
          long_min, 
          long_min)
ypol <- c(lat_min, 
          lat_min, 
          lat_max, 
          lat_max, 
          lat_min)

pol <- SpatialPolygons(list(Polygons(list(Polygon(cbind(xpol,ypol))), ID="x1")))
pol@proj4string <- CRS("+proj=longlat +datum=WGS84")

##Do the crop
sttdf_cropped <- crop(sttdf, pol)


plot(sttdf)
plot(pol, add = TRUE)

plot(sttdf_cropped)
plot(pol, add = TRUE)

#plot(pol, add=T)
#plot(sttdf_cropped)
#save(sttdf_cropped, file = "crop_and_plot_demo.Rout.save")
