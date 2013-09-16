##Load libraries
library(spacetime)
library(trajectories)

##Load sample data
data(traj_sample)

##Convert sample data into an STTDF object
sttdf <- STItoSTTDF(traj_sample)

##Create a SpatialPolygons object
lat_min <- min(sttdf@traj[[1]]@sp$lat)
lat_max <- max(sttdf@traj[[1]]@sp$lat)
long_min <- min(sttdf@traj[[1]]@sp$long)
long_max <- max(sttdf@traj[[1]]@sp$long)

lat_range <- lat_max - lat_min
long_range <- long_max - long_min

lat_max <- lat_max - lat_range/2
long_max <- long_max - long_range/2

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

##Do the crop (with discontinued trips divided into sub trips)
sttdf_cropped <- crop(sttdf, pol)
sttdf_cropped

#length(sttdf_cropped@traj[[1]]@sp) + 
#length(sttdf_cropped@traj[[2]]@sp)
#nrow(sttdf_cropped@data)

#plot(sttdf_cropped)
#plot(pol, add = T)
#save(sttdf_cropped, file = "crop_demo.Rout.save")
