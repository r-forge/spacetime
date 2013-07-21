traj_sample <- list()
crs <- CRS("+proj=longlat +datum=WGS84")
elev <- c()

path <- "/Users/jinlong/Downloads/trajectories_sample/"
traj_files <- list.files(path)
for(i in 1: length(traj_files)){
  tab <- read.delim(paste(path, traj_files[i], sep = ""), stringsAsFactors = FALSE)
  tab <- tab[, c(3, 4, 5, 2)]
  colnames(tab) = c("lat", "long", "elev", "time")
  tab$time <- as.POSIXct(tab$time)
  if (all(tab$lat > -90 & tab$lat < 90 & tab$long < 360 
          & tab$long > -180)) {
    traj_sample[[i]] = STI(SpatialPoints(tab[,2:1], crs), tab$time)
    elev = c(elev, tab$elev)
  }
}

save(traj_sample, file = "traj_sample.RData")