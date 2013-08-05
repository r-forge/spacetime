##Demo
##Create a dummy trajectory with 10 points
lat <- runif(10, 39, 40)
long <- runif(10, 115, 116)
elev <- runif(10, 95, 100)
t1 <- "2008-10-24 02:09:59"
t2 <- "2008-10-24 02:10:08" 
t1 <- as.POSIXct(t1, tz = "GMT")
t2 <- as.POSIXct(t2, tz = "GMT")
time <- seq(t1, t2, by = 1)
traj <- data.frame(lat, long, elev, time)

##Convert the trajectory into a STI object
traj <- STI(SpatialPoints(traj[,2:1]), traj$time)

##Store the trajectory into a list
lst <- list()
lst[[1]] <- traj

##Demo for STItoSTTDF method
#sttdf <- STItoSTTDF(list = lst, elev = elev, id = id, trip = trip)
sttdf <- STItoSTTDF(list = lst, elev = elev)

##Check trajectory attributes computated.
head(sttdf@data)

##Check the max and min displacement
min(sttdf@data$dist, na.rm = TRUE)
max(sttdf@data$dist, na.rm = TRUE)

##Check the max and min time lapsed
min(sttdf@data$timeLapsed, na.rm = TRUE)
max(sttdf@data$timeLapsed, na.rm = TRUE)

##Check the max and min absAngle
min(sttdf@data$absAngle, na.rm = TRUE)
max(sttdf@data$absAngle, na.rm = TRUE)

##Check the max and min speed
min(sttdf@data$speed, na.rm = TRUE)
max(sttdf@data$s, na.rm = TRUE)

##Check the max and min elevation change
##There are some extreme values here.. Need to triple check..!
min(sttdf@data$elevChange, na.rm = TRUE)
max(sttdf@data$elevChange, na.rm = TRUE)