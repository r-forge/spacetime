##Demo
##Without id and trip
sttdf <- STItoSTTDF(list = lst, elev = elev)
head(sttdf@data)

##With id and trip from GeoLife
sttdf <- STItoSTTDF(list = lst, id = id, trip = trip, elev = elev)
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