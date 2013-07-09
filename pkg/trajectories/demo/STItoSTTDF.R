#STItoSTTDF()

library("spacetime")
library("maptools")

##Input:
## list: a list of STI objects
## id: id of trajectory data. If NA, numeric value "1" will be used as dummy id
## trip: trip id of trajectory data. If NA, numeric value "1" will be used as dummy trip id

STItoSTTDF <- function(list = lst, id = NA, trip = NA, elev = elev){
  stt <- STT(STbox = NULL, list)
  
  #Count total number of points
  count <- 0
  for(i in 1: length(list)){
    np <- nrow(data.frame(list[[i]]@sp))
    count <- count + np
  }
    
  
  if(is.na(id)[1]){
    id = rep(1, count)
  }
  
  if(is.na(trip)[1]){
    trip = rep(1, count)
  }
  
  sttdf <- STTDF(stt, data.frame(id = id, trip = trip, elev = elev))
  
  ##Calcualte the distance between consecutive points for each burst (Unit: km)
  all_dist <- lapply(sttdf@traj, function(x) 
    LineLength(as.matrix(data.frame(x@sp)), longlat = TRUE, sum = FALSE)
  )
  
  ##Store all distance between consecutive points from a STTDF object in a single vector - distance
  ##The distance for the first element in each burst is set to NA
  distance <- c()
  for(d in all_dist){
    distance <- append(distance, NA)
    distance <- append(distance, d)
  }
  
  ##Add distance to data slot of the STTDF object
  sttdf@data$dist <- distance
  
  ##Calculate the time lapsed between consecutive points for each burst (Unit: seconds)
  ##The time lapsed for the first element in each burst is set to NA
  all_timeLapsed <- lapply(sttdf@traj, function(x){
    t1 <- x@time[1: length(x@time) - 1]
    t2 <- x@time[2: length(x@time)]
    timeLapsed <- c(NA, unclass(t2) - unclass(t1))
  }
  )
  
  ##Store all time lapsed between consecutive points from a STTDF boject in a single vector - timeLapsed     
  timeLapsed <- c()
  for(t in all_timeLapsed){
    timeLapsed <- append(timeLapsed, t)
  }
  
  ##Add time lapsed to data slot of the STTDF object
  sttdf@data$timeLapsed <- timeLapsed
  
  ##Calculate the absolute angle between consecutive points for each burst
  absAngle <- lapply(sttdf@traj, function(x){
    points <- as.matrix(data.frame(x@sp))
    startingPoint <- points[1, ]
    return(gzAzimuth(points, startingPoint))
  }
  )
  
  ##Store all absolute turning angles between consecutive points from a STTDF boject in a single vector - all_absAngle     
  all_absAngle <- c()
  for(angle in absAngle){
    all_absAngle <- append(all_absAngle, angle)
  }
  
  ##Add absolute turning angle to data slot of the STTDF object
  sttdf@data$absAngle <- all_absAngle
  
  ##Calucate speed and stored as a column in data slot of the STTDF object (Unit: km/h)
  sttdf@data$speed <- sttdf@data$dist / sttdf@data$timeLapsed * 3600
  
  ##Calculate elevation changes between two consecutive points and store it in sttdf@data as a column
  df1 <- sttdf@data[1: nrow(sttdf@data) - 1, 2:3]
  df2 <- sttdf@data[2: nrow(sttdf@data), 2:3]
  elevChange <- df2[, 2] - df1[, 2]
  
  ##Too slow
  ##How to avoid using for loop here?
  ##How to speed this process up?
  for(i in 1: length(elevChange)){
    if(df1[i, 2] != df2[i, 2]){
      elevChange[i] = NA
    }
  }
  elevChange <- c(NA, elevChange)
  
  
  ##Potential solution
  ##Return shorter from diff()
  elevChange = diff(sttdf@data[,3])
  elevChange = diff(sttdf@data[,3])
  tripChange = diff(as.numeric(sttdf@data[,2])) != 0
  elevChange[tripChange] = NA

  ##Add elevation change to data slot of the STTDF object
  sttdf@data$elevChange <- elevChange
  
  ##Return a STTDF object
  return(sttdf)
}


##Test
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

