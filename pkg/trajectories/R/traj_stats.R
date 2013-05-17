##Loading packages
#library(sp)
#library(spacetime)
#library(adehabitatLT)
#library(rgdal)

##This function gets the path&name of all trajectory files
traj_reader <- function(path, start = 0, end = 181) {
  
  #Replace the underscore later
  dir <- paste(path, "Geolife Trajectories 1.3/Data/", sep = "")
  
  ##Generate user index
  users <- sprintf("%03d", start:end) 
  
  ##Get path of all folders
  folders <- c()
  for(i in 1:length(users)){
    a_folder <- paste(dir, users[i], "/Trajectory/", sep = "")
    folders <- append(folders, a_folder)
  }
  
  ##Get trajectory files from all folders
  file_list <- c()
  for (i in 1:length(folders)){
    files <- list.files(folders[i])
    for(j in 1: length(list.files(folders[i]))){
      file_list <- append(file_list, 
                          paste(folders[i], list.files(folders[i])[j], sep = ""))
    }
  }
  
  return(file_list)
}

#This function converts trajectory files in STDDF objects
toSTTDF <- function(traj){
  ##Read in data and set column names
  d <- read.table(traj, skip = 6, sep = ",")
  colnames(d) <- c("lat", "long", "x", "alt", "dayPassed", "date", "time")
  
  ##Convert data&time to POSIXct objects
  dt <- as.character(paste(d$date, d$time))
  dt <- as.POSIXct(strptime(dt, "%Y-%m-%d %H:%M:%S"), tz = "GMT")
  
  ##Remove points with duplicated timestamp
  d$datetime <- dt
  if(length(dt) != length(unique(dt))){
    d <- d[!duplicated(d$datetime), ]
  }
  
  ##Determine the UTM zone by the first GPS point
  ##and convert raw data to ltraj
  zone <- (floor((d$long[1] + 180)/6) %% 60) + 1
  longlat <- data.frame(d$long, d$lat)
  locs <- project(as.matrix(longlat), 
                  paste("+proj=utm +zone=", zone, " +ellps=WGS84", sep = ""))
  xy <- coordinates(locs)
  ltr <- as.ltraj(xy, unique(dt), id = rep(1,nrow(d)), burst = rep(0,nrow(d)))
  
  ##Convert ltraj to STTDF
  sttdf <- as(ltr, "STTDF")
  
  ##Assign projection property to sttdf
  proj4string(sttdf@sp) <- paste("+proj=utm +zone=", zone, " +ellps=WGS84", sep="")
  
  ##Attached elevation as an attribute
  sttdf@data$new.col <- d$alt
  colnames(sttdf@data)[13] <- "alt"
  
  ##Extract and attached hour as an attribute
  ##(for computing speed/elevation per hour of day)
  sttdf@data$new.col <- strftime(sttdf@data$date, format = "%H", tz = "GMT")
  colnames(sttdf@data)[14] <- "hour"
  
  return(sttdf)
}

##Speed calculator - Unit: km/h
speed_cal <- function(sttdf){
  distance <- sum(sttdf@data$dist[1:length(sttdf@data$dist)-1])/1000
  timeLapsed <- sum(sttdf@data$dt[1:length(sttdf@data$dt)-1])/3600
  hoursLapsed <- timeLapsed
  speed <- distance/hoursLapsed
}

##This function calculates the hourly statstistics of all trajectories
hourly_stats <- function(sttdf){
  hs <- data.frame(dist = rep(0,24), time = rep(0,24), 
                   alt = rep(0,24), np = rep(0,24))
  
  hourstring = c("01", "02", "03", "04", "05", "06", 
                 "07", "08", "09", "10", "11", "12",
                 "13", "14", "15", "16", "17", "18",
                 "19", "20", "21", "22", "23", "00")
  
  for(point in 1:(nrow(sttdf@data)-1)){
    for(hour in 1:length(hourstring)){
      if(sttdf@data$hour[point] == hourstring[hour]){
        hs$dist[hour] <- hs$dist[hour] + sttdf@data$dist[point]
        hs$time[hour] <- hs$time[hour] + sttdf@data$dt[point]
        hs$alt[hour] <- hs$alt[hour] + sttdf@data$alt[point]
        hs$np[hour] <- hs$np[hour] + 1
      }
    }
  }
  
  return(hs)
}
