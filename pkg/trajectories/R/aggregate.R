aggregate <- function(sttdf, unit){
  
  ##Create an object to store all time stamps from all STI objects
  overall_time <- sttdf@traj[[1]]@time
  
  for(i in 2:length(sttdf@traj)){
    overall_time <- rbind(overall_time, sttdf@traj[[i]]@time)
  }
  
  ##Extract the time stamps and convert into POSIXct objects
  overall_time <- as.data.frame(overall_time)
  time <- as.POSIXct(strptime(row.names(overall_time), "%Y-%m-%d %H:%M:%S"))
  
  ##Extract the time units from the POSIXct objects
  year <- strftime(time, format="%Y")
  month <- strftime(time, format="%m")
  day <- strftime(time, format="%d")
  hour <- strftime(time, format="%H")
  minute <- strftime(time, format="%M")
  second <- strftime(time, format="%S")
  
  ##Add all time units into the data slot of STTDF object
  time_units <- cbind(year, month, day, hour, minute, second)
  sttdf@data <- cbind(sttdf@data, time_units)
  
  if(unit == "hour"){
    hs <- data.frame(dist = rep(0,24), timeLapsed = rep(0,24), 
                     elev = rep(0,24), np = rep(0,24))
    
    hourstring = c("01", "02", "03", "04", "05", "06", 
                   "07", "08", "09", "10", "11", "12",
                   "13", "14", "15", "16", "17", "18",
                   "19", "20", "21", "22", "23", "0")
    
    for(point in 1:nrow(sttdf@data)){
      for(hour in 1:length(hourstring)){
        if(sttdf@data$hour[point] == hourstring[hour]){
          hs$dist[hour] <- sum(hs$dist[hour], sttdf@data$dist[point], na.rm=TRUE)
          hs$timeLapsed[hour] <- sum(hs$timeLapsed[hour], sttdf@data$timeLapsed[point], na.rm=TRUE)
          hs$elev[hour] <- sum(hs$elev[hour], sttdf@data$elev[point], na.rm=TRUE)
          hs$np[hour] <- hs$np[hour] + 1
        }
      }
    }
    ##Calculate spped in km/h
    hs$speed <- hs$dist / hs$timeLapsed *3600
  }
  return(hs)
}