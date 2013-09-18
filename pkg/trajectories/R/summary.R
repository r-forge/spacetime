summary.STTDF = function(object, ...) {
  if(!(any(class(object) == "STTDF"))){
    stop("Object should be of class \"STTDF\"")
  }
  obj = list()
  obj[["class"]] = class(object)
  obj[["bbox"]] = bbox(coordinates(object@sp))
  obj[["is.projected"]] = is.projected(object@sp)
  obj[["proj4string"]] = object@sp@proj4string@projargs
  obj[["ntraj"]] = length(object@traj)
  obj[["starting_time"]] = time(object@time[1,])
  obj[["ending_time"]] = time(object@time[length(object@time),])
  obj[["duration"]] = round(as.numeric(difftime(obj[["ending_time"]], obj[["starting_time"]], units = "hours")), 2)
  obj[["total_dist"]] = sum(object@data$dist, na.rm = TRUE)
  obj[["time_lapsed"]] = sum(object@data$timeLapsed, na.rm = TRUE)
  obj[["ave_speed"]] = round(obj[["total_dist"]] / (obj[["time_lapsed"]]), 4)
  ##Unit: km/h
  obj[["ave_elevation"]] <- round(mean(object@data$elev, na.rm = TRUE), 4)
  
  class(obj) = "summary.STTDF"
  return(obj)
}
setMethod("summary", "STTDF", summary.STTDF)

print.summary.STTDF = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("Coordinates extend:\n")
  print(x[["bbox"]])
  cat(paste("Is projected:", x[["is.projected"]], "\n"))
  
  pst <- paste(strwrap(x[["proj4string"]]), collapse="\n")
  
  if (nchar(pst) < 40) cat(paste("proj4string : [", pst, "]\n", sep=""))
  else cat(paste("proj4string :\n[", pst, "]\n", sep=""))
  
  cat("Number of trajectories(STIs): ")
  cat(x[["ntraj"]])
  cat("\n")
  
  cat("Starting time: ")
  st = toString(x[["starting_time"]])
  cat(st)
  cat("\n")
  
  cat("Ending time: ")
  et = toString(x[["ending_time"]])
  cat(et)
  cat("\n")
  
  cat("Duration: ")
  cat(paste(x[["duration"]], "(hour)", sep = ""))
  cat("\n")
  
  cat("Total distance: ")
  cat(paste(x[["total_dist"]]/1000, "(km)", sep = ""))
  cat("\n")
  
  cat("Average speed: ")
  cat(paste(x[["ave_speed"]], "(km/h)", sep = ""))
  cat("\n")
  
  cat("Average elevation: ")
  cat(paste(x[["ave_elevation"]], "(meter)", sep = ""))
  cat("\n")
  
  invisible(x)
}



summary.STI = function(object, ...) {
  if(!(any(class(object) == "STI"))){
    stop("Object should be of class \"STI\"")
  }
  obj = list()
  obj[["class"]] = class(object)
  obj[["bbox"]] = summary(sti@sp)$bbox
  obj[["is.projected"]] = is.projected(object@sp)
  obj[["proj4string"]] = object@sp@proj4string@projargs
  obj[["npoints"]] = length(object@sp)
  obj[["starting_time"]] = time(object@time[1,])
  obj[["ending_time"]] = time(object@time[length(object@time),])
  obj[["duration"]] = round(as.numeric(difftime(obj[["ending_time"]], obj[["starting_time"]], units = "hours")), 2)
  ##May add the dist info later
  
  class(obj) = "summary.STI"
  return(obj)
}
setMethod("summary", "STI", summary.STI)

print.summary.STI = function(x, ...) {
  cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
  cat("Coordinates extend:\n")
  print(x[["bbox"]])
  cat(paste("Is projected:", x[["is.projected"]], "\n"))
  
  pst <- paste(strwrap(x[["proj4string"]]), collapse="\n")
  if (nchar(pst) < 40) cat(paste("proj4string : [", pst, "]\n", sep=""))
  else cat(paste("proj4string :\n[", pst, "]\n", sep=""))
  
  cat("Number of points: ")
  cat(x[["npoints"]])
  cat("\n")
  
  cat("Starting time: ")
  st = toString(x[["starting_time"]])
  cat(st)
  cat("\n")
  
  cat("Ending time: ")
  et = toString(x[["ending_time"]])
  cat(et)
  cat("\n")
  
  cat("Duration: ")
  cat(paste(x[["duration"]], "(hour)", sep = ""))
  cat("\n")
  
  invisible(x)
}