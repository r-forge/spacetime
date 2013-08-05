summary.STI <- function (object, ...){
  if(!(any(class(object) == "STI"))){
    stop("Object should be of class \"STI\"")
  }
  print(summary(object@sp))
  starting <- time(object@time[1,])
  ending <- time(object@time[length(object@time),])
  duration <- as.numeric(difftime(ending, starting, units = "mins"))
  cat("\n")
  cat(paste("Time:", "\n"))
  cat(paste("Starting time:", starting, "\n"))
  cat(paste("Ending time:", ending, "\n"))
  cat(paste("Duration: ", duration, "mins", "\n", sep = ""))
}
setMethod("summary", signature(object = "STI"), summary.STI)

summary.STTDF <- function (object, ...){
  if(!(any(class(object) == "STTDF"))){
    stop("Object should be of class \"STTDF\"")
  }
  total_dist <- sum(object@data$dist, na.rm = TRUE)
  total_timeLapsed <- sum(object@data$timeLapsed, na.rm = TRUE)
  ave_speed <- round(total_dist / (total_timeLapsed/3600), 4)
  ##Unit: km/h
  
  print(summary(object@sp))
  starting <- time(object@time[1,])
  ending <- time(object@time[length(object@time),])
  duration <- as.numeric(difftime(ending, starting, units = "mins"))
  cat("\n")
  cat(paste("Time:", "\n"))
  cat(paste("Starting time:", starting, "\n"))
  cat(paste("Ending time:", ending, "\n"))
  cat(paste("Duration: ", duration, "mins", "\n", sep = ""))
  cat("\n")
  cat(paste("Average speed: ", ave_speed, "km/h", "\n", sep = ""))
}
setMethod("summary", signature(object = "STTDF"), summary.STTDF)
