sample <- function(sttdf, seq){
  
  ##Create a list for selected STI objects
  stis <- list()
  data <- data.frame()
  
  ##Create an object to store all time stamps from all STI objects
  overall_time <- sttdf@traj[[1]]@time
  
  for(i in 2:length(sttdf@traj)){
    overall_time <- rbind(overall_time, sttdf@traj[[i]]@time)
  }
  
  ##Extract the time stamps and convert into POSIXct objects
  overall_time <- as.data.frame(overall_time)
  time <- as.POSIXct(strptime(row.names(overall_time), "%Y-%m-%d %H:%M:%S"))
  
  ##
  sample_time <- time[1] + seq
  
  sample_time_index <- which(time %in% sample_time)
  
  sample_sp <- overall_sp[sample_time_index, ]
  
  
  
}