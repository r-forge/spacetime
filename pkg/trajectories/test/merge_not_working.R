merge <- function(sttdf, attr_column){
  ##Create an object to store all sp points from all STI objects
  overall_sp <- sttdf@traj[[1]]@sp
  for(i in 2:length(sttdf@traj)){
    overall_sp <- rbind(overall_sp, sttdf@traj[[i]]@sp)
  }
  
  ##Create an object to store all time stamps from all STI objects
  overall_time <- sttdf@traj[[1]]@time
  for(i in 2:length(sttdf@traj)){
    overall_time <- rbind(overall_time, sttdf@traj[[i]]@time)
  }
  
  
  
}