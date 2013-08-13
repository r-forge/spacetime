crop <- function(sttdf, pol){
  
  ##Create two objects to store all sp and time from all STI objects
  overall_sp <- sttdf@traj[[1]]@sp@coords
  overall_time <- sttdf@traj[[1]]@time 
  for(i in 2:length(sttdf@traj)){
    overall_sp <- rbind(overall_sp, sttdf@traj[[i]]@sp@coords)
    overall_time <- rbind(overall_time, sttdf@traj[[i]]@time)
  }
  
  ##Run the overlay using point.in.polygon()
  overlay <- point.in.polygon(
    point.x = overall_sp[, 1],
    point.y = overall_sp[, 2],
    pol.x = pol@polygons[[1]]@Polygons[[1]]@coords[, 1], 
    pol.y = pol@polygons[[1]]@Polygons[[1]]@coords[, 2]
  )
  
  ##Get indices of overlapped (inside) points
  inside <- which(overlay > 0)
  #outside <- which(overlay == 0)
  
  ##Get the inside sp, time, and data
  sp_inside <- overall_sp[inside, ]
  sp_inside <- SpatialPoints(sp_inside, proj4string = sttdf@traj[[1]]@sp@proj4string)
  time_inside <- overall_time[inside, ]
  data_inside <- sttdf@data[inside, ]
  
  ##Create an STTDF object with inside points (sp, time, data)
  sti <- STI(sp_inside, time_inside)
  stt <- STT(list(sti))
  sttdf <- STTDF(stt, data_inside)
  
  return(sttdf)
}