crop <- function(sttdf, pol){
  
  ##Create a list for selected STI objects
  stis <- list()
  data <- data.frame()
  
  ##Do overlay operation for each STI object in the STTDF object
  for(i in 1:length(sttdf@traj)){
    points_processed <- 0
    sti <- sttdf@traj[[i]]
    overlay <- point.in.polygon(
      point.x = sti@sp$long,
      point.y = sti@sp$lat,
      pol.x = pol@polygons[[1]]@Polygons[[1]]@coords[, 1], 
      pol.y = pol@polygons[[1]]@Polygons[[1]]@coords[, 2]
    )
    
    ##Keep counting the number of points processed
    points_processed <- points_processed + length(sti@sp)
    
    ##If no overalp point, skip to next STI object
    if(sum(overlay) == 0){
      break
    }else{
      ##Get the index for overlay points
      inside_index <- which(overlay > 0)
      
      ##Index trips
      trip_index <- c(0, cumsum(diff(inside_index) != 1))
      
      ##Convert consecutive points into STI object
      for(j in 0:max(trip_index)){
        sub_trip_index <- which(trip_index == j)
        sp_inside <- sti@sp[sub_trip_index, ]
        sp_inside <- SpatialPoints(sp_inside, proj4string = sttdf@traj[[i]]@sp@proj4string)
        time_inside <- sti@time[sub_trip_index, ]
        data_inside <- sttdf@data[sub_trip_index + points_processed, ]
        
        stis <- c(stis, STI(sp_inside, time_inside))
        data <- rbind(data, data_inside)
      }
    }
  }
  
  ##Create an STTDF object with all STI objects
  stt <- STT(stis)
  sttdf_cropped <- STTDF(stt, data)
  
  return(sttdf_cropped)
}