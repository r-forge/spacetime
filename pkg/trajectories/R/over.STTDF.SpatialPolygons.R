over.STTDF.SpatialPolygons <- function(sttdf, pol){
  #Create a container for STI objects
  lines <- list()
  
  ##Store all STI objects into a list
  for(i in 1: length(sttdf@traj)){
    lines[[i]] <- Lines(Line(sttdf@traj[[i]]@sp), i)
  }
  
  ##Convert all STI objects in a single SpatialLines object
  sl <- SpatialLines(lines)
  
  ##Overlap operation
  for(i in length(sl): 1){
    overlay <- point.in.polygon(
      point.x = sl[i]@lines[[1]]@Lines[[1]]@coords[, 1], 
      point.y = sl[i]@lines[[1]]@Lines[[1]]@coords[, 2], 
      pol.x = pol@polygons[[1]]@Polygons[[1]]@coords[, 1], 
      pol.y = pol@polygons[[1]]@Polygons[[1]]@coords[, 2]
    )
    if(!any(overlay > 0)){
      sl@lines[[i]] <- NULL
    }
  }
  return(sl)
}