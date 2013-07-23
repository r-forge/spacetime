over.STTDF.SpatialPolygons <- function(sttdf, pol){
  
  ##Convert the STTDF object in a single SpatialLines object
  sl <- STTDFtoSpatialLines(sttdf)
  
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