cut.STTDF.SpatialPolygons <- function(sttdf, pol){
  ##Convert the STTDF object in a single SpatialLines object
  sl <- STTDFtoSpatialLines(sttdf)
  
  ##Create a container to store all inside SpatialLines
  sl_inside <- list()
  
  ##Overlap operation
  for(i in 1: length(sl)){
    #i <- 2
    overlay <- point.in.polygon(
      point.x = sl[i]@lines[[1]]@Lines[[1]]@coords[, 1], 
      point.y = sl[i]@lines[[1]]@Lines[[1]]@coords[, 2], 
      pol.x = pol@polygons[[1]]@Polygons[[1]]@coords[, 1], 
      pol.y = pol@polygons[[1]]@Polygons[[1]]@coords[, 2]
    )
    inside <- which(overlay > 0)
    #outside <- which(overlay == 0)
    
    if(length(inside) > 0){
      coords <- sl[1]@lines[[1]]@Lines[[1]]@coords
      coord_inside <- coords[inside, ]
      
      sp_inside <- SpatialPoints(coord_inside, proj4string = sl@proj4string)
      lines_inside <- Lines(Line(sp_inside), i)
      
      sl_inside[[i]] <- lines_inside
    } 
    if(length(inside) == 0){
      sl_inside[[i]] = 999
    }
  }
  for(j in 1:length(sl_inside)){
    if(class(sl_inside[[j]]) != "SpatialLines"){
      sl_inside[[j]] = NULL
    }
  }
  sl_inside <- SpatialLines(sl_inside)
  sl_inside@proj4string <- CRS("+proj=longlat +datum=WGS84")
  return(sl_inside)
}