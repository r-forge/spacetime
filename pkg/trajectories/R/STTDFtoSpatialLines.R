STTDFtoSpatialLines <- function(sttdf){
  #Create a container for STI objects
  lines <- list()
  
  ##Store all STI objects into a list
  for(i in 1: length(sttdf@traj)){
    lines[[i]] <- Lines(Line(sttdf@traj[[i]]@sp), i)
  }
  
  ##Convert all STI objects in a single SpatialLines object
  sl <- SpatialLines(lines)
  sl@proj4string <- CRS("+proj=longlat +datum=WGS84")
  return(sl)
}