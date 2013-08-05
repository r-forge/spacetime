STItoSpatialLines <- function(sti){
  #Create a container for STI objects
  lines <- list()
  lines[[1]] <- Lines(Line(sti@sp), 1)
  
  ##Convert STI object in a single SpatialLines object
  sl <- SpatialLines(lines)
  sl@proj4string <- CRS("+proj=longlat +datum=WGS84")
  return(sl)
}