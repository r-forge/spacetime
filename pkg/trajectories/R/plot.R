plot.STI <- function(x, y, ...) {
  if(!(any(class(x) == "STI"))){
    stop("Object should be of class \"STI\"")
  }
  sl <- STItoSpatialLines(x)
  plot(sl)
}
setMethod("plot", signature(x = "STI", y = "missing"), plot.STI)


plot.STTDF <- function(x, y, ...){
  if(!(any(class(x) == "STTDF"))){
    stop("Object should be of class \"STTDF\"")
  }
  sl <- STTDFtoSpatialLines(x)
  ##Plot the bounding box
  ###need to remove the corner points later
  plot(x@sp, pch = ".")
  ##Create n distinct colors
  colors <- rainbow(length(sl))
  ##Ploting
  for(i in 1:length(sl)){
    lines(sl[i], col = colors[i])
  }
}
setMethod("plot", signature(x = "STTDF", y = "missing"), plot.STTDF)

