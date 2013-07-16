over.STTDF.STTDF = function(x, y, returnList = FALSE, ...) {
  create.super <- function(x){
    super.x.sp <- SpatialPoints(matrix(c(0,0), nrow = 1), x@sp@proj4string)
    super.x.time <- c()
    for(i in 1:length(x@traj)){
      super.x.sp <- spRbind(super.x.sp, x@traj[[i]]@sp)
      super.x.time <- rbind(super.x.time, x@traj[[i]]@time)
    }
    super.STI <- STI(super.x.sp[2: length(super.x.sp)], super.x.time)
    return(super.STI)
  }
  
  x.super.STI <- create.super(x)
  y.super.STI <- create.super(y)
  
  lst = list(index(x.super.STI@time), index(y.super.STI@time), returnList = TRUE)
  if (any(x.super.STI@endTime > as.POSIXct(index(x.super.STI@time))))
    lst[["end.x"]] = x.super.STI@endTime
  if (any(y.super.STI@endTime > as.POSIXct(index(y.super.STI@time))))
    lst[["end.y"]] = y.super.STI@endTime
  time.index = do.call(timeMatch, lst)
  ret = lapply(1:length(time.index), function(i) {
    ti = time.index[[i]]
    if (length(ti) > 0)
      over(x.super.STI@sp[i,], y.super.STI@sp[ti,], returnList = TRUE)[[1]] + (ti - 1)
    else
      integer(0)
  })
  if (! returnList)
    ret = unlist(lapply(ret, function(x) { x[1] }))
  ret
}

setMethod("over", signature(x = "STTDF", y = "STTDF"), over.STTDF.STTDF)