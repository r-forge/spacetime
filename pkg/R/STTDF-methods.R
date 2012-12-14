STT = function(STbox = NULL, trajLst) {
	if (is.null(STbox)) {
    	bb = sapply(trajLst, function(x) bbox(x@sp))
    	sp = SpatialPoints(rbind(c(min(bb[1,]), min(bb[2,])),
        	c(max(bb[3,]), max(bb[4,]))), trajLst[[1]]@sp@proj4string)
    	coordnames(sp) = coordnames(trajLst[[1]]@sp)
		tm = do.call(c, lapply(trajLst, index))
		rt = range(tm)
		STbox = STI(sp, rt)
	}
	new("STT", STbox, traj = trajLst)
}

STTDF = function(STT, data) {
	new("STTDF", STT, data = data)
}

setClass("ltraj", representation("list"))

setAs("ltraj", "STTDF", 
	function(from) {
		d = do.call(rbind, from)
		ns = sapply(from, nrow)
		burst = sapply(from, function(x) attr(x, "burst"))
		id = sapply(from, function(x) attr(x, "id"))
		d$burst = factor(rep(burst, ns))
		d$id = factor(rep(id, ns))
		toSTI = function(x) {
			time = x[["date"]]
			ret = STI(SpatialPoints(x[c("x", "y")]), time)
			attr(ret, "burst") = attr(x, "burst")
			attr(ret, "id") = attr(x, "id")
			ret
		}
		rt = range(d$date)
		STIbox = STI(SpatialPoints(cbind(range(d$x), range(d$y))), rt)
		STTDF(STT(STIbox, trajLst = lapply(from, toSTI)), data = d)
	}
)
setAs("STTDF", "ltraj", 
	function(from) {
		x = as(from, "STIDF")
		xy = coordinates(x@sp)
		da = index(x@time)
		as.ltraj(xy, da, id = x[["id"]], burst = x[["burst"]])
	}
)
setMethod("coordinates", "STT", function(obj) {
		do.call(rbind, lapply(obj@traj, coordinates))
	}
)

# plot.STTDF = function(x, y,..., byBurst = TRUE, 
# 			col = "black", lty = 1, lwd = 1, 
# 			type = "l", pch = 1,
# 			add = FALSE) {
# 	if (add == FALSE)
# 		plot(as(x@sp, "Spatial"), ...) # sets up plotting area
# 	if (byBurst)
# 		f = x$burst
# 	else {
# 		stopifnot(!is.null(x$id))
# 		f = x$id
# 	}
# 	lst = split(data.frame(coordinates(x)), f)
# 	col = rep(col, length = length(lst))
# 	lwd = rep(lwd, length = length(lst))
# 	lty = rep(lty, length = length(lst))
# 	for (i in seq(along = lst))
# 		lines(as.matrix(lst[[i]]), col = col[i], lty = lty[i], lwd = lwd[i],
# 			type = type, pch = pch)
# }
# 
# setMethod("plot", signature(x = "STTDF", y = "missing"), plot.STTDF)
