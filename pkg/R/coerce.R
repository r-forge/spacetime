# GEOMETRY ONLY:
# STS -> STF
as.STF.STS = function(from) {
	STF(from@sp, from@time, from@endTime)
}
setAs("STS", "STF", as.STF.STS)

# STF -> STS
as.STS.STF = function(from) {
	n = length(from@sp)
	m = nrow(from@time)
	index = cbind(rep(1:n, m), rep(1:m, each=n))
	STS(from@sp, from@time, index, from@endTime)
}
setAs("STF", "STS", as.STS.STF)

# STS -> STI
as.STI.STS = function(from) {
	# replicate the sp and time columns; keeps time always ordered?
	STI(from@sp[from@index[,1],], 
			from@time[from@index[,2]], 
			from@endTime[from@index[,2]])
}
setAs("STS", "STI", as.STI.STS)

# STF -> STI
as.STI.STF = function(from) {
	as(as(from, "STS"), "STI")
}
setAs("STF", "STI", as.STI.STF)

# GEOMETRY+ATTRIBUTES, *DF:
# STSDF -> STFDF
as.STFDF.STSDF = function(from) {
	# fill the partial grid with NAs
	# mainly taken from as.SPixDF.SGDF in sp:
   	data = list()
   	n = length(from@sp) * nrow(from@time)
	index = length(from@sp) * (from@index[,2] - 1) + from@index[,1]
   	for (i in seq(along = from@data)) {
		v = vector(mode(from@data[[i]]), n)
      	if (is.factor(from@data[[i]]))
			v = factor(rep(NA, n), levels = levels(from@data[[i]]))
		else
			v[-index] = NA
		v[index] = from@data[[i]]
		data[[i]] = v
   	}
   	data = data.frame(data, stringsAsFactors = FALSE)
   	names(data) = names(from@data)
	STFDF(from@sp, from@time, data, from@endTime)
}
setAs("STSDF", "STFDF", as.STFDF.STSDF)

# STFDF -> STSDF
as.STSDF.STFDF = function(from) {
	# take out the NA cells and fill the index
	# NOTE: does not (yet) take out empty space/time entities 
	# -- should this be optional?
	n = length(from@sp)
	m = nrow(from@time)
	index = cbind(rep(1:n, m), rep(1:m, each=n))
	# copied from sp:
	sel = apply(sapply(from@data, is.na), 1, function(x) !all(x))
	index = index[sel,,drop=FALSE]
	STSDF(from@sp, from@time, from@data[sel,,drop=FALSE], index, from@endTime)
}
setAs("STFDF", "STSDF", as.STSDF.STFDF)

# STSDF -> STIDF
as.STIDF.STSDF = function(from) {
	# replicate the sp and time columns; keeps time always ordered?
	STIDF(from@sp[from@index[,1],], 
			from@time[from@index[,2]], 
			from@data,
			from@endTime[from@index[,2]])
}
setAs("STSDF", "STIDF", as.STIDF.STSDF)

# STFDF -> STIDF
as.STIDF.STFDF = function(from) {
	as(as(from, "STSDF"), "STIDF")
}
setAs("STFDF", "STIDF", as.STIDF.STFDF)

zerodist.sp = function(from) {
	cc = myCoordinates(from)
	z = zerodist(SpatialPoints(cc))
	if(!is(from, "SpatialPoints") && nrow(z) > 0) {
		sel = apply(z, 1, function(x) identical(from[x[1]],from[x[2]]))
		z = z[sel,]
	}
	# convert to unique IDs, as zerodist(, unique.ID=TRUE) would do:
	id = 1:nrow(cc)
	id[z[,1]] = id[z[,2]]
	return(id)
}

as.STSDF.STIDF = function(from) {
	# find replicates in sp and time, and fill index:
	n = nrow(from@data)
	index = matrix(as.integer(NA), n, 2)
	# space:
	z = zerodist.sp(from@sp)
	uz = unique(z)
	sp = from@sp[uz,] # different attributes at duplicate points get lost...
	index[,1] = match(z, uz)
	# time -- use the fact that xts objects are in time order:
	ix = index(from@time)
	time = unique(ix)
	#timeIsInterval(time) = timeIsInterval(from@time)
	# not that simple -- TODO: glue together & check endTime...
	ir = rle(as.numeric(ix))$lengths
	index[,2] = rep(1:length(ir), ir)
	# check:
	stopifnot(!any(is.na(index)))
	# glue together:
	STSDF(sp, time, from@data, index)
}
setAs("STIDF", "STSDF", as.STSDF.STIDF)

as.STFDF.STIDF = function(from) {
	as(as(from, "STSDF"), "STFDF")
}
setAs("STIDF", "STFDF", as.STFDF.STIDF)

setAs("STT", "STI", 
	function(from) {
		sp = do.call(rbind, lapply(from@traj, function(x) x@sp))
		time = do.call(c, lapply(from@traj, index))
		o = order(time)
		to = time[o,]
		#timeIsInterval(to) = timeIsInterval(from)
		# TODO: take care of endTime??
		new("STI", ST(sp[o,,drop=FALSE], to)) # reorders!
	}
)
setAs("STTDF", "STIDF", 
	function(from) {
		sp = do.call(rbind, lapply(from@traj, function(x) x@sp))
		time = do.call(c, lapply(from@traj, index))
		attr(time, "tzone") = attr(index(from@traj[[1]]), "tzone")
		#timeIsInterval(time) = timeIsInterval(from)
		# TODO: take care of endTime?
		STIDF(sp, time, from@data)
	}
)
setAs("STIDF", "STTDF", 
	function(from) {
		traj = lapply(split(from, from$burst), function(x) as(x, "STI"))
		STIbox = STI(SpatialPoints(cbind(range(from$x), range(from$y)), 
				from@sp@proj4string), range(from$date))
		new("STTDF", new("STT", STIbox, traj = traj), data = from@data)
	}
)

as.STFDF.Spatial = function(from) {
	#from@data$time = index(from@time)
	df = as.data.frame(t(as(from[,,1], "xts")))
	ret = addAttrToGeom(geometry(from@sp), df, match.ID = FALSE)
	# data.frame names will now be mangled time-like strings, so
	attr(ret, "time") = index(from@time) # to make it somehow accessible...
	ret
}
setAs("STFDF", "Spatial", as.STFDF.Spatial)

as.STIDF.Spatial = function(from) {
	from@data$time = index(from@time)
	addAttrToGeom(geometry(from@sp), from@data, match.ID = FALSE)
}
setAs("STIDF", "Spatial", as.STIDF.Spatial)
setAs("STSDF", "Spatial", function(from) as(as(from, "STIDF"), "Spatial"))
