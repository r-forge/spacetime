setClass("STT",  # space-time trajectory/ies without data values
  representation("ST", traj = "list"),
  validity = function(object) {
    stopifnot(length(object@traj) > 0)
	stopifnot(length(object@sp) == 2)
	stopifnot(length(object@time) == 2)
	stopifnot(all(sapply(object@traj, class) == "STI"))
	stopifnot(!isTRUE(timeIsInterval(object)))
    return(TRUE)
  }
)

setClass("STTDF",  # space-time trajectory/ies with data values
  representation("STT", data = "data.frame"),
  validity = function(object) {
	stopifnot(sum(sapply(object@traj, length)) == nrow(object@data))
    .checkAttrIsUnique(object@sp, object@time, object@data)
    return(TRUE)
  }
)
