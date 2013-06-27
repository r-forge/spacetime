library(spacetime)
#sel = 1:2
sel = TRUE
lst = list()
i = j = 1
dirs = list.files("Data")[sel]
crs = CRS("+proj=longlat +datum=WGS84")
pb = txtProgressBar(style = 3, max = length(dirs))
elev = numeric(0)
for (d in dirs) {
	dir = paste("Data", d, "Trajectory", sep = "/")
	#print(dir)
	for (f in list.files(dir, pattern = "*plt", full.names = TRUE)) {
		tab = read.csv(f, skip = 6, stringsAsFactors=FALSE)
		tab$time = as.POSIXct(paste(tab[,6],tab[,7]))
		tab[tab[,4] == -777, 4] = NA # altitude 
		tab = tab[,-c(3,5,6,7)]
		names(tab) = c("lat", "long", "elev", "time")
		if (all(tab$lat > -90 & tab$lat < 90 & tab$long < 360 
				& tab$long > -180)) {
			lst[[i]] = STI(SpatialPoints(tab[,2:1], crs), tab$time)
			elev = c(elev, tab$elev)
			i = i+1
		}
	}
	setTxtProgressBar(pb, j)
	j = j+1
}

object.size(lst)
stt = STT(lst)
rm(lst)
dim(stt)
object.size(stt)
sttdf = STTDF(stt, data.frame(elev = elev))
object.size(sttdf)
