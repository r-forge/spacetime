##Modified after trajectories/demo/read.R with id and trip added

##Set path where the "GeoLife Trajectories 1.3" is store here and run the entire script
path <- "/Users/jinlong/Documents/R/"

##Load library
library(spacetime)

##Add path to the sub-folders
path <- paste(path, "Geolife Trajectories 1.3/Data/", sep = "")

sel = TRUE
lst = list()
i = j = 1
dirs = list.files(path)[sel]
crs = CRS("+proj=longlat +datum=WGS84")
pb = txtProgressBar(style = 3, max = length(dirs))

##Add id and trip
elev = numeric(0)
id = c()
trip = c()

for (d in dirs) {
  dir = paste(path, d, "Trajectory", sep = "/")
  for (f in list.files(dir, pattern = "*plt", full.names = TRUE)) {
    tab = read.csv(f, skip = 6, stringsAsFactors=FALSE)
    tab$time = as.POSIXct(paste(tab[,6],tab[,7]))
    tab[tab[,4] == -777, 4] = NA # altitude 
    tab = tab[,-c(3,5,6,7)]
    names(tab) = c("lat", "long", "elev", "time")
    if (all(tab$lat > -90 & tab$lat < 90 & tab$long < 360 
            & tab$long > -180)) {
      lst[[i]] = STI(SpatialPoints(tab[,2:1], crs), tab$time)
      attr(lst[[i]], "id") <- d
      elev = c(elev, tab$elev)
      id = c(id, rep(d, nrow(tab)))
      a_trip = substr(f, 74, 86)
      attr(lst[[i]], "trip") <- a_trip
      trip = c(trip, rep(a_trip, nrow(tab)))
      i = i+1
    }
  }
  setTxtProgressBar(pb, j)
  j = j+1
}

object.size(lst)
#stt = STT(lst)
#rm(lst)
#dim(stt)
#object.size(stt)
sttdf = STItoSTTDF(lst, id = id, trip = trip, elev = elev)
object.size(sttdf)
#geolife_sample <- sttdf
#save(geolife_sample, file = "geolife_sample.RData")