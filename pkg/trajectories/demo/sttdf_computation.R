##Line 1 to Line 50 were copied from read.R

library(spacetime)
#sel = 1:2
sel = TRUE
lst = list()
i = j = 1
#dirs = list.files("Data")[sel]
dirs = list.files("/Users/jinlong/Documents/R/Geolife Trajectories 1.3/Data/")[sel]
crs = CRS("+proj=longlat +datum=WGS84")
pb = txtProgressBar(style = 3, max = length(dirs))
elev = numeric(0)
for (d in dirs) {
	##Changed path
  dir = paste("/Users/jinlong/Documents/R/Geolife Trajectories 1.3/Data", d, "Trajectory", sep = "/")
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
#stt = STT(lst)
#Bug here??
stt = STT(STbox = NULL, lst)
rm(lst)
dim(stt)
object.size(stt)
sttdf = STTDF(stt, data.frame(elev = elev))
object.size(sttdf)







##
count <- 0
for( a in sttdf@traj){
  count <- count + length(a@sp)
}
length(sttdf@traj[[1]]@sp)

##Game started

##Calcualte the distance between consecutive points for each burst (Unit: km)
all_dist <- lapply(sttdf@traj, function(x) 
  LineLength(as.matrix(data.frame(x@sp)), longlat = TRUE, sum = FALSE)
)


##Store all distance between consecutive points to the data slot of each burst
for(i in 1: length(sttdf@traj)){
  sttdf@traj[[i]]@data <- data.frame(all_dist[[i]])
}


##Store all distance between consecutive points from a STTDF object in a single vector - distance
##The distance for the first element in each burst is set to NA
distance <- c()
for(d in all_dist){
  distance <- append(distance, NA)
  distance <- append(distance, d)
}


##Add distance to data slot of the STTDF object
sttdf@data$dist <- dist


##Calculate the time lapsed between consecutive points for each burst (Unit: seconds)
##The time lapsed for the first element in each burst is set to NA
all_timeLapsed <- lapply(sttdf@traj, function(x){
  t1 <- x@time[1: length(x@time) - 1]
  t2 <- x@time[2: length(x@time)]
  timeLapsed <- c(NA, unclass(t2) - unclass(t1))
}
)

##Store all time lapsed between consecutive points from a STTDF boject in a single vector - timeLapsed     
timeLapsed <- c()
for(t in all_timeLapsed){
  timeLapsed <- append(timeLapsed, t)
}

##Add time lapsed to data slot of the STTDF object
sttdf@data$timeLapsed <- timeLapsed


##Calucate speed and stored as a column in data slot of the STTDF object (Unit: m/s)
sttdf@data$speed <- sttdf@data$dist * 1000 / sttdf@data$timeLapsed


##(NOT WORKING) How to identified the first piont from each burst?
##Calculate elevation changes between two consecutive points and store it in sttdf@data as a column
ele1 <- sttdf@data$ele[1: length(sttdf@data$ele) - 1]
ele2 <- sttdf@data$ele[2: length(sttdf@data$ele)]
eleChange <- c(NA, ele2 - ele1)
sttdf@data$eleChange <- eleChange

head(sttdf@data)

min(sttdf@data$eleChange, na.rm = TRUE)
max(sttdf@data$eleChange, na.rm = TRUE)

## (NOT WORKING) (naive) transportaiton mode detection based on speed
if(FALSE){
transMode <- c()
apply(data.frame(sttdf@data$speed[1:10]), 1, function(x){
  if(is.na(x)){
    transMode <- append(transMode, "NA")
  }
  if(!is.na(x)){
    if(x == 0){
      transMode <- append(transMode, "still")
    }
    if(x > 0 & x < 3){
      transMode <- append(transMode, "walk")
    }
    if(x >= 3 & x < 15){
      transMode <- append(transMode, "biking")
    }
    if(x >= 15 & x < 83){
      transMode <- append(transMode, "driving")
    }
    if(x >= 83){
      transMode <- append(transMode, "a flying superman??")
    }
  }
}
)
}
    
    
head(sttdf@data)
sttdf@data[905: 1005, ]
sttdf@traj[[1]]@data

sttdf@sp
