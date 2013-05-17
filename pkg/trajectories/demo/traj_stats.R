##Loading packages
library(sp)
library(spacetime)
library(adehabitatLT)
#library(rgdal)

############Set path of the GeoLife data folder#################
#path <- "D:/Desktop/"
#path <- "/Users/jinlong/Documents/R/"
path <- "./"

##Get the path&name of all trajectory files
all_traj <- traj_reader(path)


##A container for speed of all trajectories
speed_list <- c()


##A container for hourly stats of all trajectories
hourly_statistics <- data.frame(dist=rep(0,24), time=rep(0,24), 
                                alt=rep(0,24), np=rep(0,24))


##Get the number of trajectories for computation of time remaining
nt <- length(all_traj)


##Calculate the speed and hourly stats of all trajectories
start <- Sys.time()
for (i in 1 : nt){
  filename <- all_traj[i]
  sttdf <- toSTTDF(filename)
  speed <- speed_cal(sttdf)
  speed_list <- append(speed_list, speed)
  new_hourly_stats <- hourly_stats(sttdf)
  hourly_statistics <- hourly_statistics + new_hourly_stats
  now <- Sys.time()
  time_passed <- difftime(now, start, unit="secs")
  time_remaining <- round(time_passed * (nt - i) / i, 0)
  print(paste(i, " out of ", nt, " (", round(i/nt*100, 2),
              "%) trajectories completed.---", 
              time_remaining, " seconds remaining.", sep=""))
}
end <- Sys.time()
time_cost <- difftime(end, start, unit="mins")
print("The processing is completed, Sir!")


##Final calculations##

##Convert distance into km
hourly_statistics$dist <- hourly_statistics$dist / 1000


##Convert time into hour
hourly_statistics$time <- hourly_statistics$time / 3600
hourly_statistics$time <- round(hourly_statistics$time, 2)


##Convert altitude into meter
hourly_statistics$alt <- hourly_statistics$alt / 3.2808
hourly_statistics$hourlyalt <- hourly_statistics$hourlyalt / 3.2808


##Calculate hourly average speed and altitude
hourly_statistics$speed <- hourly_statistics$dist / hourly_statistics$time
hourly_statistics$altitude <- hourly_statistics$alt / hourly_statistics$np




##Calculate the totals
total <- c(sum(hourly_statistics$dist), 
           sum(hourly_statistics$time), 
           sum(hourly_statistics$alt), 
           sum(hourly_statistics$np), 
           (sum(hourly_statistics$dist) / 1000) / (sum(hourly_statistics$time)/3600),
           sum(hourly_statistics$alt) / sum(hourly_statistics$np))

hourly_statistics <- rbind(hourly_statistics, total)

##Rename the rows and columns
colnames(hourly_statistics) <- c("TotalDistance(km)", "TotalTime(hour)", "TotalAltitude(meter)", 
                                 "Total#ofPoints", "AverageSpeed(km/h)", "AverageAltitude(meter)")
rownames(hourly_statistics) <- append(sprintf("%02d", 1:24), "Total")


##Generate the name of trajectories and round up the speed to 2 digits.
traj_names <- all_traj
for(i in 1:length(all_traj)){
  traj_names[i] <- substr(all_traj[i], (nchar(all_traj[i]) - 32), nchar(all_traj[i]))
  speed_list[i] <- round(speed_list[i],2)
}


##Combine and rename final_speed_list
final_speed_list <- cbind(traj_names, speed_list)
colnames(final_speed_list) <- c("Trajectory", "Speed(km/h)")


##Export data into a csv file
write.table(hourly_statistics, file = "hourly_statistics.csv", row.names = T, col.names = NA, sep=",")
write.table(final_speed_list, file = "trajectory_speed.csv", row.names = F, sep = ",")




