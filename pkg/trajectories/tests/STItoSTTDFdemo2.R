##Load libraries
library(spacetime)
library(trajectories)

##Load sample trajectory data
data(traj_sample)

##Convert sample data from list of STI into STTDF
sttdf <- STItoSTTDF(traj_sample)

##Check the results
class(sttdf)

#save(sttdf, file = "STItoSTTDFdemo2.Rout.save")