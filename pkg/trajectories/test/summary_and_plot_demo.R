##NOTE
##Use test/read.R to read in the (part of) GeoLife dataset as "sttdf" for a better demo

##Load sample data
data(traj_sample)

##Convert sample data into an STTDF object
sttdf <- STItoSTTDF(traj_sample)
class(sttdf)

##Create an STI object out of the STTDF object
sti <- sttdf@traj[[5]]

##Testing the summary function
summary(sti)
summary(sttdf)

##Testing the plot function
plot(sttdf)
plot(sti)