##Load sample data
data(traj_sample)

##Convert sample data into an STTDF object
sttdf <- STItoSTTDF(traj_sample)

##Get hourly stats for sttdf
hourly_stat <- aggregate(sttdf, "hour")