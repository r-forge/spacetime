\name{STItoSTTDF}
\alias{STItoSTTDF}

\title{
STI to STTDF with attributes computation
}
\description{
Converts a list of STI objects into a single STTDF object and computes some basic attributes of trajectory, including: distance, time lapsed, change in elevation, change in moving direction between consecutive GPS points recorded. Also allows marking all attribute values with ID and trip.
}
\usage{
STItoSTTDF(list, id = NA, trip = NA, elev = NA)
}

\arguments{
  \item{list}{
  a list of STI objects.
}
  \item{id}{
  A vector of IDs of GPS point record. If NA, numeric value "1" will be added as a dummy id for all GPS points. The length of the vector should be equal to the total number of GPS points in list.
  }
  \item{trip}{
  A vector of trip of GPS point record. If NA, numeric value "1" will be added as a dummy trip for all GPS points. The length of the vector should be equal to the total number of GPS points in list.
  }
  \item{elev}{
  A vector of levation of GPS point record. The length of the vector should be equal to the total number of GPS points in list.
  }
}
\value{
\code{sttdf} return a single STTDF object with all attribute values stored in the data slot of STTDF object.
}

\author{
Jinlong Yang - jinlong at psu.edu
}

\examples{
 lat <- runif(10, 39, 40)
 long <- runif(10, 115, 116)
 elev <- runif(10, 95, 100)
 t1 <- "2008-10-24 02:09:59"
 t2 <- "2008-10-24 02:10:08" 
 t1 <- as.POSIXct(t1, tz = "GMT")
 t2 <- as.POSIXct(t2, tz = "GMT")
 time <- seq(t1, t2, by = 1)
 traj <- data.frame(lat, long, elev, time)
 traj <- STI(SpatialPoints(traj[,2:1]), traj$time)
 
 lst <- list()
 lst[[1]] <- traj
 
 STItoSTTDF(list = lst, elev = elev)
}

\keyword{spatial}
