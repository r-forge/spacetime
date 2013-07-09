\name{STItoSTTDF}
\alias{STItoSTTDF}

\title{
STI to STTDF with attributes computation
}
\description{
Converts a list of STI objects into a single STTDF object and computes some basic attributes of trajectory, including: distance, time lapsed, change in elevation, change in moving direction between consecutive GPS points recorded. Also allows marking all attribute values with ID and trip.
}
\usage{
STItoSTTDF(list = lst, id = NA, trip = NA, elev = elev)
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{sttdf} return a single STTDF object with all attribute values stored in the data slot of STTDF object.
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Jinlong Yang - jinlong at psu.edu
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
###NOT DIRECTLY EXCUTABLE - NEED TO CREATE A TOY DATASET###
sttdf <- STItoSTTDF(list = lst, elev = elev)
head(sttdf@data)
}

\keyword{GPS}
\keyword{spacetime}