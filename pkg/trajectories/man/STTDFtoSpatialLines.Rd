\name{STTDFtoSpatialLines}
\alias{STTDFtoSpatialLines}
\title{
Convert a STTDF object into a SpatialLines object
}
\description{
Convert a STTDF object into a SpatialLines object
}
\usage{
STTDFtoSpatialLines(sttdf)
}

\arguments{
  \item{sttdf}{
  a STTDF object
}
}

\value{
\code{sl} return a single SpatialLines object that contains all trajectories from the STTDF object. Only the coordinates are kept. All temporal and data attributes are discarded.
}

\author{
Jinlong Yang: jinlong@psu.edu
}