\name{STItoSpatialLines}
\alias{STItoSpatialLines}
\title{
Convert a STI object into a SpatialLines object
}
\description{
Convert a STI object into a SpatialLines object
}
\usage{
STItoSpatialLines(sti)
}

\arguments{
  \item{sti}{
  an STI object
}
}

\value{
\code{sl} return a single SpatialLines object that contains trajectory from STI object. Only the coordinates are kept. All temporal and data attributes are discarded.
}

\author{
Jinlong Yang: jinlong@psu.edu
}