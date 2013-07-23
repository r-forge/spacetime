\name{cut.STTDF.SpatialPolygons}
\alias{cut.STTDF.SpatialPolygons}
\title{
Get all points from a trajectory that overlay with a spatial polygon
}
\description{
Get all points from a trajectory that overlay with a spatial polygon
}
\usage{
cut.STTDF.SpatialPolygons(sttdf, pol)
}

\arguments{
  \item{sttdf}{
  a STTDF object
}
  \item{pol}{
  a SpatialPolygons object
}
}

\value{
\code{results} return a single SpatialLines object that contains the trajectories that overlap with the spatial polygon
}
\references{
TBA
}
\author{
Jinlong Yang: jinlong@psu.edu
}