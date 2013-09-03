\name{aggregate}
\alias{aggregate}
\title{
Aggregate an STTDF object by various time units.
}
\description{
Aggregate the stats from an STTDF object by various time units such as hour
}
\usage{
aggregate(sttdf, unit)
}

\arguments{
  \item{sttdf}{
  a STTDF object
}
  \item{unit}{
  a string that specifies the time unit, such as "hour".
}
}

\value{
\code{hs} return a dataframe that contains the aggregated stats for the STTDF object
}
\references{
TBA
}
\author{
Jinlong Yang: jinlong@psu.edu
}