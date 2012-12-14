\name{STTDF-class}
\docType{class}
\alias{STT-class}
\alias{STTDF-class}
\alias{STT}
\alias{STTDF}
\alias{ltraj-class}
\alias{coerce,STTDF,ltraj-method} 
\alias{coerce,ltraj,STTDF-method}
\alias{plot,STT,missing-method}
\title{Class "STTDF"}
\description{ A class for spatio-temporal trajectory data }
\section{Objects from the Class}{
Objects of this class carry sparse (irregular) space/time data }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{Object of class \code{"Spatial"}, containing the bounding
	box of all trajectories}
    \item{\code{time}:}{Object of class \code{"xts"}, containing the temporal bounding
	box of all trajectories}
	\item{\code{traj}:}{Object of class \code{list}, which element holding
	a \link{STI} object with an individual trajectory; }
	\item{\code{data}:}{Object of class \code{data.frame}, which holds
	the data values }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "STTDF")}: selects spatial-temporal entities, and attributes } 

    % \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  \S4method{coerce}{STTDF,ltraj}(from, to, strict=TRUE)
  \S4method{coerce}{ltraj,STTDF}(from, to, strict=TRUE)
}

\arguments{
\item{from}{from object}
\item{to}{target class}
\item{strict}{ignored}
}

\note{ The \code{data.frame} needs to have a column called
\code{burst} which is a factor (or character) and contains the
grouping of observations that come from a continuous sequence of
observations. In addition, a column \code{id} is used to identify
individual items. }

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }
\references{ http://www.jstatsoft.org/v51/i07/ }

\keyword{classes}
