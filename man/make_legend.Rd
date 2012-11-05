\name{make_legend}
\alias{make_legend}
\title{Add a color-scale legend to a plot}
\usage{
make_legend(data = NULL, col = NULL, side = 1, zlim = NULL, col.ticks = NULL, 
    cex.axis = 2, max.height = 1, col.label = "")
}
\arguments{
  \item{data}{data for which the legend should be plotted}

  \item{side}{on which side of the plot (1=bottom, 2=left,
  3=top, 4=right)}

  \item{col.ticks}{color tick marks}

  \item{max.height}{height of the density plot (typically
  not modified by user)}

  \item{cex.axis}{The magnification to be used for axis
  annotation relative to the current setting of
  \code{cex}.}

  \item{col}{colors: either a string decribing a pallette
  from the \code{RColorBrewer} package (see also
  \url{http://colorbrewer2.org/}), or a list of colors (see
  \code{\link[graphics]{image}} for suggestions).}

  \item{zlim}{minimum and maximum z values for which colors
  should be plotted, defaulting to the range of the finite
  values of \code{data}.}

  \item{col.label}{same as \code{zlim.label}}
}
\description{
  This function adds a nicely formatted color-scale legend
  to a plot.

  \strong{NOTE:} This function will be made available in
  the \pkg{LICORS} package in the next release. It will
  then be removed from the \code{NAMESPACE} of the
  \pkg{LSC} package.
}
\examples{
xx <- rnorm(100)
make_legend(xx, col = "RdBu")
make_legend(xx, col = "RdYlGn", side = 4)
}
\keyword{aplot}
\keyword{color}
