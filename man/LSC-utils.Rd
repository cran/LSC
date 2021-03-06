\name{LSC-utils}
\alias{LSC-utils}
\alias{plot.LSC}
\alias{plot_LSC_0plus1D}
\alias{plot_LSC_1plus1D}
\alias{plot_LSC_2plus1D}
\title{Utilities for LSC class}
\usage{
\method{plot}{LSC}(x, ...)

plot_LSC_1plus1D(z, col = NULL, lsc.unit = "bits", heights = c(2, 5), widths = c(5, 
    2))

plot_LSC_2plus1D(z, type = "temporal", time.frames = NULL, zlim = NULL, heights = NULL, 
    lsc.unit = "bits", data = NULL, col = NULL)

plot_LSC_0plus1D(z, col = NULL, lsc.unit = "bits", ...)
}
\arguments{
  \item{x}{an object of class \code{"LSC"}}

  \item{...}{optional arguments passed to
  \code{\link{plot_LSC_2plus1D}} or
  \code{\link{plot_LSC_1plus1D}}.}

  \item{widths}{passed to \code{\link[graphics]{layout}}
  for dividing the plotting region horizontally. A vector
  of length 2: image (left) & temporal average (right)}

  \item{z}{an object of class \code{"LSC"}}

  \item{type}{a \code{"temporal"} or a \code{"spatial"}
  summary plot of LSC}

  \item{time.frames}{a vector of length \eqn{\leq 6} to
  indicate what frames should be displayed (only for
  \code{type = "temporal"}). If \code{NULL} (default) then
  it chooses them automatically based on valleys and peaks
  in the spatial average LSC.}

  \item{zlim}{minimum and maximum z values for which colors
  should be plotted, defaulting to the range of the finite
  values of \code{z}.}

  \item{lsc.unit}{character string (default: \code{"bits"})
  to write next to the color legend}

  \item{col}{colors: either a string decribing a pallette
  from the \code{RColorBrewer} package (see also
  \url{http://colorbrewer2.org/}), or a list of colors (see
  \code{\link[graphics]{image}} for suggestions).}

  \item{data}{(optional) original data to compare to LSC
  (relevant only for \code{type = "spatial"})}

  \item{heights}{passed to \code{\link[graphics]{layout}}
  for dividing the plotting region vertically. If
  \code{data = NULL} a vector of length 2; otherwise a
  vector of length 3.}
}
\description{
  The \code{"LSC"} class lies at the core of this package
  as it describes spatio-temporal patterns in the data.  It
  is usually an array with the same spatio-temporal
  resolution as the original dataset.

  \code{plot.LSC} plots LSC of \eqn{(1+1)D} and
  \eqn{(2+1)D} systems.

  \code{\link{plot_LSC_1plus1D}} plots LSC for a (1+1)D
  field.

  \code{\link{plot_LSC_2plus1D}} plots LSC for a (2+1)D
  field.

  \code{\link{plot_LSC_0plus1D}} plots LSC for a (0+1)D
  field, i.e., a time series.
}
\examples{
\dontrun{
data(contCA00)

temp_lsc <- states2LSC(states = contCA00$predictive_states - 
    min(contCA00$predictive_states) + 1)
class(temp_lsc) <- c("LSC", "LSC_1plus1D")
plot_LSC_1plus1D(temp_lsc)
}
\dontrun{
data(contCA00)

temp_lsc <- states2LSC(states = contCA00$predictive_states - 
    min(contCA00$predictive_states) + 1)
temp_lsc_3D <- array(temp_lsc, dim = c(25, 20, 40))
class(temp_lsc_3D) <- c("LSC", "LSC_2plus1D")
plot_LSC_2plus1D(temp_lsc_3D, type = "temporal")
plot_LSC_2plus1D(temp_lsc_3D, type = "spatial")
}
state.sim <- rpois(100, 1)

lsc.est <- states2LSC(states = state.sim)
class(lsc.est) <- c("LSC", "LSC_0plus1D")
plot_LSC_0plus1D(lsc.est)

weights.sim <- matrix(runif(1000, 0, 1), ncol = 10)
weights.sim <- normalize(weights.sim)
lsc.est <- states2LSC(weight.matrix = weights.sim)
plot_LSC_0plus1D(lsc.est)
}
\seealso{
  \code{\link[LICORS]{plot.mixed_LICORS}},
  \code{\link{plot_LSC_2plus1D}},
  \code{\link{plot_LSC_1plus1D}}
}
\keyword{hplot}
