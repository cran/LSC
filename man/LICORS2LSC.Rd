\name{LICORS2LSC}
\alias{LICORS2LSC}
\title{Estimates LSC from a LICORS estimate}
\usage{
LICORS2LSC(object, type = "weights")
}
\arguments{
  \item{object}{an object of class \code{"LICORS"}}

  \item{type}{should marginal state probabilities be
  computed based on the unique state space assignment
  (\code{"argmax"}) or using the soft thresholding from the
  weight matrix (\code{"weights"}).}
}
\value{
  An object of class \code{"LSC"}
}
\description{
  A wrapper of \code{\link{states2LSC}} for a
  \code{'LICORS'} estimate from the \pkg{LICORS} package
  (in particular the output from the
  \code{\link[LICORS]{mixed_LICORS}} function). Converts
  LICORS estimates into an array with LSC.
}
\examples{
\dontrun{
# see 2nd example in 'LSC-package'
}
}
\seealso{
  \code{\link{states2LSC}}, \code{\link{LSC-utils}}
}
\keyword{manip}
