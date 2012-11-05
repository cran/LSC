\name{states2LSC}
\alias{states2LSC}
\title{Estimate local statistical complexity (LSC) from states}
\usage{
states2LSC(weight_matrix = NULL, state_vector = NULL, base = 2, type = "MLE")
}
\arguments{
  \item{weight_matrix}{\eqn{N \times K} weight matrix}

  \item{state_vector}{array of size \eqn{N} with entry
  \eqn{i} being the label \eqn{k = 1, \ldots, K} of PLC
  \eqn{i}.}

  \item{base}{logarithm base for complexity (entropy).
  Default: \code{base = 2} (thus 'bits').}

  \item{type}{estimation type for the probabilities:
  \code{c("MLE")}}
}
\description{
  Converts states (either as a size \eqn{N} array of labels
  or an \eqn{N \times K} weight matrix) into \eqn{N} local
  statistical complexities (LSC) per state.

  If \code{state_vector} is a matrix - representing the
  state space in the dimensions of the original data - then
  the LSC output will be formatted automatically to an
  array of the same shape and dimension.
}
\seealso{
  \code{\link{states2probs}}
}
\keyword{manip}
