\name{states2probs}
\alias{states2probs}
\title{Convert states to vector of probabilities of any given state}
\usage{
states2probs(weight_matrix = NULL, state_vector = NULL, type = "MLE")
}
\arguments{
  \item{state_vector}{array of size \eqn{N} with entry
  \eqn{i} being the label \eqn{k = 1, \ldots, K} of PLC
  \eqn{i}}

  \item{weight_matrix}{\eqn{N \times K} weight matrix}

  \item{type}{estimation type for the probabilities:
  \code{c("MLE")}}
}
\description{
  Converts states (either as a size \eqn{N} array of labels
  or an \eqn{N \times K} weight matrix) into \eqn{N}
  probabilities per state.

  If \code{state_vector} is a matrix - representing the
  state space in the dimensions of the original data - then
  the probabilities will be formatted automatically to an
  array of the same shape and dimension.
}
\examples{
ss <- sample.int(5, 100, replace = TRUE)
pp <- states2probs(state_vector = ss)
par(mfrow = c(2, 2), mar = c(4, 5, 1, 1))
plot(ss, xlab = "", ylab = "state")
plot(pp, xlab = "", ylab = "probability")
plot(ss, pp, xlab = "state", ylab = "probability")
}
\seealso{
  \code{\link{weight_matrix2states}}
}
\keyword{manip}
