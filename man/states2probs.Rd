\name{states2probs}
\alias{states2probs}
\title{Convert states to vector of probabilities of any given state}
\usage{
states2probs(weight.matrix = NULL, states = NULL, type = "MLE")
}
\arguments{
  \item{states}{array of size \eqn{N} with entry \eqn{i}
  being the label \eqn{k = 1, \ldots, K} of PLC \eqn{i}}

  \item{weight.matrix}{\eqn{N \times K} weight matrix}

  \item{type}{estimation type for the probabilities:
  \code{c("MLE")}}
}
\description{
  Converts states (either as a size \eqn{N} array of labels
  or an \eqn{N \times K} weight matrix) into \eqn{N}
  probabilities per state.

  If \code{states} is a matrix - representing the state
  space in the dimensions of the original data - then the
  probabilities will be formatted automatically to an array
  of the same shape and dimension.
}
\examples{
state.sim <- sample.int(5, 100, replace = TRUE)
prob.state <- states2probs(states = state.sim)
layout(matrix(1:2, ncol = 2))
plot(state.sim, xlab = "", ylab = "state")
plot(prob.state, xlab = "", ylab = "probability")
plot(state.sim, prob.state, xlab = "state", ylab = "probability", type = "h")
}
\seealso{
  \code{\link[LICORS]{weight_matrix2states}}
}
\keyword{manip}
