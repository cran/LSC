#' @title Convert states to vector of probabilities of any given state
#'
#' @description 
#' Converts states (either as a size \eqn{N} array of labels or an 
#' \eqn{N \times K} weight matrix) into \eqn{N} probabilities per state.
#' 
#' If \code{state_vector} is a matrix - representing the state space in the 
#' dimensions of the original data - then the probabilities will be formatted 
#' automatically to an array of the same shape and dimension.
#' 
#' @param state_vector array of size \eqn{N} with entry \eqn{i} being the label 
#' \eqn{k = 1, \ldots, K} of PLC \eqn{i}
#' @param weight_matrix \eqn{N \times K} weight matrix
#' @param type estimation type for the probabilities: \code{c("MLE")}
#' @keywords manip
#' @export
#' @seealso \code{\link{weight_matrix2states}}
#' @examples
#' 
#' ss = sample.int(5, 100, replace = TRUE)
#' pp = states2probs(state_vector = ss)
#' par(mfrow = c(2,2), mar = c(4,5,1,1))
#' plot(ss, xlab = "", ylab = "state")
#' plot(pp, xlab = "", ylab = "probability")
#' plot(ss, pp, xlab = "state", ylab = "probability")
#' 

states2probs <- function(weight_matrix = NULL, state_vector = NULL, type = "MLE") {
  
  if (is.null(state_vector) & is.null(weight_matrix)) {
    stop("You must provide either state vector or the weight matrix.")
  }
  
  if (!is.null(weight_matrix)) {
    marginal_state_probs <- LICORS::estimate_state_probs(weight_matrix)
    point_probs <- rowSums(sweep(weight_matrix, 2, marginal_state_probs, "*"))
  } else {
    point_probs <- LICORS::estimate_state_probs(state_vector = state_vector)[state_vector]
    names(point_probs) <- NULL
  }
  
  if (!is.null(state_vector) & !is.null(dim(state_vector))) {
    invisible(array(point_probs, dim = dim(state_vector)))
  } else {
    invisible(point_probs)
  }
}