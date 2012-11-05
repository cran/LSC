#' @title Utilities for LSC class
#' @name LSC-utils
#' @aliases plot.LSC plot_LSC_1plus1D plot_LSC_2plus1D
#' @description 
#' 
#' The \code{"LSC"} class lies at the core of this package as it describes
#' the spatio-temporal patterns in the data.  It is usually an array with the 
#' same spatio-temporal resolution as the original dataset.
#' 
#' \code{plot.LSC} plots LSC of \eqn{(1+1)D} and \eqn{(2+1)D} systems.
#' 
NULL

#' @rdname LSC-utils
#' @keywords hplot
#' @method plot LSC
#' @param x an object of class \code{"LSC"}
#' @param ... optional arguments passed to \code{\link{plot_LSC_2plus1D}} or 
#' \code{\link{plot_LSC_1plus1D}}.
#' @seealso \code{\link[LICORS]{plot.mixed_LICORS}}, 
#' \code{\link{plot_LSC_2plus1D}}, \code{\link{plot_LSC_1plus1D}}
#' @export
#' 
plot.LSC <- function(x, ...) {
  object <- x
  
  ndim <- length(dim(object))
  
  if (ndim < 2){
    stop("Plotting for time series LSC not yet implemented.")
  } else if (ndim == 2){
    plot_LSC_1plus1D(object, ...)
  } else if (ndim == 3){
    plot_LSC_2plus1D(object, ...)
  } else {
    stop("Plotting for more than 3D is not implemented.")
  } 
} 
