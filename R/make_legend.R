
#' @title Add a color-scale legend to a plot
#' @description
#' This function adds a nicely formatted color-scale legend to a plot.
#' 
#' \strong{NOTE:} This function will be made available in the \pkg{LICORS} package in the next release. It will 
#' then be removed from the \code{NAMESPACE} of the \pkg{LSC} package.
#' 
#' @param data data for which the legend should be plotted
#' @param side on which side of the plot (1=bottom, 2=left, 3=top, 4=right)
#' @param col.ticks color tick marks
#' @param max.height height of the density plot (typically not modified by user)
#' @param cex.axis The magnification to be used for axis annotation relative to 
#' the current setting of \code{cex}.
#' @param col colors: either a string decribing a pallette from the 
#' \code{RColorBrewer} package (see also \url{http://colorbrewer2.org/}), or a list of colors 
#' (see \code{\link[graphics]{image}} for suggestions).
#' @param zlim minimum and maximum z values for which colors should be 
#' plotted, defaulting to the range of the finite values of \code{data}.
#' @param col.label same as \code{zlim.label}
#' @export
#' @keywords aplot color
#' @examples 
#' xx = rnorm(100)
#' make_legend(xx, col = "RdBu")
#' make_legend(xx, col = "RdYlGn", side = 4)
#' 

make_legend <- function(data = NULL, col = NULL, side = 1, 
                        zlim = NULL, col.ticks = NULL, cex.axis = 2, 
                        max.height = 1, col.label = "") {
  
  min <- min(data)
  max <- max(data)
  if (is.null(zlim)) {
    zlim <- c(min, max)
  }
  if (length(col) == 1 & is.character(col)) {
    col <- colorRampPalette(brewer.pal(9, name = col))(100)
  }
  color_levels <- seq(zlim[1], zlim[2], length = length(col))
  
  if (is.null(col.ticks)) {
    col.ticks <- pretty(color_levels)
  }
  
  if (any(side == c(1, 3))) {
    image(color_levels, c(0, max.height), 
          matrix(data = rep(color_levels, time = 2), nrow = length(color_levels), ncol = 2), 
          col = col, xlab = "", 
          ylab = "", axes = FALSE, zlim = zlim, ylim = c(0, max.height))
  } else {
    image(c(0, max.height), color_levels, 
          matrix(data = rep(color_levels, each = 2), ncol = length(color_levels), nrow = 2), 
          col = col, xlab = "", 
          ylab = "", axes = FALSE, zlim = zlim, xlim = c(0, max.height))
  }
  axis(side = side, at = col.ticks, labels = paste(col.ticks), cex.axis = cex.axis, 
       col = "black")
  mtext(col.label, side = side, line = 3, cex = 0.75 * cex.axis)
  box()
} 
