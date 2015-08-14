#' Plot ccc
#'
#' @param ccc ccc's object
#' @param add If TRUE, it added the current picture. By default, FALSE.
#' @param pfine Delicacy of cone. delicacy = 1+pfine*rho(degree) (rounded up)
#' @param ... The other options
#'
#' @export
plot_ccc <- function(ccc, add = FALSE, xi = "n",  pfine = 1, ...) {
  height <- attr(ccc, 'height')
  if(!add) {
    plot(0, xlim = range(ccc$x-height, ccc$x+height), ylim = range(ccc$y-height, ccc$y+height), ann = FALSE, xaxt = "n", yaxt = "n", type = "n", bty = 'n', asp = TRUE)
  }
  if(xi == "n") {
    for(i in 1:nrow(ccc)) {
      theta <- seq(ccc$xi[i] - ccc$AR[i], ccc$xi[i] + ccc$AL[i], length.out = 1 + (ccc$AR[i] + ccc$AL[i]) / pi * 180 * pfine)
      arc <- list(x = ccc$x[i] + cos(theta) * height, y = ccc$y[i] + sin(theta) * height)
      arc <- list(x = c(ccc$x[i], arc$x, ccc$x[i]), y = c(ccc$y[i], arc$y, ccc$y[i]))
      polygon(x = arc$x, y = arc$y, ...)
    }
  } else {
    for(i in 1:nrow(ccc)) {
      theta <- seq(ccc$xi[i] - ccc$AR[i], ccc$xi[i] + ccc$AL[i], length.out = 1 + (ccc$AR[i] + ccc$AL[i]) / pi * 180 * pfine)
      arc <- list(x = ccc$x[i] + cos(theta) * height, y = ccc$y[i] + sin(theta) * height)
      arc <- list(x = c(ccc$x[i], arc$x, ccc$x[i]), y = c(ccc$y[i], arc$y, ccc$y[i]))
      polygon(x = arc$x, y = arc$y, ...)
      segments(ccc$x[i], ccc$y[i], ccc$x[i] + cos(ccc$xi[i]) * height, ccc$y[i] + sin(ccc$xi[i]) * height, col = xi)
    }
  }
  invisible()
}
