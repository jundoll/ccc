#' Plot generated sample
#'
#' @examples
#' samples <- sample_make(pattern = 1)
#' dat <- all_make(samples, 100, 1/2, pi/5)
#' sample_plot(samples)
#' plot_ccc(dat, add = TRUE, xi = "black", col = "white", border = "black")
#' sample_lines(samples)
#' @export
sample_plot <- function(dat, col = 4, pch = 16, ...) {
  info <- attr(dat, "sample_info")
  xrange <- (info$xrange - mean(info$xrange)) * 2 + mean(info$xrange)
  yrange <- (info$yrange - mean(info$yrange)) * 2 + mean(info$yrange)
  plot(dat, xlim = xrange, ylim = yrange, type = 'n', asp = TRUE, xlab = "", ylab = "", bty = 'n', xaxt = "n", yaxt = "n", main = info$title)
  rect(xrange[1], yrange[1], xrange[2], yrange[2], col = 'orange', border = 'white')
  points(dat, col = col, pch = pch, ...)
}
#' line xi
sample_lines <- function(dat, ...) {
  info <- attr(dat, "sample_info")
  lines(info$xarea, info$yarea, ...)
}
