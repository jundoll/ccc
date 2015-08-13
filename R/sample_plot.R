#' Plot generated sample
#'
#' @export
sample_plot <- function(dat, ...) {
  info <- attr(dat, "sample_info")
  xrange <- (info$xrange - mean(info$xrange)) * 2 + mean(info$xrange)
  yrange <- (info$yrange - mean(info$yrange)) * 2 + mean(info$yrange)
  plot(samples, xlim = xrange, ylim = yrange, type = 'n', asp = TRUE, ann = FALSE, bty = 'n', xaxt = "n", yaxt = "n")
  rect(xrange[1], yrange[1], xrange[2], yrange[2], col = 'orange', border = 'white')
  points(dat, col = 4, pch = 16)
  lines(info$xarea, info$yarea, ...)
}
