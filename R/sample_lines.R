#' line xi
#'
#' @param dat dat
#' @param ... option
#'
#' @export
sample_lines <- function(dat, ...) {
  info <- attr(dat, "sample_info")
  lines(info$xarea, info$yarea, ...)
}
