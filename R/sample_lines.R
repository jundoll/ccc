#' line xi
#'
#' @export
sample_lines <- function(dat, ...) {
  info <- attr(dat, "sample_info")
  lines(info$xarea, info$yarea, ...)
}
