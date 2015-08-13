#' Generate sample data
#'
#' @export
sample_make <- function(pattern = 1, ...) {
  switch(pattern,
         "1" = sample_paper(...),
         "Not available number")
}
