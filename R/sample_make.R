#' Generate sample data
#'
#' @export
sample_make <- function(pattern = 1, ...) {
  switch(pattern,
         "1" = sample_paper(...),
         "2" = sample_name(...),
         "Not available number")
}
#' example of paper
sample_paper <- function(N = 1000) {
  res <- rep(list(numeric(N)), 2)
  names(res) <- c("x", "y")

  for(i in 1:N) {
    outside <- TRUE
    while(outside) {
      xy <- runif(2, -1/2, 1/2)
      axy <- abs(xy)
      h <- axy[2] / axy[1] * 1/2
      outside <- ( h <= 1/6 | h >= 1/2 )
    }
    res$x[i] <- xy[1]
    res$y[i] <- xy[2]
  }

  attr(res, "sample_info") <- list(title  = "sample in paper",
                                   xrange = c(-1/2, 1/2),
                                   yrange = c(-1/2, 1/2),
                                   xarea  = c(0, 1/2, 1/2, 0, 1/2, 1/2, 0, -1/2, -1/2, 0, -1/2, -1/2, 0),
                                   yarea  = c(0, 1/6, 1/2, 0, -1/6, -1/2, 0, 1/6, 1/2, 0, -1/6, -1/2, 0))
  res
}
#' sample data in spatstat package
sample_name <- function(name = "bramblecanes", package = "spatstat") {
  data(list = name, package = package)
  dat <- get(name)
  res <- list(x = dat$x, y = dat$y)

  attr(res, "sample_info") <- list(title  = paste(name, "in", package, "package"),
                                   xrange = dat$window$xrange,
                                   yrange = dat$window$yrange)
  res
}






