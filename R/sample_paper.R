#' Undescribed
#'
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

  attr(res, "sample_info") <- list(xrange = c(-1/2, 1/2),
                                   yrange = c(-1/2, 1/2),
                                   xarea  = c(0, 1/2, 1/2, 0, 1/2, 1/2, 0, -1/2, -1/2, 0, -1/2, -1/2, 0),
                                   yarea  = c(0, 1/6, 1/2, 0, -1/6, -1/2, 0, 1/6, 1/2, 0, -1/6, -1/2, 0))
  res
}
