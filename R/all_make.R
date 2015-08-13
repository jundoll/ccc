#' 初期状態からplot直前までの加工全部
#'
#' @param samples 標本点
#' @param coneN 錐の数
#' @param height 錐の大きさ
#' @param azimuth 錐の最小の角度
#' @param control 追加オプション
#'
#' @examples
#' samples <- data.frame(x = rnorm(10), y = rnorm(10))
#' dat <- all_make(samples, 100, 1, pi/2, control = list(max_loop = 1e+05))
#' @export
all_make <- function(samples, coneN, height, azimuth, control) {
  # exception handling
  stopifnot(coneN > 0)
  if(is.list(samples))  samples <- as.data.frame(samples)
  if(!is.list(samples)) samples <- as.data.frame(matrix(as.vector(samples), 2))
  colnames(samples) <- c("x", "y")

  # initial set
  cone <- data.frame(x = numeric(coneN), y = numeric(coneN), xi = numeric(coneN), AR = numeric(coneN), AL = numeric(coneN))
  settled <- 0
  if(missing(control)) control <- list()
  if(is.null(control$max_loop))     control$max_loop   <- min(3000, coneN)
  if(is.null(control$min_loop))     control$min_loop   <- 500
  if(is.null(control$parallel))     control$parallel   <- TRUE
  if(is.null(control$xrange))       control$xrange     <- range(samples$x)
  if(is.null(control$yrange))       control$yrange     <- range(samples$y)
  if(is.null(control$anglerange))   control$anglerange <- c(0, 2*pi)

  # make
  while(settled < coneN) {
    loops <- max(min(coneN-settled, control$max_loop), control$min_loop)
    cands <- data.frame(x  = runif(loops, control$xrange[1],     control$xrange[2]),
                        y  = runif(loops, control$yrange[1],     control$yrange[2]),
                        xi = runif(loops, control$anglerange[1], control$anglerange[2]))
    ## アルゴリズム実装
    cone0 <- pforeach::pforeach(cand = pforeach::rows(cands), .c = rbind, .parallel = control$parallel) ({
      # res <- c(頂点のx座標, 頂点のy座標, 中心方向, 時計回り方向に最大の角度, 反時計回り方向に最大の角度)
      res <- numeric(5)
      res[1:3] <- c(cand$x, cand$y, cand$xi)
      res[4:5] <- 0

      # 頂点と標本の距離 --> n
      distance <- sqrt( (samples$x - cand$x)^2 + (samples$y - cand$y)^2)

      # condition 1 (flag1 = TRUE --> 円内に含まなれない標本点)
      flag1 <- ( height <= distance )
      if(all(flag1)) {  # 全標本点に対してPCPを満たしているならば, 半円として返す
        res[4:5] <- pi/2
        return(res)
      }

      # condition 2 (flag2 = TRUE --> 錐内に含まれない標本点)
      L <- (samples$x[!flag1] - cand$x) * cos(cand$xi) + (samples$y[!flag1] - cand$y) * sin(cand$xi)
      R <- distance[!flag1] * cos(azimuth / 2)
      flag2 <- flag1
      flag2[!flag1] <- (L <= R)
      if(any(!flag2)) {  # 全標本点に対してPCPを満たさなければ, NAを返す
        res[4:5] <- 0
        return(res)
      }

      # condition 3 (flag3 = TRUE --> まだ錐を描画していない領域に新たな錐を生成できる)
      # --> 未実装 (アルゴリズムがわかっていない)
      # --> このループ外で行う

      # option 1 (時計回り方向に最大の角度を求める)
      angle <- Arg(complex(real = samples$x[!flag1] - cand$x, imaginary = samples$y[!flag1] - cand$y))
      base_angle <- (angle - cand$xi + 4*pi) %% (2*pi)
      res[4] <- if (any(base_angle >= pi)) min(pi/2, 2*pi - max(base_angle[base_angle >= pi])) else pi/2
      # option 2 (反時計回り方向に最大の角度を求める)
      res[5] <- if (any(base_angle < pi)) min(pi/2, min(base_angle[base_angle < pi])) else pi/2

      # return
      return(res)
    })
    cone0 <- as.data.frame(cone0)
    colnames(cone0) <- c('x', 'y', 'xi', 'AR', 'AL')
    rownames(cone0) <- NULL
    cone0 <- subset(cone0, AL >= (azimuth/2), AR >= (azimuth/2))
    if(nrow(cone0) == 0) next
    cone[settled+(1:nrow(cone0)), ] <- cone0
    settled <- settled + nrow(cone0)
  }
  res_cone <- cone[1:coneN, ]
  attr(res_cone, 'height') <- height
  res_cone
}
