
step <- function(v){
  v['x'] <- v['x'] + v['xv']
  v['y'] <- v['y'] + v['yv']
  if (v['xv'] != 0)
    v['xv']<- v['xv'] - sign( v['xv'] )
  v['yv'] <- v['yv'] - 1
  v
}

launch <- function(v, tgt){
  on_tgt <- function(v, tgt){
    v['x'] <= tgt['x_u']   &&
      v['x'] >= tgt['x_l'] &&
      v['y'] >= tgt['y_l'] &&
      v['y'] <= tgt['y_u']
  }
  res <- list(v)
  repeat{
    v <- step(v)
    if (v['x'] > tgt['x_u'] || v['y'] < tgt['y_l'])
      return()
    res[[length(res) + 1]] <- v
    if (on_tgt(v, tgt))
      return(res)
  }
}

max_y <- function(tgt, xv_rng, yv_rng){
  yval <- -Inf
  for (xv in xv_rng)
    for (yv in yv_rng){
      v <- c(x = 0, y = 0, xv = xv, yv = yv)
      res <- launch(v, tgt)
      if (is.null(res))
        next()
      ym <- max(vapply(res, `[`, numeric(1), 'y'))
      yval <- max(yval, ym)
    }
  yval
}

# d <- 'target area: x=192..251, y=-89..-59'
# d <- 'target area: x=20..30, y=-10..-5'
# tgt <- c(x_l = 20, x_u = 30, y_l = -10, y_u = -5)
# launch(c(x = 0, y = 0, xv = 3, yv = 0), tgt)
# res <- max_y(tgt, 1:10, 1:10)
# print(res)

# part one
tgt <- c(x_l = 192, x_u = 251, y_l = -89, y_u = -59)
res <- max_y(tgt, 1:100, 1:100)
print(res)

# part two
n_values <- function(tgt, xv_rng, yv_rng){
  n <- 0
  for (xv in xv_rng)
    for (yv in yv_rng){
      v <- c(x = 0, y = 0, xv = xv, yv = yv)
      res <- launch(v, tgt)
      if (is.null(res))
        next()
      n <- n + 1
    }
  n
}
# tgt <- c(x_l = 20, x_u = 30, y_l = -10, y_u = -5)
# res <- n_values(tgt, 1:100, -100:100)
# print(res)

tgt <- c(x_l = 192, x_u = 251, y_l = -89, y_u = -59)
res <- n_values(tgt, 1:300, -200:200)
print(res)
