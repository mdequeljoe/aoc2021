
# d <- strsplit("3,4,3,1,2", ",")[[1]]
# d <- as.numeric(d)
d <- readLines('data/day06.txt', warn = FALSE)
d <- strsplit(d, ',')[[1]]
d <- as.numeric(d)

sim <- function(d, n = 1) {
  for (i in seq_len(n)) {
    completed     <- d == 0
    n             <- length(d[completed])
    d[!completed] <- d[!completed] - 1
    d[completed]  <- 6
    d             <- c(d, rep(8, n))
  }
  d
}

res <- sim(d, 80)
print(length(res))

# part two
options(scipen = 99)
total_fish <- function(d, n) {
  res <- list()
  .h  <- function(timer, n)
    paste0('timer=', timer, '_n=', n)
  .f <- function(timer, n) {
    if (!is.null(ans <- res[[.h(timer, n)]]))
      return(ans)
    
    s <- 1
    if (n <= timer)
      return(s)
    
    duration <- (timer + 1)
    n_rem    <- n - duration
    k        <- floor(n_rem / 7)
    periods  <- n_rem - (seq_len(k + 1) - 1) * 7
    for (p in periods)
      s <- s + .f(8, p)
    
    res[[.h(timer, n)]] <<- s
    s
  }
  
  ans <- vapply(d, .f, numeric(1), n = n)
  sum(ans)
}

print( total_fish(d, 256) )

