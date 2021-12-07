
d <- readLines('data/day07.txt', warn = FALSE)
d <- as.numeric(strsplit(d, ',')[[1]])
# d <- strsplit("16,1,2,0,4,2,7,1,2,14", ",")[[1]]
# d <- as.numeric(d)

min_fuel <- function(d){
  rng <- range(d)
  ans <- vapply(rng[1]:rng[2], function(i){
    sum(abs(d - i))
  }, numeric(1))
  min(ans)
}

print( min_fuel(d) )

# part two
min_fuel2 <- function(d){
  rng <- range(d)
  .f  <- function(v) sum(seq_len(v))
  ans <- vapply(rng[1]:rng[2], function(i){
    dist <- abs(d - i)
    o    <- vapply(dist, .f, numeric(1))
    sum(o)
  }, numeric(1))
  min(ans)
}

print( min_fuel2(d) )

