
parse_coords <- function(x){
  x <- strsplit(x, ' -> ')
  lapply(x, function(k){
    k   <- strsplit(k, ',')
    res <- unlist(lapply(k, as.numeric))
    setNames(res, c('x0', 'y0', 'x1', 'y1'))
  })
}

is_line <- function(l){
  l['x0'] == l['x1'] || l['y0'] == l['y1']
}

get_lines <- function(d){
  l <- vapply(d, is_line, logical(1))
  d[l] 
}

line_rng <- function(l){
  if (l['y0'] == l['y1'])
    return( lapply(l['x0']:l['x1'], `c`, l['y0'][[1]]) )
  l <- lapply(l['y0']:l['y1'], `c`, l['x0'][[1]])
  lapply(l, rev)
}

d <- readLines('data/day05.txt', warn = FALSE)
# d <- readLines('data/day05_example.txt', warn = FALSE)
d   <- parse_coords(d)
l   <- get_lines(d)
rng <- lapply(l, line_rng)
rng <- unlist(rng, FALSE)
res <- rng[duplicated(rng)]
res <- unique(res)
print(length(res))

# part two
diag_rng <- function(k){
  a <- c(k['x0'][[1]], k['y0'][[1]])
  b <- c(k['x1'][[1]], k['y1'][[1]])
  d <- -(a - b)
  n <- abs(d[1])
  s <- sign(d)
  lapply(0:n, function(i){
    a + s * i
  })
}

get_rng <- function(d){
  lapply(d, function(k){
    if (is_line(k))
      return(line_rng(k))
    diag_rng(k)
  })
}

rng <- get_rng(d)
rng <- unlist(rng, FALSE)
res <- rng[duplicated(rng)]
res <- unique(res)
print(length(res))
