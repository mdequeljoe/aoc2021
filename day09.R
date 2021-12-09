
adj_coord <- function(m, r, k){
  nr <- nrow(m)
  nk <- ncol(m)
  a  <- list(
    up    = c(-1, 0),
    down  = c(1, 0),
    left  = c(0, -1),
    right = c(0, 1)
  )
  res <- lapply(a, function(d){
    dr <- r + d[1]
    dk <- k + d[2]
    if (dr > nr || dr < 1 || dk > nk || dk < 1)
      return()
    c(dr, dk)
  })
  res[lengths(res) > 0]
}

is_low_point <- function(m, r, k){
  a   <- adj_coord(m, r, k)
  res <- vapply(a, function(x) m[r, k] < m[x[1], x[2]], logical(1))
  all(res)
}


d <- readLines('data/day09.txt', warn = FALSE)
# d <- readLines('data/day09_example.txt', warn = FALSE)

d <- strsplit(d, "\n")
d <- lapply(d, function(r) {
  as.numeric(strsplit(r, '')[[1]])
})
m <-
  matrix(unlist(d),
         ncol = lengths(d)[1],
         nrow = length(d),
         byrow = TRUE)

s <- 0
for (r in 1:nrow(m))
  for (k in 1:ncol(m))
    if (is_low_point(m, r, k))
      s <- s + m[r, k] + 1
print(s)

# part two
size_basin <- function(m, r, k) {
  .h <- function(cell)
    sprintf('x=%d_y=%d', cell[1], cell[2])
  
  qq  <- list(c(r, k))
  res <- logical(0)
  
  repeat {
    if (!length(qq))
      break()
    
    coord <- qq[[1]]
    qq    <- qq[-1]
    ac    <- adj_coord(m, coord[1], coord[2])
    
    for (cell in ac) {
      seen <- isTRUE(res[.h(cell)])
      if (m[cell[1], cell[2]] == 9 || seen)
        next()
      qq[[length(qq) + 1]] <- cell
    }
    
    res[.h(coord)] <- TRUE
  }
  
  length(res)
}

basins <- numeric(0)
for (r in 1:nrow(m))
  for (k in 1:ncol(m))
    if (is_low_point(m, r, k))
      basins[length(basins) + 1] <- size_basin(m, r, k)

basins <- sort(basins, TRUE)
ans    <- Reduce(`*`, basins[1:3])
print(ans)

