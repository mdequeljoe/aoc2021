
d <- readLines('data/day11.txt', warn = FALSE)
# d <- readLines('data/day11_example.txt', warn = FALSE)
# d <- readLines('data/day11_examplebis.txt', warn = FALSE)

set_grid <- function(d){
  d <- strsplit(d, '')
  d <- lapply(d, as.numeric)
  matrix(unlist(d), ncol = lengths(d)[1], nrow = length(d), byrow = TRUE)
}

adj_coords <- function(m, coord){
  nr <- nrow(m)
  nk <- ncol(m)
  a  <- list(
    up          = c(-1,  0),
    down        = c( 1,  0),
    left        = c( 0, -1),
    right       = c( 0,  1),
    upper_left  = c(-1, -1),
    upper_right = c(-1,  1),
    lower_left  = c( 1, -1),
    lower_right = c( 1,  1)
  )
  res <- lapply(a, function(d){
    dr <- coord[1] + d[1]
    dk <- coord[2] + d[2]
    if (dr > nr || dr < 1 || dk > nk || dk < 1)
      return()
    c(dr, dk)
  })
  res[lengths(res) > 0]
}

.h <- function(coord)
  sprintf('row=%d_col=%d', coord[1], coord[2])

get_flashed <- function(m){
  o <- which(m > 9, arr.ind = TRUE, useNames = FALSE)
  o <- lapply(seq_len(nrow(o)), function(i) o[i,])
  setNames(o, vapply(o, .h, character(1)))
}

val <- function(m, coord) m[coord[1], coord[2]]

model_energy <- function(m, steps = 100){
  n_flashed <- 0
  for (s in seq_len(steps)){
    m  <- m + 1
    qq <- flashed <- get_flashed(m)
    repeat{
      if (length(qq) == 0)
        break()
      
      coord <- qq[[1]]
      qq    <- qq[-1]
      a     <- adj_coords(m, coord) 
      
      for (k in a){
        if (!is.null( flashed[[ .h(k) ]] ))
          next()
        
        m[ k[1], k[2] ] <- val(m, k) + 1
        if (val(m, k) > 9)
          qq[[ .h(k) ]] <- flashed[[ .h(k) ]] <- k
      }
    }
    for (k in flashed)
      m[ k[1], k[2] ] <- 0
    n_flashed <- n_flashed + length(flashed)
  }
  n_flashed
}

m <- set_grid(d)
print( model_energy(m, 100) )

# part two
sync_grid <- function(m){
  cnt <- 0
  repeat{
    cnt <- cnt + 1
    m   <- m + 1
    qq  <- flashed <- get_flashed(m)
    repeat{
      if (length(qq) == 0)
        break()
      
      coord <- qq[[1]]
      qq    <- qq[-1]
      a     <- adj_coords(m, coord) 
      
      for (k in a){
        if (!is.null( flashed[[ .h(k) ]] ))
          next()
        
        m[ k[1], k[2] ] <- val(m, k) + 1
        if (val(m, k) > 9)
          qq[[ .h(k) ]] <- flashed[[ .h(k) ]] <- k
      }
    }
    for (k in flashed)
      m[ k[1], k[2] ] <- 0
    
    if (all(m == 0))
      return(cnt)
  }
}

m <- set_grid(d)
print( sync_grid(m) )
