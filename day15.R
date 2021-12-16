

ll <- function(){
  node <- function(l, next_ = NULL){
    self <- new.env()
    self$value <- l
    self$next_ <- next_
    self
  }
  self <- new.env()
  self$head <- NULL
  self$push <- function(l){
    nd <- node(l)
    if (is.null(self$head)){
      nd$next_ <- self$head
      self$head <- nd
      return(invisible())
    }
    prev <- tmp <- self$head
    repeat{
      if (is.null(tmp)){
        prev$next_ <- nd
        break()
      }
      if (l$score < tmp$value$score){
        if (identical(tmp, self$head)){
          nd$next_ <- self$head
          self$head <- nd
          break()
        }
        nd$next_ <- tmp
        prev$next_ <- nd
        break()
      }
      prev <- tmp
      tmp  <- tmp$next_
    }
  }
  self$pop <- function(){
    h <- self$head
    self$head <- h$next_
    h$value
  }
  self$is_empty <- function(){
    is.null(self$head)
  }
  self
}

set_grid <- function(d) {
  d <- strsplit(d, '')
  d <- lapply(d, function(x) {
    as.numeric(x)
  })
  matrix(unlist(d),
         nrow = length(d),
         ncol = lengths(d),
         byrow = TRUE)
}

adj_coords <- function(m, coord, nr, nk) {
  r  <- coord[1]
  k  <- coord[2]
  a  <- list(
    left  = c(0, -1),
    up    = c(-1, 0),
    down  = c(1, 0),
    right = c(0, 1)
  )
  res <- lapply(a, function(d) {
    dr <- r + d[1]
    dk <- k + d[2]
    if (dr > nr || dr < 1 || dk > nk || dk < 1)
      return()
    c(dr, dk)
  })
  res[lengths(res) > 0]
}

# dijkstra shortest path
min_risk <- function(m) {

  val <- function(m, coord)
    m[coord[1], coord[2]]
  
  nr <- nrow(m)
  nk <- ncol(m)
  scores <- matrix(Inf, nrow = nr, ncol = nk)
  scores[1, 1] <- 0
  visited <- matrix(FALSE, nrow = nr, ncol = nk)

  qq <- ll()
  qq$push(list(
    coord = c(1, 1),
    score = 0
  ))

  repeat {
    if (qq$is_empty())
      break()
    
    k <- qq$pop()
    if (val(visited, k$coord))
      next()
    visited[k$coord[1], k$coord[2]] <- TRUE
    
    adj <- adj_coords(m, k$coord, nr, nk)
    for (a in adj) {
      if (val(visited, a))
        next()
      # print(a)
      scores[a[1], a[2]] <- 
        min( 
          val(scores, a), 
          val(scores, k$coord) + val(m, a) 
        )
      qq$push(
        list(coord = a, score = val(scores, a))
      )
    }
  }
  scores[nrow(m), ncol(m)]
}

d <- readLines('data/day15.txt', warn = FALSE)
# d <- readLines('data/day15_example.txt', warn = FALSE)

m <- set_grid(d)
res <- min_risk(m)
print(res)

# part two
get_full_grid <- function(m, n = 5){
  .h <- function(r, k)
    sprintf('r=%s_k=%s', r, k)
  cnt <- matrix(0, n, n)
  s <- seq_len(n)
  cnt[1, s] <- s - 1 
  cnt[s, 1] <- s - 1
  for (k in s[-1])
    cnt[s[-1], k] <- cnt[s[1], k] + s[-length(s)]
  
  res <- list()
  for (r in s)
    for (k in s)
      res[[.h(r, k)]] <- (m + cnt[r, k] - 1) %% 9 + 1
  
  res <- split(res, ceiling(seq_along(res) / n))
  res <- lapply(res, function(k) do.call(cbind, k))
  do.call(rbind, res)
}

m2 <- get_full_grid(m)
res <- min_risk(m2)
print(res)

