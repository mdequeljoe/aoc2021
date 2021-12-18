
set_pairs <- function(d){
  d <- strsplit(d, ' -> ')
  setNames(
    vapply(d, `[`, character(1), 2),
    vapply(d, `[`, character(1), 1)
  )
}

node <- function(value, next_ = NULL){
  self <- new.env(parent = emptyenv())
  self$value <- value
  self$next_ <- next_
  self
}

ll <- function(){
  self <- new.env()
  self$head <- NULL
  .h <- function(a, b) paste0(a, b)
  self$show <- function(){
    current <- self$head
    res     <- character(0)
    repeat{
      if (is.null(current))
        break
      res <- c(res, current$value)
      current <- current$next_
    }
    paste(res, collapse = '')
  }
  self$match <- function(pairs){
    current <- self$head
    repeat{
      cval <- current$value
      if (length(cval) == 1)
        break()
      val  <- pairs[.h(cval[1], cval[2])]
      current$value[2] <- val
      
      if (is.null(current$next_)){
        current$next_ <- node(cval[2])
        break()
      }
      current <- current$next_
    }
  }
  self$update_pairs <- function(){
    current <- self$head
    repeat{
      k   <- current$next_
      if (is.null(k))
        break()
      val <- c(current$value[2], k$value[1])
      o <- node(val)
      current$next_ <- o
      o$next_ <- k
      current <- k
    }
  }
  self
}

run_steps <- function(polymer, pairs, n = 10){
  polymer <- strsplit(polymer, '')[[1]]
  p <- lapply(seq(1, length(polymer) - 1, 1), function(i){
    polymer[c(i, i + 1)]
  })
  
  l <- ll()
  l$head <- node(p[[1]], NULL)
  k <- l$head
  
  for (i in seq_along(p)[-1]){
    k$next_ <- node(p[[i]], NULL)
    k <- k$next_
  }
  
  l$match(pairs)
  for (i in seq_len(n)[-1]){
    l$update_pairs()
    l$match(pairs)
  }
  l$show()
}

d <- readLines('data/day14.txt', warn = FALSE)
# d <- readLines('data/day14_example.txt', warn = FALSE)
polymer <- d[1]
pairs   <- set_pairs( d[3:length(d)] )

res <- run_steps(polymer, pairs, 10)
res <- strsplit(res, '')[[1]]
res <- table(res)
print( max(res) - min(res) )

# part two
# (, N) (N, N) (N, C) (C, B) (B, )
# 
# (, N) (N, N) (N, C) (C, B) (B, )
#          C      B      H
# 
# (, N) (N, C) (C, N) (N, B) (B, C) (C, H) (H, B) (B, ): n = 4, c = 4, b = 4, h = 2
# 
# divide table by 2 gives correct result s1  -> (N, C) (N, B) (C H) (B, )

run_steps <- function(polymer, pairs, n = 10) {
  polymer <- strsplit(polymer, '')[[1]]
  .hl  <- function(l) {
    a <- l[[1]]
    b <- l[[2]]
    if (!length(a))
      a <- 'empty'
    if (!length(b))
      b <- 'empty'
    paste0(a, b)
  }
  .hp <- function(v)
    paste0(v[1], v[2])
  p <- list()
  for (i in seq_along(polymer)) {
    pr <- list(polymer[i - 1], polymer[i])
    if (is.null( p[[ .hl(pr) ]] )){
      p[[ .hl(pr) ]] <- list(pair = pr, cnt = 1)
    } else {
      p[[ .hl(pr) ]]$cnt <- p[[ .hl(pr) ]]$cnt + 1
    }
  }
  pr <- list(polymer[length(polymer)], character(0))
  p[[ .hl(pr) ]] <- list(pair = pr, cnt = 1)
  
  for (step in seq_len(n)) {
    np <- list()
    for (i in seq_along(p)) {
      pr <- p[[i]]$pair
      if (any(lengths(pr) == 0)) {
        np[[ .hl(pr) ]] <- p[[i]]
        next()
      }
      
      cnt <- p[[i]]$cnt
      val <- pairs[[ .hp(pr) ]]
      pr_1 <- list(pr[[1]], val)
      pr_2 <- list(val, pr[[2]])
      
      if (is.null( np[[ .hl(pr_1) ]] )){
        np[[ .hl(pr_1) ]] <- list(pair = pr_1, cnt = cnt)
      } else {
        np[[ .hl(pr_1) ]]$cnt <- np[[ .hl(pr_1) ]]$cnt + cnt
      }
      
      if (is.null( np[[ .hl(pr_2) ]])){
        np[[ .hl(pr_2) ]] <- list(pair = pr_2, cnt = cnt)
      } else {
        np[[ .hl(pr_2) ]]$cnt <- np[[ .hl(pr_2) ]]$cnt + cnt
      }
    }
    p <- np
  }
  p
}

table_letters <- function(res) {
  ans <- setNames(rep(0, length(LETTERS)), LETTERS)
  for (x in res)
    for (k in x$pair)
      ans[k] <- ans[k] + x$cnt
  ans[ans != 0] / 2
}

options(scipen = 99)
res <- run_steps(polymer, pairs, n = 40)
res <- table_letters(res)
print(max(res) - min(res))

