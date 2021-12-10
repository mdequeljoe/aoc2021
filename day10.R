
d <- readLines('data/day10.txt', warn = FALSE)
# d <- readLines('data/day10_example.txt', warn = FALSE)
d <- strsplit(d, "")

open_chars <- function()
  c("{", "<", "[", "(")

is_open <- function(x) x %in% open_chars()

close_chars <- function()
  c("}", ">", "]", ")")

char_key <- function()
  setNames(close_chars(), open_chars())

is_valid <- function(o, k, key){
  key[o] == k
}

check_line <- function(l){
  stopifnot(is_open(l[1]))
  key <- char_key()
  st  <- l[1]
  for (k in l[-1]){
    if (is_open(k)){
      st <- c(st, k)
      next()
    }
    n <- length(st)
    if (is_valid(st[n], k, key)) {
      st <- st[-n]
    } else {
      return(k)
    }
  }
}

val <- c(")" = 3, "]" = 57, "}" = 1197, ">" =  25137)
res <- lapply(d, check_line)
res <- unlist(res)
print( sum( val[res] ) )

# part two
score_line <- function(l){
  stopifnot(is_open(l[1]))
  val <- c(`)` = 1, `]` = 2, `}` = 3, `>` = 4)
  key <- char_key()
  st  <- l[1]
  for (k in l[-1]){
    if (is_open(k)){
      st <- c(st, k)
      next()
    }
    n <- length(st)
    if (is_valid(st[n], k, key)) {
      st <- st[-n]
    } else {
      return()
    }
  }
  
  s <- 0
  for (ochar in rev(st)){
    s     <- s * 5
    cchar <- key[ochar]
    s     <- s + val[[cchar]]
  }
  s
}

res <- lapply(d, score_line)
res <- sort(unlist(res))
mp  <- ceiling(length(res) / 2)
print( res[mp] )

