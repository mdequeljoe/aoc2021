
d <- readLines('data/day04.txt', warn = FALSE)
# d <- readLines('data/day04_example.txt', warn = FALSE)

set_cards <- function(x){
  as.numeric(strsplit(x, ',')[[1]])
}

set_boards <- function(x){
  x <- x[nzchar(x)]
  x <- split(x, ceiling(seq_along(x) / 5))
  x <- lapply(x, function(m){
    m <- strsplit(m, ' ')
    m <- lapply(m, function(r){
      as.numeric(r[nzchar(r)])
    })
    matrix(unlist(m), nrow = 5, ncol = 5, byrow = TRUE)
  })
  x
}

check_boards <- function(cards, boards, n){
  if (n < 5)
    return()
  k <- cards[seq_len(n)]
  res <- lapply(seq_along(boards), function(i){
    b <- boards[[i]]
    rows <- apply(b, 1, function(row) all(row %in% k))
    cols <- apply(b, 2, function(col) all(col %in% k))
    if (any(rows) || any(cols))
      i
  })
  unlist(res)
}

calc_score <- function(cards, boards, i, n){
  b <- boards[[i]]
  k <- cards[seq_len(n)]
  sum(b[!b %in% k]) * k[length(k)]
}

get_winner <- function(cards, boards, n = 1) {
  len <- length(cards)
  repeat {
    winner <- check_boards(cards, boards, n)
    if (!is.null(winner))
      return(list(winner = winner, n = n))
    if (n == len)
      return()
    n <- n + 1
  }
}

cards  <- set_cards(d[1])
boards <- set_boards(d[3:length(d)])
w      <- get_winner(cards, boards)
print( calc_score(cards, boards, w$winner, w$n) )

# part two
n <- 1
repeat{
  w <- get_winner(cards, boards, n)
  if (length(boards) == 1 && !is.null(w$winner)){
    print(calc_score(cards, boards, 1, w$n))
    break()
  }
  boards <- boards[-w$winner]
  n      <- w$n + 1
}

