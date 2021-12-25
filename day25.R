
set_grid <- function(d) {
  m <- strsplit(d, '')
  matrix(unlist(m),
         nrow = length(m),
         ncol = lengths(m)[1],
         byrow = TRUE)
}

move_east <- function(m){
  res <- m
  nk <- ncol(m)
  for (r in 1:nrow(m)){
    for (k in 1:nk){
      if (m[r, k] != '>')
        next()
      adj_col <- k %% nk + 1
      if (m[r, adj_col] == '.'){
        res[r, adj_col] <- '>'
        res[r, k] <- '.'
      }
    }
  }
  res
}

move_south <- function(m){
  res <- m
  nr <- nrow(m)
  for (r in 1:nr){
    for (k in 1:ncol(m)){
      if (m[r, k] != 'v')
        next()
      adj_row <- r %% nr + 1
      if (m[adj_row, k] == '.'){
        res[adj_row, k] <- 'v'
        res[r, k] <- '.'
      }
    }
  }
  res
}

d <- readLines('data/day25.txt', warn = FALSE)
# d <- readLines('data/day25_example.txt', warn = FALSE)

m <- set_grid(d)
cnt <- 0
repeat{
  m <- move_south(move_east(m_ <- m))
  cnt <- cnt + 1
  if (identical(m_, m))
    break()
}
print(cnt)
