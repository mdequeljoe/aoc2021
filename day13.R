
set_grid <- function(d) {
  d <- strsplit(d, ',')
  d <- lapply(d, function(x)
    as.numeric(x) + 1)
  nr <- Reduce(max, lapply(d, `[`, 2))
  nk <- Reduce(max, lapply(d, `[`, 1))
  m  <- matrix(0, nrow = nr, ncol = nk)
  for (k in d)
    m[k[2], k[1]] <- 1
  m
}

set_folds <- function(f) {
  rx = '.+(x|y)=([0-9]+)'
  lapply(f, function(x) {
    list(fold = sub(rx, '\\1', x), i = as.numeric(sub(rx, '\\2', x)) + 1)
  })
}

fold_grid <- function(m, i, axis = c('x', 'y')) {
  axis <- match.arg(axis)
  if (axis == 'x') {
    stopifnot(all(m[, i] == 0))
    a <- m[, 1:(i - 1)]
    for (k in (i + 1):ncol(m))
      a[, i - (k - i)] <-  a[, i - (k - i)] + m[, k]
    a[a > 1] <- 1
    return(a)
  }
  stopifnot(all(m[i, ] == 0))
  a <- m[1:(i - 1), ]
  for (r in (i + 1):nrow(m))
    a[i - (r - i),] <- a[i - (r - i),] + m[r,]
  a[a > 1] <- 1
  a
}

# d <- readLines('data/day13_example.txt', warn = FALSE)
# m <- set_grid(d)
# m <- fold_grid(m, 7, 'y')
# m <- fold_grid(m, 5, 'x')

d <- readLines('data/day13.txt', warn = FALSE)
b <- which(!nzchar(d))
f <- d[(b + 1):length(d)]
f <- set_folds(f)
d <- d[1:(b - 1)]
m <- set_grid(d)

res <- fold_grid(m, f[[1]]$i, f[[1]]$fold)
print(sum(res))

# part two
for (i in seq_along(f))
  m <- fold_grid(m, f[[i]]$i, f[[i]]$fold)

catm <- function(m) {
  m[m == 0] <- ' '
  m <- apply(m, 1, paste, collapse = '')
  m <- paste(m, collapse = '\n')
  cat(m, '\n')
}

catm(m)
# CAFJHZCK