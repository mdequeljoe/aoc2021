
d <- readLines('data/day03.txt', warn = FALSE)
d <- strsplit(d, '')

res <- lapply(1:lengths(d)[1], function(i){
  v <- unlist(lapply(d, `[`, i))
  vt <- table(v)
  names(vt)[vt == max(vt)]
})

g <- unlist(res)
e <- ifelse(g == '1', '0', '1')
g <- strtoi(paste(g, collapse = ''), base = 2)
e <- strtoi(paste(e, collapse = ''), base = 2)
print(g*e)

# part two
screen <- function(d, FUN = `max`, default = '1'){
  for (i in 1:lengths(d)[1]){
    v  <- vapply(d, `[`, character(1), i)
    vt <- table(v)
    b  <- names(vt)[vt == FUN(vt)]
    if (length(b) == 2)
      b <- default
    d <- d[v == b]
  }
  d <- paste(unlist(d), collapse = '')
  strtoi(d, base = 2)
}

oxygen <- screen(d)
co2    <- screen(d, `min`, '0')
print(oxygen * co2)

