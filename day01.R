
d <- readLines('data/day01.txt', warn = FALSE)
d <- as.numeric(d)
res <- diff(d)
print(length(res[res > 0]))

# part 2
res <- vapply(1:(length(d) - 2), function(i){
  sum(d[i:(i + 2)])
}, numeric(1))
res <- diff(res)
print(length(res[res > 0]))

# alternatively: (a + b + c) > (b + c + d) -> a > d
# res <- diff(d, 3)
# print(length(res[res > 0]))
