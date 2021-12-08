

d <- readLines('data/day08.txt', warn = FALSE)
# d <- readLines('data/day08_example.txt', warn = FALSE)
d <- strsplit(d, ' \\| ')

get_id <- function(d, id) {
  lapply(d, function(x) {
    strsplit(x[id], ' ')[[1]]
  })
}

o   <- get_id(d, 2)
ans <- lapply(o, nchar)
ans <- unlist(ans)
ans <- length(ans[ans %in% c(2, 3, 4, 7)])
print(ans)

# part two
digits <- list(
  `9` = c('A', 'B', 'C', 'D', 'F', 'G'),
  `8` = c('A', 'B', 'C', 'D', 'E', 'F', 'G'),
  `7` = c('A', 'C', 'F'),
  `6` = c('A', 'B', 'D', 'E', 'F', 'G'),
  `5` = c('A', 'B', 'D', 'F', 'G'),
  `4` = c('B', 'C', 'D', 'F'),
  `3` = c('A', 'C', 'D', 'F', 'G'),
  `2` = c('A', 'C', 'D', 'E', 'G'),
  `1` = c('C', 'F'),
  `0` = c('A', 'B', 'C', 'E', 'F', 'G')
)

decode <- function(d) {
  nk <- function(d, i)
    nchar(d) == i
  wires <- strsplit(d, '')

  w1 <- wires[ nk(d, 2) ][[1]]
  w7 <- wires[ nk(d, 3) ][[1]]
  w4 <- wires[ nk(d, 4) ][[1]]
  w8 <- wires[ nk(d, 7) ][[1]]
  
  slots <- setNames(vector('list', 7), LETTERS[1:7])
  slots[['C']] <- slots[['F']] <- intersect(w1, w7)
  slots[['A']] <- w7[!w7 %in% slots[['C']]]
  
  # find 9 from 4
  # all slots in 4 must be in 9
  w9 <- Filter(function(x) {
    all(w4 %in% x) && length(x) == 6
  }, wires)[[1]]

  # Then slot E is the letter in 8 not in 9
  slots[['E']] <- w8[!w8 %in% w9]
  
  # we also know that 0 must have both slots C F
  slots_cf <- unique(c(slots$C, slots$F))
  w0 <- Filter(function(x) {
    length(x) == 6 &&
      all(slots_cf %in% x) &&
      !identical(x, w9)
  }, wires)[[1]]
  
  # then we know 6
  w6 <- Filter(function(x) {
    length(x) == 6 && !identical(x, w9) && !identical(x, w0)
  }, wires)[[1]]

  # then only 5 digits remaining
  # 5 has all slots in 6 except E
  w5 <- Filter(function(x) {
    all(x %in% w6) && !slots[['E']] %in% x
  }, wires)[[1]]

  # all slots in 3 must be 9
  w3 <- Filter(function(x) {
    all(x %in% w9) && length(x) == 5 && !identical(x, w5)
  }, wires)[[1]]
  
  w2 <- setdiff(wires, list(w3, w5, w6, w0, w9, w8, w4, w7, w1))[[1]]

  # now determine remaining slots
  # slot c is in 2, slot f is not
  sc <- slots$C
  slots$C <- sc[sc %in% w2]
  slots$F <- slots$F[!slots$F %in% slots$C]
  
  # slot g
  sg <- Reduce(intersect, list(w0, w2, w3, w5, w6, w9))
  sg <- sg[!sg %in% unlist(slots)]
  slots$G <- sg
  
  # slot b
  sb <- Reduce(intersect, list(w0, w4, w5, w6, w8, w9))
  sb <- sb[!sb %in% unlist(slots)]
  slots$B <- sb
  
  # slot d
  slots$D <- setdiff(letters[1:7], unlist(slots))
  
  slots
}

get_signal <- function(o, slots, digits) {
  o <- strsplit(o, '')
  s <- setNames(names(slots), unlist(slots))
  res <- vapply(o, function(v){
    val <- s[ v ]
    ans <- Filter(function(d){
      all(d %in% val) && length(d) == length(val)
    }, digits)
    names(ans)
  }, character(1))
  
  as.numeric(paste(res, collapse = ''))
}

# example
# p <-
#   strsplit('acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab', ' ')[[1]]
# ans <- decode(p)
# o <- strsplit('cdfeb fcadb cdfeb cdbaf', ' ')[[1]]
# get_signal(o, ans, digits)

segments <- get_id(d, 1)
slots    <- lapply(segments, decode)
o        <- get_id(d, 2)

ans <- vapply(seq_along(o), function(i){
  get_signal(o[[i]], slots[[i]], digits)
}, numeric(1))

print( sum(ans) )

