
parse_str <- function(s){
  f <- function(x, i = 1){
    res <- list()
    pr <- numeric(0)
    repeat{
      if (i > length(x))
        break()
      k <- x[i]
      if (k == ','){
        i <- i + 1
        if (x[i] == '[' && length(pr)){
          res <- c(res, list(pr))
          pr <- numeric(0)
        }
        next()
      }
      if (k == ']'){
        if (length(pr))
          res <- c(res, list(pr))
        return(list(res = res, i = i + 1))
      }
      if (k == '['){
        v <- f(x, i + 1)
        res <- c(res, list(v$res))
        i <- v$i
        next()
      }
      v <- character(0)
      repeat{
        if (!grepl('[0-9]', k))
          break()
        v <- c(v, k)
        i <- i + 1
        k <- x[i]
      }
      v <- as.numeric(paste(v, collapse = ''))
      pr <- c(pr, v)

    }
    list(res = res, i = i)
  }
  x <- strsplit(s, '')[[1]]
  unlist(f(x)$res, FALSE)
}

# s <- '[1,2]'
# parse_str(s)
# s <- '[[1,2],3]'
# parse_str(s)
# s <- '[9,[8,7]]'
# parse_str(s)
# s <- '[[[[1,2],[3,4]],[[5,6],[7,8]]],9]'
# res <- parse_str(s)
# str(res)
# s <- '[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]'
# res <- parse_str(s)
# str(res)
# s <- '[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]'
# res <- parse_str(s)
# str(res)

explode_pair <- function(l){
  pair_found <- FALSE
  add_left <- add_right <- TRUE
  pair <- numeric(0)
  exploded_depth <- 1
  last_x_cnt <- 0
  find_pair <- function(l, depth){
    l <- lapply(l, function(x){
      if (is.list(x)){
        res <- find_pair(x, depth + 1)
        r1 <- res[[1]]
        if (is.list(r1)){
          if (isTRUE(r1$exploded)){
            res <- res[[1]][[1]]
          }
        }
        res
      } else if (depth > 4 && !pair_found && length(x) == 2){
        pair <<- x
        pair_found <<- TRUE
        exploded_depth <<- depth
        list(0, exploded = TRUE)
      } else {
        if (!pair_found)
          last_x_cnt <<- last_x_cnt + length(x)
        if (pair_found && add_right){
          x[1] <- x[1] + pair[2]
          add_right <<- FALSE
        }
        x
      }
        
    })
    l
  }
  repair <- function(l){
    if (!pair_found)
      return(l)
    l <- lapply(l, function(x){
      if (is.list(x)){
        if (length(x) == 2 && 
            is.numeric(a <- x[[1]]) && 
            is.numeric(b <- x[[2]])){
          list(c(a, b))
        } else 
          repair(x)
      } else
        x
    })
    l
  }
  cnt <- 0
  find_left_add <- function(l){
    if (last_x_cnt == 0 || !pair_found)
      return(l)
    l <- lapply(l, function(x){
      if (is.list(x)){
        x <- find_left_add(x)
      } else {
        for (i in seq_along(x)){
          cnt <<- cnt + 1
          if (cnt == last_x_cnt && add_left){
            x[i] <- x[i] + pair[1]
            add_left <<- FALSE
          }
        }
      }
      x
    })
    l
  }
  
  l <- find_pair(l, 1)
  l <- repair(l)
  l <- find_left_add(l)
  l
}

s <- parse_str('[[[[[9,8],1],2],3],4]')
e <- explode_pair(s)
ans <- parse_str('[[[[0,9],2],3],4]')
stopifnot(identical(e, ans))

s <- parse_str('[7,[6,[5,[4,[3,2]]]]]')
e <- explode_pair(s)
ans <- parse_str('[7,[6,[5,[7,0]]]]')
stopifnot(identical(e, ans))

s <- parse_str('[[6,[5,[4,[3,2]]]],1]')
e <- explode_pair(s)
ans <- parse_str('[[6,[5,[7,0]]],3]')
stopifnot(identical(e, ans))

s <- parse_str('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]')
e <- explode_pair(s)
ans <- parse_str('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]')
stopifnot(identical(e, ans))

s <- parse_str('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]')
e <- explode_pair(s)
ans <- parse_str('[[3,[2,[8,0]]],[9,[5,[7,0]]]]')
stopifnot(identical(e, ans))

s <- parse_str('[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]')
e <- explode_pair(s)
ans <- parse_str('[[[[0,7],4],[[7,8],[6,0]]],[8,1]]')
stopifnot(identical(e, ans))

split_num <- function(l){
  find_split <- TRUE 
  flatten <- FALSE
  split <- function(l){
    l <- lapply(l, function(x){
      if (is.list(x)){
        if (find_split){
          x <- split(x)
          if (!find_split && flatten){
            x <- unlist(x, FALSE)
            flatten <<- FALSE
          }
        }
      } else {
        if (length(x) == 1 && x >= 10 && find_split){
          res <- c( floor(x / 2), ceiling(x / 2)  )
          x <- list(res)
          find_split <<- FALSE
        } else if (length(x) == 2 && find_split){
          if (x[1] >= 10){
            res <- c( floor(x[1] / 2), ceiling(x[1] / 2)  )
            x <- list(list(res), x[2])
            find_split <<- FALSE
            flatten <<- TRUE
          } else if (x[2] >= 10){
            res <- c( floor(x[2] / 2), ceiling(x[2] / 2)  )
            x <- list(x[1], list(res))
            find_split <<- FALSE
            flatten <<- TRUE
          }
          
        }
      }
      x
    })
    l
  }
  split(l)
}

s <- parse_str('[[[[4,3],4],4],[7,[[8,4],9]]]')
s2 <- parse_str('[1,1]')
s <- list(s,  s2)
ans <- parse_str('[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]')
stopifnot(identical(s, ans))

# explode
s <- explode_pair(s)
ans <- parse_str('[[[[0,7],4],[7,[[8,4],9]]],[1,1]]')
stopifnot(identical(s, ans))

# explode
e <- explode_pair(s)
ans <- parse_str('[[[[0,7],4],[15,[0,13]]],[1,1]]')
stopifnot(identical(e, ans))

sn <- split_num(e)
ans <- parse_str('[[[[0,7],4],[[7,8],[0,13]]],[1,1]]')
stopifnot(identical(sn, ans))


reduce_number <- function(res){
  funs <- list(explode = explode_pair, split = split_num)
  repeat{
    r <- res
    for (j in seq_along(funs)){
      r_ <- res
      res <- funs[[j]](res)
      if (!identical(res, r_))
        break()
    }
    if (identical(res, r))
      break()
  }
  res
}

sum_numbers <- function(d){
  res <- parse_str( d[1] )
  for (i in seq_along(d)[-1]){
    res <- list(res, parse_str( d[i] ))
    res <- reduce_number(res)
  }
  res
}

d <- strsplit('[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]', '\n')[[1]]
res <- sum_numbers(d)
ans <- parse_str('[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]')
stopifnot(identical(res, ans))

d <- c(
  '[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]',
  '[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]'
)
res <- sum_numbers(d)
ans <- parse_str('[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]')
stopifnot(identical(res, ans))

d <- c(
  '[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]',
  '[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]'
)
res <- sum_numbers(d)
ans <- parse_str('[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]')
stopifnot(identical(res, ans))

d <- c(
  '[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]',
  '[7,[5,[[3,8],[1,4]]]]'
)
res <- sum_numbers(d)
ans <- parse_str('[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]')
stopifnot(identical(res, ans))

d <- c(
  '[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]',
  '[[2,[2,2]],[8,[8,1]]]'
)
res <- sum_numbers(d)
ans <- parse_str('[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]')
stopifnot(identical(res, ans))

d <- c(
  '[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]',
  '[2,9]'
)
res <- sum_numbers(d)
ans <- parse_str('[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]')
stopifnot(identical(res, ans))

d <- c(
  '[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]',
  '[1,[[[9,3],9],[[9,0],[0,7]]]]'
)
res <- sum_numbers(d)
ans <- parse_str('[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]')
stopifnot(identical(res, ans))

d <- c(
  '[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]',
  '[[[5,[7,4]],7],1]'
)
res <- sum_numbers(d)
ans <- parse_str('[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]')
stopifnot(identical(res, ans))

d <- c(
  '[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]',
  '[[[[4,2],2],6],[8,7]]'
)
res <- sum_numbers(d)
ans <- parse_str('[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]')
stopifnot(identical(res, ans))


d <- strsplit('[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]', '\n')[[1]]
res <- sum_numbers(d)
ans <- parse_str('[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]')
stopifnot(identical(res, ans))

d <- strsplit('[[[[4,3],4],4],[7,[[8,4],9]]]
[1,1]', '\n')[[1]]
res <- sum_numbers(d)
ans <- parse_str('[[[[0,7],4],[[7,8],[6,0]]],[8,1]]')
stopifnot(identical(res, ans))

d <- strsplit('[1,1]
[2,2]
[3,3]
[4,4]', '\n')[[1]]
res <- sum_numbers(d)
ans <- parse_str('[[[[1,1],[2,2]],[3,3]],[4,4]]')
stopifnot(identical(res, ans))

d <- strsplit('[1,1]
[2,2]
[3,3]
[4,4]
[5,5]', '\n')[[1]]
res <- sum_numbers(d)
ans <- parse_str('[[[[3,0],[5,3]],[4,4]],[5,5]]')
stopifnot(identical(res, ans))

d <- strsplit('[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]', '\n')[[1]]

res <- sum_numbers(d)
ans <- parse_str('[[[[5,0],[7,4]],[5,5]],[6,6]]')
stopifnot(identical(res, ans))

d <- strsplit('[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]', '\n')[[1]]
res <- sum_numbers(d)
ans <- parse_str('[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]')
stopifnot(identical(res, ans))

calc_magnitude <- function(l){
  f <- function(l){
    lapply(l, function(x){
      if (is.list(x)){
        x <- calc_magnitude(x)[[1]]
      } else {
        if (length(x) > 1)
          x <- sum(x * c(3, 2))
      }
      x
    })
  }
  res <- f(l)
  if (length(res) == 2)
    sum( unlist(res) * c(3, 2) )
  else 
    res
}

s <- parse_str('[9,1]')
res <- calc_magnitude(s)
ans <- 29
stopifnot(res == ans)

s <- parse_str('[[9,1],[1,9]]')
res <- calc_magnitude(s)
ans <- 129
stopifnot(res == ans)

s <- parse_str('[[1,2],[[3,4],5]]')
res <- calc_magnitude(s)
ans <- 143
stopifnot(res == ans)

s <- parse_str('[[[[0,7],4],[[7,8],[6,0]]],[8,1]]')
res <- calc_magnitude(s)
ans <- 1384
stopifnot(res == ans)

s <- parse_str('[[[[1,1],[2,2]],[3,3]],[4,4]]')
res <- calc_magnitude(s)
ans <- 445
stopifnot(res == ans)

s <- parse_str('[[[[3,0],[5,3]],[4,4]],[5,5]]')
res <- calc_magnitude(s)
ans <- 791
stopifnot(res == ans)

s <- parse_str('[[[[5,0],[7,4]],[5,5]],[6,6]]')
res <- calc_magnitude(s)
ans <- 1137
stopifnot(res == ans)

s <- parse_str('[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]')
res <- calc_magnitude(s)
ans <- 3488
stopifnot(res == ans)

# part one
d <- readLines('data/day18.txt', warn = FALSE)
res <- sum_numbers(d)
print(calc_magnitude(res))

# part two
max_mag <- -Inf
for (i in seq_along(d)){
  x <- d[i]
  for (j in seq_along(d)[-i]){
    y <- d[j]
    res <- sum_numbers(c(x, y))
    mag <- calc_magnitude(res)
    max_mag <- max(max_mag, mag)
  }
}
print(max_mag)



