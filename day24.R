
alu <- function(d, v){
  reg <- c(w = 0, x = 0, y = 0, z = 0)
  for (l in d){
    if (l[1] == 'inp'){
      stopifnot(length(v) > 0)
      reg[ l[2] ] <- v[1]
      v <- v[-1]
      # print(reg)
      next()
    }
    x <- reg[ l[2] ]
    y <- l[3]
    if (y %in% names(reg))
      y <- reg[ y ]
    else
      y <- as.numeric(y)
    f <- switch(l[1],
                `add` = `+`,
                `mul` = `*`,
                `div` = function(x, y) floor(x / y),
                `mod` = `%%`,
                `eql` = function(x, y) if (x == y) 1 else 0)
    reg[ l[2] ] <- f(x, y)
  }
  reg
}

translate_alu <- function(d){
  res <- 'w <- x <- y <- z <- 0'
  for (l in d){
    if (l[1] == 'inp'){
      res <- c(res, '# input')
      res <- c(res, 'w <- v[1]')
      res <- c(res, 'v <- v[-1]')
      next()
    }
    if (l[1] %in% c('add', 'mul', 'mod')){
      f <- switch(l[1], `add` = "+", `mul` = "*", `mod` = "%%")
      res <- c(res, sprintf('%s <- %s %s %s', l[2], l[2], f, l[3]))
      next()
    }
    if (l[1] == 'div'){
      res <- c(res, sprintf('%s <- floor(%s / %s)', l[2], l[2], l[3]))
      next()
    }
    res <- c(res, sprintf('%s <- if (%s == %s) 1 else 0', l[2], l[2], l[3]))
  }
  res <- c(res, 'c(w = w, x = x, y = y, z = z)')
  res <- paste(res, collapse = '\n')
  res
}

# d <- readLines('data/day24.txt', warn = FALSE)
# d <- strsplit(d, ' ')
# p <- translate_alu(d)
# cat(p)
# gives the following function:
f0 <- function(v){
  w <- x <- y <- z <- 0
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 1)
  x <- x + 13
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 6
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 1)
  x <- x + 11
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 11
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 1)
  x <- x + 12
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 5
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 1)
  x <- x + 10
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 6
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 1)
  x <- x + 14
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 8
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 26)
  x <- x + -1
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 14
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 1)
  x <- x + 14
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 9
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 26)
  x <- x + -16
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 4
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 26)
  x <- x + -8
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 7
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 1)
  x <- x + 12
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 13
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 26)
  x <- x + -16
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 11
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 26)
  x <- x + -13
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 11
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 26)
  x <- x + -6
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 6
  y <- y * x
  z <- z + y
  # input
  w <- v[1]
  v <- v[-1]
  x <- x * 0
  x <- x + z
  x <- x %% 26
  z <- floor(z / 26)
  x <- x + -6
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- y * 0
  y <- y + 25
  y <- y * x
  y <- y + 1
  z <- z * y
  y <- y * 0
  y <- y + w
  y <- y + 1
  y <- y * x
  z <- z + y
  c(w = w, x = x, y = y, z = z)
}

# refactor to:
f <- function(w, z, z_div, x_add, y_add){
  x <- z %% 26
  z <- floor(z / z_div)
  x <- x + x_add
  x <- if (x == w) 1 else 0
  x <- if (x == 0) 1 else 0
  y <- (25 * x) + 1
  z <- z * y
  y <- (w + y_add) * x
  z <- z + y
  c(w, z, x)
}

# hardcoded input program
p <- function(v){
  r <- f(v[1],   0,   1,   13, 6)
  r <- f(v[2],  r[2], 1,   11, 11)
  r <- f(v[3],  r[2], 1,   12, 5)
  r <- f(v[4],  r[2], 1,   10, 6)
  r <- f(v[5],  r[2], 1,   14, 8)
  r <- f(v[6],  r[2], 26,  -1, 14)
  r <- f(v[7],  r[2], 1,   14, 9)
  r <- f(v[8],  r[2], 26, -16, 4)
  r <- f(v[9],  r[2], 26,  -8, 7)
  r <- f(v[10], r[2], 1,   12, 13)
  r <- f(v[11], r[2], 26, -16, 11)
  r <- f(v[12], r[2], 26, -13, 11)
  r <- f(v[13], r[2], 26,  -6, 6)
  r <- f(v[14], r[2], 26,  -6, 1)
  r[2]
}

v <- c(1, 2, 4, 7, 9, 2, 4, 6, 2, 8, 3, 9, 2, 1)
stopifnot(p(v) == f0(v)['z'])

# find combinations where x is 0
message('checking first 6 digits...')
res <- list()
rng <- 1:9
for (i in rng)
  for (i2 in rng)
    for (i3 in rng)
      for (i4 in rng)
        for (i5 in rng)
          for (i6 in rng){
            r <- f(i,     0, 1,  13, 6)
            r <- f(i2, r[2], 1,  11, 11)
            r <- f(i3, r[2], 1,  12, 5)
            r <- f(i4, r[2], 1,  10, 6)
            r <- f(i5, r[2], 1,  14, 8)
            r <- f(i6, r[2], 26, -1, 14)
            if (r[3] == 0){
              v <- c(i, i2, i3, i4, i5, i6)
              res[[length(res) + 1]] <- list(v = v, r2 = r[2])
              # cat(v, r, r[2] %% 26, '\n') 
            }
          }

# 7-8
message('checking digits 7 and 8...')
res2 <- list()
for (l in res){
  for (i in 1:9){
    for (j in 1:9){
      r <- f(i, l$r2, 1,   14, 9)
      r <- f(j, r[2], 26, -16, 4)
      if (r[3] == 0){
        res2[[length(res2) + 1]] <- 
          list(v = c(l$v, i, j), r2 = r[2])
      }
    }
  }
}

#9 
message('checking digit 9...')
res3 <- list()
for (l in res2){
  for (i in 1:9){
    r <- f(i, l$r2, 26,  -8, 7)
    if (r[3] == 0){
      res3[[length(res3) + 1]] <- 
        list(v = c(l$v, i, j), r2 = r[2])
    }
  }
}

#10-11
message('checking digits 10 and 11...')
res4 <- list()
for (l in res3){
  for (i in 1:9){
    for (j in 1:9){
      r <- f(i, l$r2, 1,   12, 13)
      r <- f(j, r[2], 26, -16, 11)
      if (r[3] == 0){
        res4[[length(res4) + 1]] <- 
          list(v = c(l$v, i, j), r2 = r[2])
      }
    }
  }
}

#12-13
message('checking digits 12 and 13...')
res5 <- list()
for (l in res4){
  for (i in 1:9){
    for (j in 1:9){
      r <- f(i, l$r2, 26, -13, 11)
      r <- f(j, r[2], 26,  -6, 6)
      if (r[3] == 0){
        res5[[length(res5) + 1]] <- 
          list(v = c(l$v, i, j), r2 = r[2])
      }
    }
  }
}

# find where z = 0
#14
message('checking digit 14 (final digit)...')
res6 <- list()
for (l in res5){
  for (i in 1:9){
    r <- f(i, l$r2, 26,  -6, 1)
    if (r[2] == 0){
      res6[[length(res6) + 1]] <- 
        list(v = c(l$v, i), r2 = r[2])
    }
  }
}

#part one
ans <- res6[[length(res6)]]$v
print(paste(ans, collapse = ''))
#"949929927996199"

# part two
ans <- res6[[1]]$v
print(paste(ans, collapse = ''))
# "11931881141161"




