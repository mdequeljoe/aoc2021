

str_to_bin <- function(str) {
  res <- lapply(strsplit(str, '')[[1]], BMS::hex2bin)
  unlist(res)
}

vec_to_int <- function(vec)
  strtoi(paste0(vec, collapse = ''), 2)

# https://stackoverflow.com/questions/13536832/strtoi-fails-to-convert-string-to-integer-returns-na
vec_to_int2 <- function(vec){
  sum( vec * 2^rev( (seq_along(vec) - 1)) )
}

parse_literal <- function(v, i) {
  val <- numeric(0)
  repeat {
    g <- v[i]
    val <- c(val, v[(i + 1):(i + 4)])
    i <- i + 5
    if (g == 0)
      break()
  }
  list(value = vec_to_int2(val), i = i)
}
# x <- str_to_bin('D2FE28')
# parse_literal(x[7:length(x)])

parse_packet <- function(x, i = 1) {
  s <- 0
  i0 <- i
  ver <- vec_to_int(x[i:(i + 2)])
  id  <- vec_to_int(x[(i + 3):(i + 5)])
  i <- i + 6
  id_type <- x[i]
  if (id == 4) {
    p <- parse_literal(x, i)
    s <- s + p$value
    i <- p$i
  } else if (id_type == 0) {
    i    <- i + 1
    len  <- vec_to_int(x[i:(i + 14)])
    i    <- i + 15
    slen <- i + len
    repeat {
      if (i >= slen)
        break()
      p <- parse_packet(x, i)
      s <- s + p$value
      ver <- ver + p$ver
      i <- p$i
    }
  } else {
    i <- i + 1
    np <- vec_to_int(x[i:(i + 10)])
    i <- i + 11
    for (j in seq_len(np)) {
      p <- parse_packet(x, i)
      s <- s + p$value
      ver <- ver + p$ver
      i <- p$i
    }
  }
  list(value = s, ver = ver, i = i)
}

# 
# # examples
# # 00111000000000000110111101000101001010010001001000000000
# # VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
# s <- str_to_bin('38006F45291200')
# x <-
#   strsplit('00111000000000000110111101000101001010010001001000000000',
#            '')[[1]]
# x <- as.numeric(x)
# print(parse_packet(x))
# 
# # 11101110000000001101010000001100100000100011000001100000
# # VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
# x <- str_to_bin('EE00D40C823060')
# print(parse_packet(x))
# 
# str <- '8A004A801A8002F478' 
# x   <- str_to_bin(str)
# print(parse_packet(x))
# 
# str <- '620080001611562C8802118E34'
# x   <- str_to_bin(str)
# print(parse_packet(x))
# 
# str <- 'C0015000016115A2E0802F182340'
# x   <- str_to_bin(str)
# print(parse_packet(x))
# 
# str <- 'A0016C880162017C3686B18A3D4780'
# x   <- str_to_bin(str)
# print(parse_packet(x))

d <- readLines('data/day16.txt', warn = FALSE)
x   <- str_to_bin(d)
print(parse_packet(x)$ver)

# part two
parse_packet2 <- function(x, i = 1) {
  s <- numeric(0)
  i0 <- i
  ver <- vec_to_int(x[i:(i + 2)])
  id  <- vec_to_int(x[(i + 3):(i + 5)])
  i <- i + 6
  id_type <- x[i]
  if (id == 4) {
    p <- parse_literal(x, i)
    s <- c(s, p$value)
    i <- p$i
  } else if (id_type == 0) {
    i    <- i + 1
    len  <- vec_to_int(x[i:(i + 14)])
    i    <- i + 15
    slen <- i + len
    repeat {
      if (i >= slen)
        break()
      p <- parse_packet2(x, i)
      s <- c(s, p$value)
      ver <- ver + p$ver
      i <- p$i
    }
  } else {
    i <- i + 1
    np <- vec_to_int(x[i:(i + 10)])
    i <- i + 11
    for (j in seq_len(np)) {
      p <- parse_packet2(x, i)
      s <- c(s, p$value)
      ver <- ver + p$ver
      i <- p$i
    }
  }

  f <- switch(as.character(id), 
              `0` = sum,
              `1` = function(...) Reduce(`*`, ...),
              `2` = min,
              `3` = max, 
              `5` = function(v) if (v[1] > v[2]) 1 else 0,
              `6` = function(v) if (v[1] < v[2]) 1 else 0,
              `7` = function(v) if (v[1] == v[2]) 1 else 0,
              `4` = function(v) v)
  s <- f(s)
  list(value = s, ver = ver, i = i)
}

# x <- str_to_bin('C200B40A82')
# parse_packet2(x)
# 
# x <- str_to_bin('04005AC33890')
# parse_packet2(x)
# 
# x <- str_to_bin('880086C3E88112')
# parse_packet2(x)
# 
# x <- str_to_bin('CE00C43D881120')
# parse_packet2(x)
# 
# x <- str_to_bin('D8005AC2A8F0')
# parse_packet2(x)
# 
# x <- str_to_bin('F600BC2D8F')
# parse_packet2(x)
# 
# x <- str_to_bin('9C005AC2F8F0')
# parse_packet2(x)
# 
# x <- str_to_bin('9C0141080250320F1802104A08')
# parse_packet2(x)


d <- readLines('data/day16.txt', warn = FALSE)
x <- str_to_bin(d)
print(parse_packet2(x)$value)

