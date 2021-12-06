
d <- readLines('data/day02.txt', warn = FALSE)
d <- strsplit(d, ' ')
pos <- list(h = 0, d = 0)
for (k in d){
  v <- as.numeric(k[2])
  if (k[1] == 'forward'){
    pos$h <- pos$h + v
    next()
  }
  if (k[1] == 'backward'){
    pos$h <- pos$h - v
    next()
  }
  if (k[1] == 'up'){
    pos$d <- pos$d - v
    next()
  }
  pos$d <- pos$d + v
}

print(pos$h * pos$d)

# part two
pos <- list(h = 0, d = 0, aim = 0)
for (k in d){
  v <- as.numeric(k[2])
  if (k[1] == 'forward'){
    pos$h <- pos$h + v
    pos$d <- pos$d + (pos$aim * v)
    next()
  }
  if (k[1] == 'backward'){
    pos$h <- pos$h - v
    next()
  }
  if (k[1] == 'up'){
    pos$aim <- pos$aim - v
    next()
  }
  pos$aim <- pos$aim + v
}

print(pos$h * pos$d)
