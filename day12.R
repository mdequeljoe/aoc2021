
set_caves <- function(d){
  d <- strsplit(d, '-')
  caves <- setNames(
    vapply(d, `[`, character(1), 2),
    nm <- vapply(d, `[`, character(1), 1)
  )
  rem   <- nm != 'start' & caves != 'end'
  caves <- c(caves, setNames(nm[rem], caves[rem]))
}

is_small_cave <- function(cave)
  cave == tolower(cave)

find_paths <- function(key){
  paths <- list()
  f <- function(key, el, path = character(0)){
    path  <- c(path, el)
    m     <- names(key) == el
    caves <- key[m]
    if (!length(caves))
      return()
    
    for (v in caves){
      if (v == 'end'){
        paths[[length(paths) + 1]] <<- c(path, v)
        next()
      }
      if (v %in% path && is_small_cave(v))
        next()
      f(key, v, path)
    }
  }
  f(key, 'start')
  paths
}

d <- readLines('data/day12.txt', warn = FALSE)
# d <- readLines('data/day12_example.txt', warn = FALSE)
# d <- readLines('data/day12_example2.txt', warn = FALSE)
# d <- readLines('data/day12_example3.txt', warn = FALSE)

d   <- set_caves(d)
res <- find_paths(d)
print( length(res) )

# part two
find_paths2 <- function(key){
  n_paths <- 0
  f <- function(key, el, path = character(0), allow_revisit = TRUE){
    path[length(path) + 1] <- el
    if (allow_revisit){
      sc            <- path[path == tolower(path)]
      allow_revisit <- anyDuplicated(sc) == 0
    }
    caves <- key[names(key) == el]
    if (!length(caves))
      return()
    for (v in caves){
      if (v == 'end'){
        n_paths <<- n_paths + 1
        next()
      }
      if (v == 'start')
        next()
      if (v %in% path && is_small_cave(v) && !allow_revisit)
        next()
      f(key, v, path, allow_revisit)
    }
  }
  f(key, 'start')
  n_paths
}

print( find_paths2(d) )
