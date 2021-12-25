

# part one solved by hand:
# minimize largest costs 
# min B moves based on above
# min A based on aboves
'
min D moves: (that are possible)
(9 * 1000) 
(8 * 1000)
min C moves: (that are possible)
(5 * 100)
(5 * 100)

#############
#...........#
###A#C#B#A###
  #D#D#B#C#
  #########
2 + (6 * 10) + (5 * 10)
#############
#.B.B.....A.#
###A#C# # ###
  #D#D# #C#
  #########
(5 * 100) + (5 * 100)
#############
#.B.B.....A.#
###A# #C# ###
  #D#D#C# #
  #########
(8 * 1000)
#############
#.B.B.....A.#
###A# #C# ###
  #D# #C#D#
  #########
(3 * 10) + (4 * 10)
#############
#.........A.#
###A#B#C# ###
  #D#B#C#D#
  #########
2
#############
#.A.......A.#
### #B#C# ###
  #D#B#C#D#
  #########
(9 * 1000)
#############
#.A.......A.#
### #B#C#D###
  # #B#C#D#
  #########
9 + 2
#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########
' -> s

score <-
  (2 * 1)    + 
  (6 * 10)   + 
  (5 * 10)   + 
  (5 * 100)  + 
  (5 * 100)  + 
  (8 * 1000) + 
  (3 * 10)   + 
  (4 * 10)   + 
  (2 * 1)    + 
  (9 * 1000) +
  (11 * 1)
print(score)


# part two
'
#############
#...........#
###A#C#B#A###
  #D#C#B#A#
  #D#B#A#C#
  #D#D#B#C#
  #########
'

set_grid <- function(d){
  d <- strsplit(d, '')
  l <- lengths(d)[1]
  d <- lapply(d, function(v){
    if (length(v) != l){
      v <- c(v, rep(' ', l - length(v)))
    }
    v
  })
  matrix(unlist(d), length(d) ,lengths(d)[1], byrow = TRUE)
}

catm <- function(m){
  m <- apply(m, 1, paste, collapse = '')
  m <- paste(m, collapse = '\n')
  cat(m, '\n')
}

.h <- digest::getVDigest()

ll <- function(){
  node <- function(l, next_ = NULL){
    self <- new.env()
    self$value <- l
    self$next_ <- next_
    self
  }
  self <- new.env()
  self$head <- NULL
  self$push <- function(l){
    nd <- node(l)
    if (is.null(self$head)){
      nd$next_ <- self$head
      self$head <- nd
      return(invisible())
    }
    prev <- tmp <- self$head
    repeat{
      if (is.null(tmp)){
        prev$next_ <- nd
        break()
      }
      if (l$score < tmp$value$score){
        if (identical(tmp, self$head)){
          nd$next_ <- self$head
          self$head <- nd
          break()
        }
        nd$next_ <- tmp
        prev$next_ <- nd
        break()
      }
      prev <- tmp
      tmp  <- tmp$next_
    }
  }
  self$pop <- function(){
    h <- self$head
    self$head <- h$next_
    h$value
  }
  self$is_empty <- function(){
    is.null(self$head)
  }
  self
}

sort_pods2 <- function(m){
  
  pods   <- LETTERS[1:4]
  energy <- setNames(c(1, 10, 100, 1000), pods)
  slot_a <- list(c(3, 4), c(4, 4), c(5, 4), c(6, 4))
  slot_b <- lapply(slot_a, `+`, c(0, 2))
  slot_c <- lapply(slot_b, `+`, c(0, 2))
  slot_d <- lapply(slot_c, `+`, c(0, 2))
  
  home <- setNames(list(slot_a, slot_b, slot_c, slot_d), pods)
  
  hallway <- list(
    c(2, 2), 
    c(2, 3),
    c(2, 5),
    c(2, 7),
    c(2, 9), 
    c(2, 11),
    c(2, 12)
  )
  
  val <- function(m, k) m[k[1], k[2]]

  sorted <- function(m){
    sa <- all(m[3:6, 4] == 'A')
    if (!sa)
      return(FALSE)
    sb <- all(m[3:6, 6] == 'B')
    if (!sb)
      return(FALSE)
    sc <- all(m[3:6, 8] == 'C')
    if (!sc)
      return(FALSE)
    all(m[3:6, 10] == 'D')
  }
  
  home_vals <- function(m, p, home)
    vapply(home[[p]], val, character(1), m = m)
  
  home_is_open <- function(m, p, home){
    all(home_vals(m, p, home) %in% c(' ', p))
  }
  
  locate_pod <- function(m, p){
    id <- which(m == p, TRUE)
    lapply(1:nrow(id), function(i){
      as.numeric( c(id[i, 1][[1]], id[i, 2][[1]]) )
    })
  }
  
  movable_pods <- function(m, pods, score) {
    res <- list()
    for (p in pods) {
      id <- locate_pod(m, p)
      for (coord in id) {
        k <- list(coord = coord, val = p, score = score)
        if (list(k$coord) %in% home[[p]] && home_is_open(m, p, home))
          next()
        # is in hallway and end pod is open (empty or has same pods)
        if (list(k$coord) %in% hallway) {
          k$pos <- 'hallway'
          if (home_is_open(m, p, home))
            res[[length(res) + 1]] <- k
          next()
        }
        k$pos <- 'start'
        # is in starting pod and nothing above and 
        # nothing blocking the two first resting places
        pos_above <- k$coord + c(-1, 0)
        rest_spot1 <- c(2, k$coord[2] - 1)
        rest_spot2 <- c(2, k$coord[2] + 1)
        open_rest <- val(m, rest_spot1) == '.' || val(m, rest_spot2) == '.'
        if (val(m, pos_above) %in% c('.', ' ') && open_rest)
          res[[length(res) + 1]] <- k
      }
    }
    res
  }
  
  try_move <- function(m, start, end, start_type, end_type){
    # check if hallway is clear
    d <- 1
    if (start[2] > end[2])
      d <- -1
    col_rng <- (start[2] + d):end[2]
    v <- m[2, col_rng]
    if (!all(v == '.'))
      return()
    
    if (start_type == 'start' && end_type == 'start'){
      steps <- (start[1] - 2) + abs(start[2] - end[2]) + (end[1] - 2)
    } else {
      steps <- abs(start[1] - end[1]) + abs(start[2] - end[2])
    }
    
    sval <- val(m, start)
    score <- steps * energy[[sval]]
    m[start[1], start[2]] <- if (start_type == 'hallway') '.' else ' '
    m[end[1], end[2]] <- sval
    list(m = m, score = score)
  }
  
  possible_moves <- function(m, mp){
    res <- list()
    for (p in mp){
      # if in hallway possible moves are the home slots
      # otherwise possible moves are home slots and open hallway slots
      # if home slot is open always go there, dont check hallways
      if (home_is_open(m, p$val, home)) {
        for (coord in rev(home[[p$val]]))
          if (val(m, coord) == ' ')
            break()
        s <-
          try_move(m,
                   p$coord,
                   coord,
                   start_type = p$pos,
                   end_type = 'start')
        
        if (!is.null(s)){
          # if went directly home dont try any other moves
          if (p$pos == 'start')
            return(list(list(m = s$m, score = p$score + s$score)))
          res[[length(res) + 1]] <-
            list(m = s$m, score = p$score + s$score)
        }
      } else {
        if (p$pos == 'hallway')
          next()
        for (k in hallway){
          if (val(m, k) != '.')
            next()
          s <- try_move(m, p$coord, k, start_type = 'start', end_type = 'hallway')
          if (!is.null(s))
            res[[length(res) + 1]] <-
            list(m = s$m, score = p$score + s$score)
        }
      }
    }
    res
  }
  
  st <- ll()
  st$push(list(m = m, score = 0))
  seen <- logical(0)
  score <- Inf
  repeat{
    if (st$is_empty())
      break()
    x <- st$pop()
    
    nm <- .h(list(x$m))
    if (isTRUE(seen[nm]))
      next()
    seen[nm] <- TRUE

    # if (x$score %% 100 == 0)
    #   print(x$score)

    mp <- movable_pods(x$m, pods, x$score)
    moves <- possible_moves(x$m, mp)
    for (mv in moves){
      if (sorted(mv$m)){
        score <- min(score, mv$score)
        next()
      }
      st$push(mv)
    }
  }
  score
}

st <- Sys.time()
d <- readLines('data/day23.txt', warn = FALSE)
m <- set_grid(d)
m2 <- set_grid(c('  #D#C#B#A#  ', '  #D#B#A#C#  '))
m <- rbind(m[1:3, ], m2, m[4:nrow(m), ])
res <- sort_pods2(m)
print(res)
print(st)
print(Sys.time())

