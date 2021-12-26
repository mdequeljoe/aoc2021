

play <- function(p1, p2){
  pos    <- c(p1, p2)
  turns  <- 1:2
  scores <- c(0, 0)
  die    <- 1:3
  rolls  <- 0
  repeat{
    for (i in turns){
      rolls     <- rolls + 3
      pos[i]    <- (pos[i] + sum(die) - 1) %% 10 + 1
      scores[i] <- scores[i] + pos[i]
      if (scores[i] >= 1000)
        return(min(scores) * rolls)
      die <- (die + 3 - 1) %% 100 + 1
    }
  }
}

# print( play(4, 8) )

# Player 1 starting position: 6
# Player 2 starting position: 2
print( play(6, 2) )

# part two
play2 <- function(p1, p2){
  
  roll_table <- function(){
    res <- numeric()
    for (r1 in 1:3) 
      for (r2 in 1:3)
        for (r3 in 1:3)
          res <- c(res, r1 + r2 + r3)
    rt <- table(res)
    lapply(seq_along(rt), function(i){
      list(roll = as.numeric(names(rt)[i]), cnt = rt[[i]])
    })
  }
  
  s <- list(
    pos = c(p1, p2),
    player = 1,
    scores = c(0, 0)
  )
  
  wins <- c(0, 0)
  
  play_one_turn <- function(s, rt, m = 1) {
    i <- s$player
    for (r in rt){
      s_ <- s
      s_$pos[i] <- (s_$pos[i] + r$roll - 1) %% 10 + 1
      s_$scores[i] <- s_$scores[i] + s_$pos[i]
      if (s_$scores[i] >= 21) {
        wins[ i ] <<- wins[ i ] + m * r$cnt
        next()
      }
      s_$player <- (s_$player) %% 2 + 1
      play_one_turn(s_, rt, m * r$cnt)
    }
    
  }
  rt <- roll_table()
  play_one_turn(s, rt)
  wins
}

options(scipen = 99)
res <- play2(6, 2) 
print( max(res) )
