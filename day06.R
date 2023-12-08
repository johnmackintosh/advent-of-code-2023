
# test cases
Time <- c(7, 15, 30)
Distance <- c(9, 40, 200)

# function
ways_to_win <- function(Time = 7, Distance = 9) {
  # no need to test 0 or max Distance
  t <- seq(from = 1, to = (Time - 1),  by = 1)
  # reverse t, as the time remaining descends as time held increases
  d <- rev(t)
  res <- t * d
  res <- length(res[res > Distance])
}

# check
# mapply
res <- mapply(ways_to_win, Time, Distance) |> prod(res = _)
prod(res)

# test with supplied data
duration <- c(51, 92, 68, 90)
distance <- c(222, 2031, 1126,  1225)
res <- mapply(ways_to_win, duration, distance) |> prod(res = _)

# part 2
newtime <- 51926890
newdist <- 222203111261225
res2 <- ways_to_win(newtime, newdist)
#gc()
