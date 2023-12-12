input <-  read.table("day06.txt")

ways_to_win <- function(Time = 7, Distance = 9) {
  t <- seq(from = 1, to = (Time - 1),  by = 1)
  d <- rev(t)
  res <- t * d
  res <- length(res[res > Distance])
}

input <-  read.table("day06.txt")

duration <- input[1,2:5]
distance <- input[2,2:5]

time <-  c(duration$V2,duration$V3,duration$V4, duration$V5)
travel <- c(distance$V2,distance$V3,distance$V4, distance$V5)
res <- mapply(ways_to_win, time, travel) |> prod(res = _)

# part 2
newtime <- as.numeric(paste0(duration$V2,duration$V3,duration$V4, duration$V5))
newdist <- as.numeric(paste0(distance$V2,distance$V3,distance$V4, distance$V5))
res2 <- ways_to_win(newtime, newdist)
gc()

