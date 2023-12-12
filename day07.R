library(data.table)

#### functions ####
count_occurrences <- function(x, value){

res <-   lengths(regmatches(x, gregexpr(value,x)))
res
}

set_as_factor <- function(x){x <- factor(x,
                                         labels = c(1,2,3,4,5,6,7,8,9,
                                                    10,11,12,13,14),
                                         levels = c("1", "2", "3", "4", "5",
                                                    "6", "7", "8", "9", "T",
                                                    "J", "Q", "K", "A"),
                                         ordered = TRUE)}
#### end of functions ####


DT <- fread("day07.txt")
setnames(DT, new = c("hand", "bid"))
DT[,rowid := rleid(hand)][]

DT[, `:=`(A = count_occurrences(hand,"A"),
          K = count_occurrences(hand,"K"),
          Q = count_occurrences(hand,"Q"),
          J = count_occurrences(hand,"J"),
          `T` = count_occurrences(hand,"T"),
          `9` = count_occurrences(hand,"9") ,
          `8` = count_occurrences(hand,"8"),
          `7` = count_occurrences(hand,"7"),
          `6` = count_occurrences(hand,"6"),
          `5` = count_occurrences(hand,"5"),
          `4` = count_occurrences(hand,"4"),
          `3` = count_occurrences(hand,"3"),
          `2` = count_occurrences(hand,"2"))]

DT[, row_max := max(.SD),
   .SDcols = c("A", "K", "Q", "J",
               "T", "9", "8", "7",
               "6", "5", "4", "3", "2"), rowid][]


DT[,distinct_nos := sum(.SD != 0), .(hand, bid, rowid, row_max)]


DT <- DT[,tstrsplit(hand,""), .(hand, bid, rowid, row_max, distinct_nos)]

DT <- DT[,lapply(.SD, set_as_factor),
                 .SDcols = c("V1", "V2", "V3", "V4", "V5"),
                 .(hand, bid, rowid, row_max, distinct_nos)]

DT <- DT[, lapply(.SD, as.numeric),
       .SDcols = c("V1", "V2", "V3", "V4", "V5"),
       .(hand,bid,rowid, row_max, distinct_nos)]

DT <- setorder(DT, -row_max, distinct_nos, -V1, -V2, -V3, -V4, -V5)
DT[,rank := rev(rleid(hand))]
DT[,product := bid * rank]

sum(DT$product) # 250898830

# PART 2 - WIP
DT <- fread("day07.txt")
setnames(DT, new = c("hand", "bid"))
DT[,rowid := rleid(hand)][]

DT[, `:=`(A = count_occurrences(hand,"A"),
          K = count_occurrences(hand,"K"),
          Q = count_occurrences(hand,"Q"),
          J = count_occurrences(hand,"J"),
          `T` = count_occurrences(hand,"T"),
          `9` = count_occurrences(hand,"9") ,
          `8` = count_occurrences(hand,"8"),
          `7` = count_occurrences(hand,"7"),
          `6` = count_occurrences(hand,"6"),
          `5` = count_occurrences(hand,"5"),
          `4` = count_occurrences(hand,"4"),
          `3` = count_occurrences(hand,"3"),
          `2` = count_occurrences(hand,"2"))]

DT[, row_max := max(.SD),
   .SDcols = c("A", "K", "Q", "J",
               "T", "9", "8", "7",
               "6", "5", "4", "3", "2"), rowid][]


DT[,distinct_nos := sum(.SD != 0), .(hand, bid, rowid, row_max)]


return_letters <- function(DT, distinct =2, maxr = 4) {

  ans <- DT[J != 0 & distinct_nos == distinct & row_max == maxr,
            ][,lapply(strsplit(hand,""),unique)
              ][, lapply(.SD,FUN = gsub, pattern = "J",replacement = "")
                ][,lapply(.SD, max)]


ans2 <- length(ans)

ans2 <- melt(ans, measure.vars = 1:length(ans2))

return(ans2$value)
}
