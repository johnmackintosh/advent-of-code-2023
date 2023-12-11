library(data.table)
DT <- fread("day07.txt")
setnames(DT, new = c("hand", "bid"))
DT[,rowid := rleid(hand)][]

count_occurrences <- function(x, value){

res <-   lengths(regmatches(x, gregexpr(value,x)))
res
}

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
          `2` = count_occurrences(hand,"2")) ]

DT[, row_max := max(.SD), .SDcols = 4:16, rowid][]

DT[,distinct_nos := sum(.SD != 0), .(hand, bid, rowid, row_max)]


set_as_factor <- function(x){x <- factor(x,
                                         labels = c(1,2,3,4,5,6,7,8,9,
                                                    10,11,12,13,14),
                                         levels = c("1", "2", "3", "4", "5",
                                                   "6", "7", "8", "9", "T",
                                                   "J", "Q", "K", "A"),
                                         ordered = TRUE)}

lookup <- DT[,tstrsplit(hand,""), .(hand, bid, rowid, row_max, distinct_nos)]

lookup <- lookup[,lapply(.SD, set_as_factor),
                 .SDcols = c("V1", "V2", "V3", "V4", "V5"),
                 .(hand, bid, rowid, row_max, distinct_nos)]

lookup[, lapply(.SD, as.numeric),
       .SDcols = c("V1", "V2", "V3", "V4", "V5"),
       .(hand,bid,rowid, row_max, distinct_nos)]

res <- setorder(lookup, -row_max, distinct_nos, -V1, -V2, -V3, -V4, -V5)
res[,rank := rev(rleid(hand))]
res[,product := bid * rank]

# part 1
sum(res$product)
# 250898830
