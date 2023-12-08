library(data.table)
path <- "day2.txt"
DT <- setDT(read.table(path, header = FALSE, sep = "\n"))

DT[, gameID := row(DT)
   ][,red := gsub("[^0-9,]", "", regmatches(V1, gregexpr("\\d+\\s+red\\b", V1)))
     ][,blue := gsub("[^0-9,]", "", regmatches(V1, gregexpr("\\d+\\s+blue\\b", V1)))
       ][,green :=  gsub("[^0-9,]", "", regmatches(V1, gregexpr("\\d+\\s+green\\b", V1)))
         ]

DT <- DT[, lapply(.SD, \(x) {max(as.numeric(unlist(tstrsplit(as.character(x), split = ","))))}),
         .SDcols = c("red","blue","green"),gameID][]

DT[red <= 12 & blue <= 14 & green <= 13,][,sum(gameID)] # 2377

DT[,sum(red * green * blue)][] # 71220
