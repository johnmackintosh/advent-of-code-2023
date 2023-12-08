library(data.table)
path <- "day05.txt"

data <- readLines(path)
DT <- as.data.table(data)
setnames(DT, "data", "V1")
DT <- DT[,group := rleid(cumsum(grepl("^[[:alpha:]]+-+[[:alpha:]]+-[[:alpha:]]+\\s+map:",
                                V1)))][!V1 == ""][]

converter <- function(x){
  as.numeric(unlist(tstrsplit(x, split = " ")))
}

# convert seeds to numeric vector
seeds <- DT[group == 1, gsub("seeds: ", "", V1)]
seeds <- as.data.table(converter(seeds))
setkey(seeds, V1)


DT <- DT[group > 1]
DT <- DT[!grepl("^[[:alpha:]]+-+[[:alpha:]]+-[[:alpha:]]+\\s+map:",V1),
         tstrsplit(V1,split = " "), group][]

setnames(DT, old = c("V1", "V2", "V3"),
         new = c("dest_start", "source_start", "range_length"))

DT <- DT[,lapply(.SD, converter),
    .SDcols = c("dest_start", "source_start", "range_length"),
    .(group)
    ][,`:=`(dest_end = dest_start + (range_length - 1),
  source_end = source_start + (range_length - 1))]

setkey(DT, group,source_start, source_end)

seed_to_soil <- DT[group == 2]
soil_to_fertilizer <- DT[group == 3]
fertilizer_to_water    <- DT[group == 4]
water_to_light   <- DT[group == 5]
light_to_temperature   <- DT[group == 6]
temperature_to_humidity  <- DT[group == 7]
humidity_to_location   <- DT[group == 8]



find_matches <- function(input_dt, lookup_dt) {
 res <-  lookup_dt[input_dt,
            .(V1 = i.V1,
              dest_start = x.dest_start,
              source_start = x.source_start,
              source_end = x.source_end),
            on = .(source_start <= V1,
                   source_end >= V1)
  ][, dest := fcase(is.na(source_start), V1,
                    !is.na(source_start),V1 - source_start + dest_start)
  ][,.(V1 = dest)]
 return(res)

}


# part 1
t1 <- find_matches(seeds, seed_to_soil) |>
  find_matches(input_dt = _, soil_to_fertilizer) |>
  find_matches(input_dt = _,fertilizer_to_water) |>
 find_matches(input_dt = _,water_to_light) |>
 find_matches(input_dt = _,light_to_temperature) |>
 find_matches(input_dt = _,  temperature_to_humidity) |>
 find_matches(input_dt = _,  humidity_to_location)

  min(t1) #313045984
