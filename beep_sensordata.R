
rm(list = ls())

## Packages
library(xlsx)
library(httr)
library(jsonlite)
library(imputeTS)

## Gegevens voor API call, token: qlqPY9Lf8v3Bqff15ihl0lKWHKJKOvcmImeEfXePpvmmJrn3halP5p05DUIh
setwd("/home/sander/Documents/personal/beekeeping/beep/beep_codes")
bearer <- source("bearer.txt")
start <- .POSIXct(Sys.time(), "GMT") - 7*60*60*24
end <- .POSIXct(Sys.time(), "GMT")

req <- httr::GET("https://api.beep.nl/api/sensors/measurements",
                 httr::add_headers(
                 "Authorization" = paste("Bearer",  bearer$value)),
                 query = list(start = start, end = end,
                 weather = 1))

# req$status_code
cont <- rawToChar(req$content)
d <- jsonlite::fromJSON(cont)$measurements
rm(list = setdiff(ls(), "d"))

## Convert time variable to actual date/time format
d$time <- as.POSIXct(d$time, format="%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
d$time <- as.POSIXct(format(d$time, tz = "Europe/Amsterdam", usetz = TRUE))

## In case of missings, interpolate, but remove trailing NA's
while(is.na(d$weight_kg[length(d$weight_kg)])) {
  d <- d[-length(d$weight_kg), ]
}

if(sum(is.na(d$weight_kg)) > 0) {
  d$weight_kg <- na_interpolation(d$weight_kg)
}
  
## Weight sensor data, confined to this day
plot(d$time, d$weight_kg, type = "l",
     xlim = c(as.POSIXct(paste(Sys.Date(), "00:00:01 CEST", sep = " ")),
              as.POSIXct(paste(Sys.Date(), "23:59:59 CEST", sep = " "))),
     xlab = "Time of day", ylab = "Weight (kg)",
     xaxt = "n") # Day
vline <- as.POSIXct(paste(Sys.Date(), substr(as.character(max(d$time)), start = 11, stop = 18)))
abline(v = vline, col = "red", lty = 2)
axis.POSIXct(1, at = as.POSIXct(paste(Sys.Date(), "00:00:01 CEST")) + 
               0:12*60*120, labels = c(seq(0, 24, by = 2)))
day.before <- subset(d, d$time > as.POSIXct(paste(Sys.Date(), "00:00:00 CEST",
              sep = " ")) - 1*60*60*24 & d$time < as.POSIXct(paste(Sys.Date(), "23:59:59 CEST",
              sep = " ")) - 1*60*60*24)
day.before <- subset(day.before, day.before$time > vline - 1*60*60*24)
day.before$time <- day.before$time + 1*60*60*24
lines(day.before$time, day.before$weight_kg, col = "red")

## End of file.