
rm(list = ls())

## Packages
library(xlsx)
library(httr)
library(jsonlite)

## Gegevens voor API call, token: qlqPY9Lf8v3Bqff15ihl0lKWHKJKOvcmImeEfXePpvmmJrn3halP5p05DUIh
setwd("/home/sander/Documents/personal/beekeeping/beep/beep_codes")
bearer <- source("bearer.txt")
start <- Sys.time() - 7*60*60*24
end <- Sys.time()

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
d$time <- as.POSIXct(d$time, format="%Y-%m-%dT%H:%M:%SZ", tz = "CEST")

## Omit all data before April 14th, 16:15 (no hive on base)
d <- subset(d, d$time > "2023-04-14 16:15:00 CEST")

## Weight sensor data, confined to this day
plot(d$time, d$weight, type = "l",
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
day.before <- subset(day.before, day.before$time > vline - 1*60*60*24 - 15)
day.before$time <- day.before$time + 1*60*60*24
lines(day.before$time, day.before$weight_kg, col = "red")

## End of file.