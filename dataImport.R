library(data.table)
library(dplyr)
library(zoo)
library(reshape)
SR = function(x, n = 252) mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(n)
portfolioMean = function(rgeom){
  Rarithm = exp(rgeom)
  log(mean(Rarithm))
}

spot = fread("../data/newData/spot.csv")
NZD = fread("../data/newData/NZD.csv")
spot = rbind(spot, NZD[ticker == "NZD Curncy"])
spot = spot[ticker != "GHS Curncy"]

spot[, dupl := seq_along(px_last) > 1, by = c("ticker", "date")]
spot = spot[dupl != TRUE]
spot[, dupl := NULL]

spot[, date := as.Date(date, "%Y-%m-%d")]
spot[is.na(px_last) & (!is.na(px_bid) | !is.na(px_ask)), px_last := ifelse(is.na(px_bid), px_ask, ifelse(is.na(px_ask), px_bid, (px_ask + px_bid)/2))]
spot[, asset := substr(ticker, 1, 3)]
spot[asset %in% c("EUR", "GBP", "AUD", "NZD"), px_last := 1/px_last]
spot[, spot := 1 / px_last]
setkey(spot, asset, date)
# spot = spot[asset %in% c("EUR", "GBP", "CHF", "JPY", "NZD", "AUD", "CAD", "SEK", "NOK", "DKK")]

fwd = fread("../data/newData/fwd.csv")
fwd = rbind(fwd, NZD[ticker != "NZD Curncy"])
fwd[, date := as.Date(date, "%Y-%m-%d")]
fwd[, dupl := seq_along(px_last) > 1, by = c("ticker", "date")]
fwd = fwd[dupl != TRUE]
fwd[, dupl := NULL]
fwd[is.na(px_last) & (!is.na(px_bid) | !is.na(px_ask)), px_last := ifelse(is.na(px_bid), px_ask, ifelse(is.na(px_ask), px_bid, (px_ask + px_bid)/2))]
fwd[, asset := substr(ticker, 1, 3)]
fwd[, maturity := substr(ticker, 4, 5)]
fwd[asset %in% c("EUR", "GBP", "AUD", "NZD"), px_last := 1/px_last]
fwd[, fwd := 1/px_last]
