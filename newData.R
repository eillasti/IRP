library(data.table)
library(dplyr)
library(zoo)
spot = fread("../data/newData/spot.csv")
NZD = fread("../data/newData/NZD.csv")
spot = rbind(spot, NZD[ticker == "NZD Curncy"])

spot[, date := as.Date(date, "%Y-%m-%d")]
spot[is.na(px_last) & (!is.na(px_bid) | !is.na(px_ask)), px_last := ifelse(is.na(px_bid), px_ask, ifelse(is.na(px_ask), px_bid, (px_ask + px_bid)/2))]
spot[, asset := substr(ticker, 1, 3)]
spot[, spot := px_last]
setkey(spot, asset, date)

fwd = fread("../data/newData/fwd.csv")
fwd = rbind(fwd, NZD[ticker != "NZD Curncy"])
fwd[, date := as.Date(date, "%Y-%m-%d")]
fwd[is.na(px_last) & (!is.na(px_bid) | !is.na(px_ask)), px_last := ifelse(is.na(px_bid), px_ask, ifelse(is.na(px_ask), px_bid, (px_ask + px_bid)/2))]
fwd[, asset := substr(ticker, 1, 3)]
fwd[, maturity := substr(ticker, 4, 5)]

spotReturns = spot[!is.na(px_last), spot, keyby = c("asset", "date")]
spotReturns[, rx := log(spot) - stats::lag(log(spot)), by = asset]
spotReturns = spotReturns[!is.na(rx)]

fwd1m = fwd[maturity == "1M", list(fwd = px_last), c("asset", "date")]
fwd1m

monthlyReturns = merge(spot[, list(asset, date, spot)], fwd1m, by = c("asset", "date"))
monthlyReturns[, date := as.yearmon(date)]
monthlyReturns = monthlyReturns[, list(spot = spot[1], fwd = fwd[1]), keyby = c("asset", "date")]
monthlyReturns[, rx := log(spot) - stats::lag(log(fwd)), by = asset]
monthlyReturns = monthlyReturns[!is.na(rx)]
monthlyReturns

monthlyReturns[asset == "AUD"]
monthlyReturns[, mean(rx * 12), by = asset]

#volatilities
vols = fread("../data/newData/vols.csv")
vols[, date := as.Date(date, "%Y-%m-%d")]
vols[, asset := substr(ticker, 1,3)]
vols[asset == "USD", asset := substr(ticker, 4, 6)]
vols[grepl("V",substr(ticker, 7, 12)), strike := "ITM"]
vols[grepl("25B",substr(ticker, 7, 12)), strike := "25B"]
vols[grepl("25R",substr(ticker, 7, 12)), strike := "25R"]
vols[grepl("10B",substr(ticker, 7, 12)), strike := "10B"]
vols[grepl("10R",substr(ticker, 7, 12)), strike := "10R"]
vols[is.na(strike)]

vols[grepl("ON",substr(ticker, 7, 12)), maturity := "ON"]
vols[grepl("1W",substr(ticker, 7, 12)), maturity := "1W"]
vols[grepl("1M",substr(ticker, 7, 12)), maturity := "1M"]
vols[grepl("3M",substr(ticker, 7, 12)), maturity := "3M"]
vols[grepl("6M",substr(ticker, 7, 12)), maturity := "6M"]
vols[grepl("9M",substr(ticker, 7, 12)), maturity := "9M"]
vols[grepl("1Y",substr(ticker, 7, 12)), maturity := "1Y"]
vols[grepl("2Y",substr(ticker, 7, 12)), maturity := "2Y"]
vols[grepl("5Y",substr(ticker, 7, 12)), maturity := "5Y"]
vols[is.na(maturity)]

vols[is.na(px_last) & (!is.na(px_bid) | !is.na(px_ask)), px_last := ifelse(is.na(px_bid), px_ask, ifelse(is.na(px_ask), px_bid, (px_ask + px_bid)/2))]

# vols[, dVol := ]

volFX = vols[strike == "ITM" & maturity == "1M", list(vol = mean(px_last, na.rm = TRUE)), keyby = date]
volFX
plot(volFX$date, volFX$vol, type = "l")

  
