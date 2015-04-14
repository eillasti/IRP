library(data.table)
library(dplyr)
library(zoo)
library(reshape)

### SPOT
spot = fread("../data/newData/spot.csv")
NZD = fread("../data/newData/NZD.csv")
spot = rbind(spot, NZD[ticker == "NZD Curncy"])
spot = spot[ticker != "GHS Curncy"]
spot = spot[ticker != "TRY Curncy"]

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
save(spot, file = "../Data/spot.RData")

### FWD
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
save(fwd, file = "../Data/fwd.RData")

### Implied dtIV
dtIV = fread("../data/newData/vols.csv")
dtIV[, date := as.Date(date, "%Y-%m-%d")]
dtIV[, asset := substr(ticker, 1,3)]
dtIV[asset == "USD", asset := substr(ticker, 4, 6)]
dtIV[grepl("V",substr(ticker, 7, 12)), strike := "ITM"]
dtIV[grepl("25B",substr(ticker, 7, 12)), strike := "25B"]
dtIV[grepl("25R",substr(ticker, 7, 12)), strike := "25R"]
dtIV[grepl("10B",substr(ticker, 7, 12)), strike := "10B"]
dtIV[grepl("10R",substr(ticker, 7, 12)), strike := "10R"]
dtIV[is.na(strike)]

dtIV[grepl("ON",substr(ticker, 7, 12)), maturity := "ON"]
dtIV[grepl("1W",substr(ticker, 7, 12)), maturity := "1W"]
dtIV[grepl("1M",substr(ticker, 7, 12)), maturity := "1M"]
dtIV[grepl("3M",substr(ticker, 7, 12)), maturity := "3M"]
dtIV[grepl("6M",substr(ticker, 7, 12)), maturity := "6M"]
dtIV[grepl("9M",substr(ticker, 7, 12)), maturity := "9M"]
dtIV[grepl("1Y",substr(ticker, 7, 12)), maturity := "1Y"]
dtIV[grepl("2Y",substr(ticker, 7, 12)), maturity := "2Y"]
dtIV[grepl("5Y",substr(ticker, 7, 12)), maturity := "5Y"]
dtIV[is.na(maturity)]

dtIV[is.na(px_last) & (!is.na(px_bid) | !is.na(px_ask)), px_last := ifelse(is.na(px_bid), px_ask, ifelse(is.na(px_ask), px_bid, (px_ask + px_bid)/2))]

save(dtIV, file = "../Data/dtIV.RData")

###vol indices
volFX = dtIV[strike == "ITM" & maturity == "1M", list(vol = mean(px_last, na.rm = TRUE) / 100), keyby = date]
volFX

vixFX = fread("../data/newData/JPM_FX_VIX.csv")  
vixFX[, date := as.Date(date, "%Y-%m-%d")]

vixSP = fread("../data/newData/VIX.csv")
vixSP[, date := as.Date(date, "%Y-%m-%d")]

dtVix = merge(vixSP[, list(date, vixSP = px_last/100)], 
            vixFX[, list(date, vixFX = px_last/100)],
            by = "date", all = TRUE)
dtVix = merge(dtVix,
            volFX[, list(date, ivFX = vol)],
            by = "date", all = TRUE)

save(dtVix, file = "../Data/dtVIX.RData")


###FF factors 
dtFF = fread("../data/FF.csv")
dtFF[, date := as.Date(as.character(Date), "%Y%m%d")]
dtFF = dtFF[, list(date, MKT = Mkt, SMB, HML)]
save(dtFF, file = "../Data/dtFF.RData")


###FF portfolios
dtPortfoliosFF = fread("../data/FF100_processed.csv", sep = ",")
dtPortfoliosFF[, Date := as.Date(Date)]
dtPortfoliosFF = dtPortfoliosFF[!is.na(Returns)]
dtPortfoliosFF = dtPortfoliosFF[, list(date = Date, 
                                       asset = paste0("Size", PortfolioSize, "Price", PortfolioPrice),
                                       rx = log(Returns+1))]
dtPortfoliosFF = dtPortfoliosFF[date > "1980-01-01"]
setkey(dtPortfoliosFF, asset, date)
save(dtPortfoliosFF, file = "../Data/dtPortfoliosFF.RData")
