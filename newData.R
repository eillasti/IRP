library(data.table)
library(dplyr)
library(zoo)

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
spot[, spot := px_last]
setkey(spot, asset, date)

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

fwd1m = fwd[maturity == "1M", list(fwd = px_last), c("asset", "date")]

spotReturns = spot[!is.na(px_last), spot, keyby = c("asset", "date")]
spotReturns[, rx := log(spot) - stats::lag(log(spot)), by = asset]
spotReturns = spotReturns[!is.na(rx)]
spotReturns = merge(spotReturns,  fwd1m, all.x = TRUE)
spotReturns[, rf := (log(fwd) - log(spot)) * 12]
spotReturns[, t1 := as.numeric(date - min(date))]

# fwd[, ifelse(.N > 1, px_last[1] - px_last[2], 0), keyby = c("date", "ticker")][V1 != 0]
# fwd[ticker == "CAD1M Curncy" & date == "2015-02-27"]

monthlyReturns = merge(spot[, list(asset, date, spot)], fwd1m, by = c("asset", "date"))
monthlyReturns[, date := as.yearmon(date)]
monthlyReturns = monthlyReturns[, list(spot = spot[1], fwd = fwd[1]), keyby = c("asset", "date")]
monthlyReturns[, rx := log(spot) - stats::lag(log(fwd)), by = asset]
monthlyReturns = monthlyReturns[!is.na(rx)]
monthlyReturns

monthlyReturns[asset == "AUD"]
monthlyReturns[, mean(rx * 12), by = asset]

volFX = vols[strike == "ITM" & maturity == "1M", list(vol = mean(px_last, na.rm = TRUE) / 100), keyby = date]
volFX

vixFX = fread("../data/newData/JPM_FX_VIX.csv")  
vixFX[, date := as.Date(date, "%Y-%m-%d")]

vixSP = fread("../data/newData/VIX.csv")
vixSP[, date := as.Date(date, "%Y-%m-%d")]

plot(volFX$date, volFX$vol, type = "l", ylim = c(0, 0.5))
lines(vixFX$date, vixFX$px_last/100, col = "red")
lines(vixSP$date, vixSP$px_last/100/2, col = "green")

vix = merge(vixSP[, list(date, vixSP = px_last)], 
            vixFX[, list(date, vixFX = px_last)], 
            by = "date", all.x = TRUE)
vix[, rVixSP := HARres(vixSP)]
vix[, rVixFX := HARres(vixFX)]

m1 = lm(vixFX ~ vixSP, vix)
summary(m1)

m1 = lm(rVixFX ~ rVixSP, vix)
summary(m1)
vix[, rVixFXres := (rVixFX - predict(m1, .SD))]

carryTrade = copy(monthlyReturns)
carryTrade[, asset := paste0("CT_", ntile(log(fwd) - log(spot), 5)), by = "date"]
plot(carryTrade[, mean(rx) * 12, keyby= "asset"][[2]])


volBetas = merge(spotReturns, vix[, list(date, rVixFX, rVixSP, rVixFXres)])
# volBetas = volBetas[!is.na(rf)]
# volBetas[, qrf := ntile(rf, 5), by = "date"]
# unique(volBetas[!is.na(rf)]$asset)
# unique(volBetas$qrf)
# 
dt1 = volBetas[asset == "AUD"]
# dt1 = dt1[!is.na(rf) & !is.na(rVixSP)]
# dt1[is.na(rVixSP)]
# 
# table(dt1$qrf)
# 
# library(np)
# m = lm( rx ~ rVixSP + rVixSP : rf + rVixSP : factor(qrf) + rVixSP : t1, dt1)
# summary(m)
# anova(m)

volBetas[!is.na(rVixSP), betaVSP_t := {
  print(asset[1])
  bw = npscoef(rx ~  rVixSP | t1, data = .SD, bws = 200, betas = TRUE)
  bw$beta[, 2]
}, by = "asset"]

summary(bw)
plot(bw$beta[, 2])
plot(predict(bw, newdata = dt1[1:2000])/dt1$rVixSP[1:2000])
?npscoef
predict(bw, type = "terms")
fitted(bw)
# volBetas = volBetas[!is.na(rVixFX)]
# m = lm(rx ~ )

volBetas[, betaVSP := coef(lm(rx ~ rVixSP))[2], by = "asset"]
volBetas[, betaVFX := coef(lm(rx ~ rVixFX))[2], by = "asset"]
# volBetas1 = volBetas[, list(betaVFX = betaVFX[1], betaVSP = betaVSP[1]), keyby = "asset"]
volBetas[, month := as.yearmon(date)]
volBetas1 = volBetas[, list(betaVSP = mean(betaVSP_t, na.rm = TRUE)), keyby = c("asset", "month")]
volBetas1[, date := month]

assetAvgReturns = monthlyReturns[, list(rx = mean(rx) * 12), keyby = "asset"]
assetAvgReturns = merge(assetAvgReturns, volBetas)
# assetAvgReturns = assetAvgReturns[!is.na(betaVSP)]
# assetAvgReturns = assetAvgReturns[asset != "GHS"]
setkey(assetAvgReturns, betaVSP)

monthlyReturns1 = merge(monthlyReturns, volBetas1, by = c("asset", "date"))
monthlyReturns1 = monthlyReturns1[!is.na(betaVSP)]
assetAvgReturns = monthlyReturns1[, list(rx = mean(rx), betaVSP = mean(betaVSP)), by = "asset"]

plot(monthlyReturns1[asset == "JPY"]$betaVSP)

m = lm(rx ~ betaVSP, assetAvgReturns)
summary(m)
with(assetAvgReturns, {
  plot(betaVSP, rx)
  lines(betaVSP, predict(m))
  text(betaVSP, rx, asset)
})


m = lm(rx ~ betaVFX, assetAvgReturns)
summary(m)
with(assetAvgReturns, {
  plot(betaVFX, rx)
  lines(betaVFX, predict(m))
  text(betaVFX, rx, asset)
})


m = lm(rx ~ betaVSP, assetAvgReturns)
summary(m)
plot(assetAvgReturns$rx, predict(m))


with(assetAvgReturns, {
  plot(betaVSP, rx)
  lines(betaVSP, predict(m))
#   text(betaVSP, rx, asset)
})

