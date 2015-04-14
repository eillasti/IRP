library(data.table)
library(zoo)
library(xts)
library(plyr)
library(reshape)

load("../Data/spot.RData")
load("../Data/fwd.RData")
SR = function(x, n = 252) mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(n)
portfolioMean = function(rgeom){
  Rarithm = exp(rgeom)
  log(mean(Rarithm))
}
fwd1m = fwd[maturity == "1M", list(fwd = fwd[1]), keyby = c("asset", "date")]


###Monthly returns with accurate forward rates (more or less)
monthlyReturns = merge(spot[, list(asset, date, spot)], fwd1m, by = c("asset", "date"))
# monthlyReturns[, day := date]
monthlyReturns[, date := as.yearmon(date)]
# monthlyReturns[, topq_rf]
#Find bad yearmons
# monthlyReturns = monthlyReturns[!is.na(fwd)]
monthlyReturns = monthlyReturns[, list(spot = tail(spot, n = 1), 
                                       fwd = tail(fwd, n = 1),
                                       rx = log(tail(spot,n = 1)) - log(fwd[1]),
                                       rf = 12 * (log(spot[1]) - log(fwd[1]))
                                       ), 
                                keyby = c("asset", "date")]
# monthlyReturns = monthlyReturns[, list(rx = ), 
#                                 by = c("asset", "date")]
# monthlyReturns[, rx := (log(spot) - stats::lag(log(fwd))), by = "asset"]
# monthlyReturns[, rf := 12 * (log(spot) - log(fwd)) * , by = "asset"]
# monthlyReturns = monthlyReturns[!is.na(rx)]
# monthlyReturns[, mean(rx) * (12), by = "asset"][, .SD, keyby = "V1"]
# monthlyReturns[asset == "CHF", mean(rx) * 12]
# monthlyReturns[asset == "AUD"]
plot(monthlyReturns[asset == "AUD", list(date, rf)], type = "l")
# monthlyReturns[asset == "AUD" & date > "2000-01-01", mean(rf)]
monthlyReturns = monthlyReturns[, list(asset, date, rx, rf)]
save(monthlyReturns, file = "../Data/monthlyReturns.RData")

# monthlyReturns[, ,eam]


###Daily returns with inaccurate interest rates
dailyReturns = spot[!is.na(px_last), spot, keyby = c("asset", "date")]
dailyReturns[, rx := log(spot) - stats::lag(log(spot)), by = "asset"]
dailyReturns = merge(dailyReturns,  fwd1m)
dailyReturns[, rf := (log(spot) - log(fwd)) * 12]
# dailyReturns[, rf := as.numeric(stats::filter(rf, rep(1/22,22), sides = 1)), by = "asset"]
# dailyReturns[, rf := c(rep(NA, 22), rf[23])]

dailyReturns[, rx := rx + as.numeric(date - stats::lag(date)) * rf / 252, by = "asset"]


# dailyReturns[, rx := rx + as.numeric(date - stats::lag(date)), by = "asset"]


dailyReturns = dailyReturns[!is.na(rx)]
dailyReturns = dailyReturns[, list(asset, date,  rx, rf)]
plot(dailyReturns[asset == "AUD", list(date, rf)], type = "l")
dailyReturns
save(dailyReturns, file = "../Data/dailyReturns.RData")


###Risk factors

###DOL
dtDOL = dailyReturns[, list(DOL = portfolioMean(rx)), by = "date"]

###Carry-trade portfolio risk factor
dtCT = copy(dailyReturns)
dtCT[, portfolio := paste0("CT", ntile(rf, 5)), by = "date"]
# dtCT[, SR(rx), by = portfolio]
# table(dtCT$asset, dtCT$portfolio)
dtCT = data.table(cast(dtCT, date ~ portfolio, value = "rx", fun.aggregate = portfolioMean))
dtCT = dtCT[!is.na(CT5)]
dtCT[, CT := log(exp(CT5) - exp(CT1) + 1)]
# SR(dtCT1$CT)
# SR(dtCT1$CT5 - dtCT1$CT1)
dtCT = dtCT[, list(date, CT)]

# plot(dtCT1$date, exp(cumsum(dtCT1$CT)), type = "l")

###Volatility indices
load("../Data/dtVix.RData")
source("HAR.R")
dtVix[!is.na(vixSP), rVixSP := HARres(vixSP)]
dtVix[!is.na(vixFX), rVixFX := HARres(vixFX)]
dtVix[!is.na(ivFX), rIvFX := HARres(ivFX)]

m1 = lm(vixFX ~ vixSP, dtVix)
summary(m1)

m1 = lm(vixFX ~ ivFX, dtVix)
summary(m1)

m1 = lm(rVixFX ~ rVixSP, dtVix)
summary(m1)
dtVix[, rVixFXres := (rVixFX - predict(m1, .SD))]

### FF factors
load("../Data/dtFF.RData")

### Momentum factor


###Risk factors
dtFactors = merge(dtDOL, dtCT, by = "date", all = TRUE)
dtFactors = merge(dtFactors, dtVix, by = "date", all = TRUE)
dtFactors = merge(dtFactors, dtFF, by = "date", all = TRUE)
# dtFactors = merge(dtFactors, dtMFX, by = "date", all = TRUE)

# dtFactors[, dVixFX := vixFX - stats::lag(vixFX)]
# 
# m = lm(rVixFX ~ dVixFX, dtFactors)
# summary(m)
save(dtFactors, file = "../Data/dtFactors.RData")
