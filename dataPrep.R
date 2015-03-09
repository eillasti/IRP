load("../Data/spot.RData")
load("../Data/fwd.RData")
SR = function(x, n = 252) mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(n)
portfolioMean = function(rgeom){
  Rarithm = exp(rgeom)
  log(mean(Rarithm))
}
fwd1m = fwd[maturity == "1M", list(fwd = fwd), c("asset", "date")]


###Monthly returns with accurate forward rates (more or less)
monthlyReturns = merge(spot[, list(asset, date, spot)], fwd1m, by = c("asset", "date"))
monthlyReturns[, date := as.yearmon(date)]
# monthlyReturns = monthlyReturns[!is.na(fwd)]
monthlyReturns = monthlyReturns[, list(spot = spot[1], fwd = fwd[1]), keyby = c("asset", "date")]
monthlyReturns[, rx := (log(spot) - stats::lag(log(fwd))), by = asset]
monthlyReturns[, rf := 12 * (log(spot) - log(fwd)), by = "asset"]
monthlyReturns = monthlyReturns[!is.na(rx)]
# monthlyReturns[, mean(rx) * (12), by = "asset"][, .SD, keyby = "V1"]
# monthlyReturns[asset == "AUD", mean(rf)]
monthlyReturns = monthlyReturns[, list(asset, date, rx, rf)]
save(monthlyReturns, file = "../Data/monthlyReturns.RData")

###Daily returns with inaccurate interest rates
dailyReturns = spot[!is.na(px_last), spot, keyby = c("asset", "date")]
dailyReturns[, rx := log(spot) - stats::lag(log(spot)), by = "asset"]
dailyReturns = merge(dailyReturns,  fwd1m)
dailyReturns[, rf := (log(spot) - log(fwd)) * 12]
dailyReturns[, rf := as.numeric(stats::filter(rf, rep(1/22,22), sides = 1)), by = "asset"]
dailyReturns[, rx := rx + as.numeric(date - stats::lag(date)) * rf / 365, by = "asset"]
dailyReturns = dailyReturns[!is.na(rx)]
dailyReturns = dailyReturns[, list(asset, date,  rx, rf)]
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

###Risk factors
dtFactors = merge(dtDOL, dtCT, by = "date", all = TRUE)
dtFactors = merge(dtFactors, dtVix, by = "date", all = TRUE)
dtFactors = merge(dtFactors, dtFF, by = "date", all = TRUE)

save(dtFactors, file = "../Data/dtFactors.RData")
