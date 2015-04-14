top10 = c("EUR", "GBP", "CHF", "JPY", "NZD", "AUD", "CAD", "SEK", "NOK", "DKK")

# big10 = copy(monthlyReturns1)
monthlyReturns1 = monthlyReturns[asset %in% top10]

monthlyReturns
setkey(monthlyReturns1, asset, date)
key(monthlyReturns1)
# monthlyReturns1[, rx := rx]
monthlyReturns1[, mom1 := ma(rx, 1), by = "asset"]
monthlyReturns1[, mom3 := ma(rx, 3), by = "asset"]
monthlyReturns1[, mom6 := ma(rx, 6), by = "asset"]
monthlyReturns1[, mom12 := ma(rx, 12), by = "asset"]

monthlyReturns1[, qmom1 := paste0("P",ntile(mom1, 5)), by = "date"]
monthlyReturns1[, qmom3 := paste0("P",ntile(mom3, 5)), by = "date"]
monthlyReturns1[, qmom6 := paste0("P",ntile(mom6, 5)), by = "date"]
monthlyReturns1[, qmom12 := paste0("P",ntile(mom12, 5)), by = "date"]

monthlyReturns1 = monthlyReturns1[!is.na(mom6)]
monthlyReturns1 = monthlyReturns1[date >= "Jan 1989"]

monthlyReturns1[, qrf := paste0("CT",ntile(rf, 5)), by = "date"]

dtFactors

# monthlyReturns1[, qrf := ntile(rf, 5), by = "date"]
# 
# library(ridge)
# ?ridge
# linearRidge
# 
# f = formula(rx * 12 ~ I((qrf == 1) * 1) + I((qrf == 5) * 1))
# modelEstimatorLm = function(dt){
#   f = formula(rx * 12 ~ mom1 + mom3 + mom6 + mom12 + rf + qrf)
# 
# #   f = formula(rx * 12 ~ rf)
#   lm(f, data = dt)
#   library(partykit)
#   ctree(rx ~ mom1 + mom3 + mom6 + mom12 + rf + qrf, data = data.frame(dt))
# #   linearRidge(f, data = as.data.frame(dt))
# }
# 
# m = lm(f, monthlyReturns1)
# m = linearRidge(f, monthlyReturns1)
# m = ctree(rx ~ mom1 + mom3 + mom6 + mom12 + rf + qrf, data = (monthlyReturns1))
# library(randomForest)
# m = randomForest(rx ~ mom1 + mom3 + mom6 + mom12 + rf + qrf + qmom1 + qmom3 + qmom6, data = (monthlyReturns1))
# m = ctree(rx ~ mom1 + mom3 + mom6 + mom12 + rf + qrf + qmom1 + qmom3 + qmom6, data = (monthlyReturns1))
# plot(m)
# monthlyReturns1[, rx_pred := predict(m, newdata = .SD)]
# summary(m)
# 
# 
# monthlyReturns1[, rx_pred := predictReturns(.SD, 1, modelEstimator = modelEstimatorLm)]
# monthlyReturns1[, w := portfolioWeights(.SD, q = 2)]
# # monthlyReturns1[, w := 0]
# # monthlyReturns1[, w := 1 * (rx_pred > 0)]
# # monthlyReturns1[is.na(w), w := 0]
# # monthlyReturns1[qrf == "CT1", w := -1]
# # monthlyReturns1[qrf == "CT5", w := 1]
# 
# 
# dtMarket = monthlyReturns1[, list(rx = sum(w * rx)), by = date]
# dtMarket[, crx := cumsum(rx)]
# plot(dtMarket$date, dtMarket$crx, type = "l")
# SR(dtMarket$rx, 12)
# 
