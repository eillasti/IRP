# #Carry-trade
# top10 = c("EUR", "GBP", "CHF", "JPY", "NZD", "AUD", "CAD", "SEK", "NOK", "DKK")
# dt1 = copy(monthlyReturns)
# key(dt1)
# dt1 = dt1[asset %in% top10]
# dt1[, qrf := paste0("CT",ntile(rf, 5)), by = "date"]
# 
# dt2 = data.table(cast(dt1, date ~ qrf, value = "rx", fun.aggregate = "mean"))
# dt2 = dt2[!is.na(CT1) & !is.na(CT5)]
# dt2[, SR(CT5 - CT1, 12)]
# # dt2[, SR(CT5 - CT1, 12)]

dt
#Momentum
top10 = c("EUR", "GBP", "CHF", "JPY", "NZD", "AUD", "CAD", "SEK", "NOK", "DKK")
dt1 = copy(monthlyReturns)

# dt1 = dt1[asset %in% top10]
setkey(dt1, asset, date)
key(dt1)
# dt1[, rx := rx]
dt1[, mom1 := ma(rx, 1), by = "asset"]
dt1[, mom3 := ma(rx, 3), by = "asset"]
dt1[, mom6 := ma(rx, 6), by = "asset"]
dt1[, mom12 := ma(rx, 12), by = "asset"]

dt1[, qmom1 := paste0("P",ntile(mom1, 5)), by = "date"]
dt1[, qmom3 := paste0("P",ntile(mom3, 5)), by = "date"]
dt1[, qmom6 := paste0("P",ntile(mom6, 5)), by = "date"]
dt1[, qmom12 := paste0("P",ntile(mom12, 5)), by = "date"]

dt1 = dt1[!is.na(mom6)]
dt1 = dt1[date >= "Jan 1989"]

dt1[, qrf := paste0("CT",ntile(rf, 5)), by = "date"]
# dt1[, portfolio := paste0("P",ntile(mom3, 5)), by = "date"]

# 
# library(MASS)
# date0 = as.yearmon("Jan 1993")
# m = lm(rx *12 ~ mom3 + mom6 + rf, data = dt1[date < date0])
# summary(m)
# # m = lm(rx  ~ mom3, data = dt1)
# dt1[, rx_pred := predict(m, newdata = .SD)]
# # dt1[, pos := rx_pred/sum(abs(rx_pred)), by = "date"]
# # dt1[, pos := pos - mean(pos), by = "date"]
# dt1[, pos := 0]
# # dt1[portfolio == "P1", pos := -1]
# # dt1[portfolio == "P5", pos := 1]
# dt1[, qrx_pred := ntile(rx_pred, 5), by = "date"]
# dt1[, N := 5L, by = "date"]
# dt1[date > date0 & qrx_pred == N, pos := 1]
# dt1[date > date0 & qrx_pred == 1, pos := -1]
# # dt1[is.na]
# dt3 = dt1[, list(rx = sum(pos * rx)), by = "date"]
# # dt3 = dt1[, list(rx = sum(pos * rx)), by = "date"]
# dt3[, crx := cumsum(rx)]
# plot(dt3[, list(date, crx)], type = "l")
# dt3[, SR(rx,12)]
# library(moments)
# skewness(dt3$rx)
# kurtosis(dt3$rx)
# # jarque.test(dt3$rx)

date0 = as.yearmon("Jan 1991")
date1 = date0
dt1[,pos := 0]
while(date1 < "Jan 2015"){
  m = lm(rx * 12 ~ mom1 + mom3 + mom6 + mom12 + rf + qrf, data = dt1[date < date1])
  summary(m)
  dt1[date > date1, rx_pred := predict(m, newdata = .SD)]
  dt1[date > date1, pos := 0]
  dt1[date > date1, qrx_pred := ntile(rx_pred, 5), by = "date"]
  dt1[date > date1, N := 5L, by = "date"]
  dt1[date > date1 & qrx_pred == N, pos := 1]
  dt1[date > date1 & qrx_pred == 1, pos := -1]
  date1 = date1 + 1
}
dt3 = dt1[, list(rx = sum(pos * rx)), by = "date"]
# dt3 = dt1[, list(rx = sum(pos * rx)), by = "date"]
dt3[, crx := cumsum(rx)]
plot(dt3[, list(date, crx)], type = "l")
dt3[date > date0, SR(rx,12)]
library(moments)
skewness(dt3$rx)
kurtosis(dt3$rx)

marketFX = dt1[, list(ym = date, pos = pos, asset)]

dailyReturns1 = copy(dailyReturns)
dailyReturns1[, ym := as.yearmon(date)]
dailyReturns1 = merge(dailyReturns1, marketFX, by = c("asset", "ym"))
dtMFX = dailyReturns1[, list(MFX = sum(pos * rx)), keyby = "date"]
dtMFX = dtMFX[MFX != 0]


# 
# dt2 = data.table(cast(dt1, date ~ portfolio, value = "rx", fun.aggregate = "mean"))
# dt2 = dt2[!is.na(P1) & !is.na(P5)]
# dt2[, P := P5 - P1]
# dt2[, cP := cumsum(P)]
# summary(dt2)
# plot(dt2[, list(date, cP)], type = "l")
# dt2[, SR(P, 12)]
# 
# dt2[is.na(P1) | is.na(P5)]
# 
# # dt2[, SR(CT5 - CT1, 12)]
# 
