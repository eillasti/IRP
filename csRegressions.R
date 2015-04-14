load("../Data/monthlyReturns.RData")
monthlyReturns

# monthlyReturns[, qrf := ntile(rf, 10), by = "date"]
# !(asset %in% c("KRW", "COP"))
avgReturns = monthlyReturns[, 
                            list(rx = mean(rx) * 12,
                                 rf = mean(rf)), by = "asset"]

dtReturnsAndFactors = merge(avgReturns, dtBetasConst, by = "asset")
dtReturnsAndFactors[, qrf := ntile(rf, 10)]
dtReturnsAndFactors = dtReturnsAndFactors[, list(rx = mean(rx), 
                                                 rVixFX = mean(rVixFX),
                                                 DOL = mean(DOL)), by = qrf]
# dtReturnsAndFactors = dtReturnsAndFactors[date > "1990-01-01"]
# dtReturnsAndFactors = dtReturnsAndFactors[asset != "TRY"]


mCS = lm(rx ~ MKT, dtReturnsAndFactors)
summary(mCS)
dtReturnsAndFactors[, rx_pred := predict(mCS, newdata = .SD)]
setkey(dtReturnsAndFactors, MFX)
with(dtReturnsAndFactors, {
  plot(MFX, rx)
  text(MFX, rx, asset)
  lines(MFX, rx_pred)
})

mCS = lm(rx ~ MFX , dtReturnsAndFactors)
summary(mCS)
dtReturnsAndFactors[, rx_pred := predict(mCS, newdata = .SD)]
setkey(dtReturnsAndFactors, rVixFX)
with(dtReturnsAndFactors, {
  plot(rVixFX, rx)
  text(rVixFX, rx, qrf)
  lines(rVixFX, rx_pred)
})

#####################
#Monthly kernel betas
#####################
# plot(dtBetasKernelMonthly[asset == "AUD", MKT * 252])

dtBetasKernelMonthly = copy(dtBetasKernel)
dtBetasKernelMonthly[, date := as.yearmon(date)]
dtBetasKernelMonthly = dtBetasKernelMonthly[, list(MKT = mean(MKT),
                                                   rVixFX = mean(rVixFX),
                                                   CT = mean(CT),
                                                   DOL = mean(DOL)), 
                                            keyby = c("asset", "date")]
dtReturnsAndBetasKernel = merge(monthlyReturns, dtBetasKernelMonthly)
dtReturnsAndBetasKernel[, MKT := MKT * 252]
# dtReturnsAndBetasKernel[, rVixFX := MKT]
# dtReturnsAndBetasKernel[, MKT := MKT]

m = lm(rx ~ asset + asset : rVixFX + CT, data = dtReturnsAndBetasKernel)
summary(m)
library(ridge)

m1 = lm(rx * 12 ~rf, data = dtReturnsAndBetasKernel)
summary(m1)


library(MASS)
dtReturnsAndBetasKernel = dtReturnsAndBetasKernel[asset %in% c("EUR", "GBP", "CHF", "JPY", "NZD", "AUD", "CAD", "SEK", "NOK", "DKK")]


m = lm(rx ~ rVixFX - 1 + DOL, data = dtReturnsAndBetasKernel[abs(rVixFX) < 1])
summary(m)
plot(m)
anova(m)
# dtReturnsAndBetasKernel[, rx:= value]
mCS = gam(rx * 12 ~ s(rf, k = 30), data = dtReturnsAndBetasKernel[rf < 0.2])
summary(mCS)
plot(mCS)
anova(mCS)
dtReturnsAndBetasKernel[date > "2010-01-01", mean(rx)*12, by = "asset"]
# predict(mCS, data.table(DOL = 0, rVixFX = -1))

dtReturnsAndBetasKernel[, qrf := ntile(rf, 10), by = "date"]
table(dtReturnsAndBetasKernel$qrf, dtReturnsAndBetasKernel$asset)

dt1 = dtReturnsAndBetasKernel[asset == "NZD"]
m = gam(rx ~ (rVixFX) + s(rf, k = 20) + DOL, data = dt1)
plot(m)
summary(m)

portfolios = copy(dtReturnsAndBetasKernel)
# portfolios = portfolios[asset == "CHF"]
portfolios[, qrf := ntile(rf, 10), by = "date"]
# portfolios[, qV := ntile(rVixFX, 50)]
# avgCT = portfolios[, list(rx = mean(rx), 
#                           MKT = mean(MKT), 
#                           rVixFX = mean(rVixFX),
#                           DOL = mean(DOL)), 
#                    by = c("qrf", "date")]
avgCT = portfolios[, list(rx = mean(rx), 
                          MKT = mean(MKT), 
#                           rVixFX = mean(rVixFX),
                          CT = mean(CT),
                          DOL = mean(DOL)), 
                   by = c("qrf")]

avgCT = portfolios[, list(rx = mean(rx), 
                          MKT = mean(MKT), 
                                                    rVixFX = mean(rVixFX),
                          CT = mean(CT),
                          DOL = mean(DOL)), 
                   by = c("asset")]
# 
# avgCT = portfolios[, list(rx = mean(rx), 
#                           MKT = mean(MKT), 
#                           rVixFX = mean(rVixFX),
#                           DOL = mean(DOL)), 
#                    by = c("qV", "asset")]
# avgCT = portfolios[, list(rx = mean(rx), 
#                           MKT = mean(MKT), 
#                           rVixFX = mean(rVixFX),
#                           DOL = mean(DOL)), 
#                    keyby = c("qV")]


setkey(avgCT, CT)
mCS = lm(rx * 12 ~  rVixFX + DOL - 1, avgCT)
summary(mCS)


setkey(avgCT, rVixFX)
plot(avgCT$rVixFX, 12 * avgCT$rx)
lines(avgCT$rVixFX, predict(mCS, avgCT))
text(avgCT$rVixFX, 12 * avgCT$rx, avgCT$asset)

plot(avgCT$CT, 12 * avgCT$rx)
lines(avgCT$CT, predict(mCS, avgCT))
text(avgCT$CT, 12 * avgCT$rx, avgCT$asset)


#mimicking portfolio
dtReturnsT = data.table(cast(dailyReturns, date ~ asset, value = "rx"))
dtReturnsT = merge(dtReturnsT, dtFactors[, list(date, rVixFX)], by = "date")
dtReturnsT[, t1 := as.numeric(date)]
m = npscoef(rVixFX ~ AUD + JPY + CHF + NZD | t1, dtReturnsT, bws = 252)
summary(m)
mean(predict(m) * 252)
# dtReturnsAndBetasKernel1 = cast(dtReturnsAndBetasKernel, date ~ asset, value = "rx")
# dtMonthlyFactors = copy(dtFactors)
# dtMonthlyFactors[, date := as.yearmon(date)]
# dtMonthlyFactors = dtMonthlyFactors[, list(rVixFX = sum(rVixFX)), keyby = "date"]
# dtReturnsAndBetasKernel1 = merge(dtReturnsAndBetasKernel1, 
#                                  dtMonthlyFactors,
#                                  by = "date")
# dtReturnsAndBetasKernel1 = data.table(dtReturnsAndBetasKernel1)
# dtReturnsAndBetasKernel1[, t1 := as.numeric(date)]
# m = npscoef(rVixFX ~ AUD + JPY + CHF + NZD | t1, dtReturnsAndBetasKernel1)
# m = lm(rVixFX ~ NZD + JPY, dtReturnsAndBetasKernel1)
# summary(m)
# mean(predict(m))
# 
# summary(lm(NZD ~ rVixFX, dtReturnsAndBetasKernel1))
# summary(lm(rx ~ rVixFX, dtReturns[asset == "NZD"]))
# 
# ####Stocks
# avgReturns = dtPortfoliosFF[date > "1990-01-01", list(rx = mean(rx) * 252), by = "asset"]
# dtReturnsAndFactors = merge(avgReturns, dtBetasFFConst, by = c("asset"))

# avgReturns = monthlyReturns[, 
#                             list(rx = mean(rx) * 12,
#                                  rf = mean(rf)), by = "asset"]

mCS = lm(rx ~ rVixFX + DOL + MKT, dtReturnsAndFactors)
summary(mCS)
dtReturnsAndFactors[, rx_pred := predict(mCS, newdata = .SD)]
setkey(dtReturnsAndFactors, MKT)
with(dtReturnsAndFactors, {
  plot(MKT, rx)
#   text(rVixFX, rx, qrf)
  lines(MKT, rx_pred)
})

