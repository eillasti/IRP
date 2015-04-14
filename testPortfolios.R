dt1
testPortfolios = dt1[, list(asset, date, rx, CT = qrf, MOM = qmom3)]
dtMom = testPortfolios[, list(rx = mean(rx)), keyby = c("MOM", "date")]
setnames(dtMom, "MOM", "asset")
dtCT = testPortfolios[, list(rx = mean(rx)), keyby = c("CT", "date")]
setnames(dtCT, "CT", "asset")
testPortfolios = rbind(dtMom, dtCT)
setkey(testPortfolios, asset, date)
testPortfolios[, mean(rx * 12), by = asset]

dtFactorsMonthly= copy(dtFactors)
dtFactorsMonthly[, date := as.yearmon(date)]
dtFactorsMonthly = dtFactorsMonthly[, list(MKT = sum(MKT) / 100, MFX = sum(MFX)), by = "date"]
testPortfolios= merge(testPortfolios, dtFactorsMonthly, by = c("date"))
# testPortfolios[is.na(MFX)]
setkey(testPortfolios, asset, date)
testPortfolios= testPortfolios[!is.na(MKT) & !is.na(MFX)]

# dailyReturns
# testPortfoliosBetas = copy(dailyReturns)
# testPortfoliosBetas[, mom3 := ma(rx, 65), by = "asset"]
# testPortfoliosBetas[, qmom3 := paste0("P",ntile(mom3, 5)), by = "date"]
# testPortfoliosBetas[!is.na(mom3) & date > "1990-01-01", mean(rx * 252, na.rm = TRUE), by = "qmom3"]

m = lm(rx * 12~ -1 + asset + MKT : asset + MFX : asset, testPortfolios)
# m = lm(rx * 12~ -1 + asset + MKT : asset, testPortfolios)
# m = lm(rx * 12 ~ asset - 1 , testPortfolios)
summary(m)
anova(m)
plot(coef(m)[1:8])
plot(coef(m)[9:16], type = "p")


facts = c("MFX", "MKT")

dtBetasConst = testPortfolios[, list(fact = facts, beta = {
  #   dt = dtReturns[asset == "AUD"]
  SD = .SD[, c("rx", facts), with = FALSE]
  m = lm(rx ~ ., data = SD)
  as.numeric(coef(m)[-1])
}), by = "asset"]

testPortfolios[, t1 := as.numeric(date)]
# dtReturns = dtReturns[asset %in% c("EUR", "GBP", "CHF", "JPY", "NZD", "AUD", "CAD", "SEK", "NOK", "DKK")]
dtBetasKernel = ddply(testPortfolios[complete.cases(testPortfolios[, list(MKT, MFX)])], .(asset), function(dt){
  print(1)
  modelNp = npscoef(rx ~ MKT + MFX | t1, data = dt, betas = TRUE)
  data.table(coef(modelNp)[, -1])[, date := dt$date]
})
dtBetasKernel = data.table(dtBetasKernel)
setkey(dtBetasKernel, asset, date)

dtBetasConst = cast(dtBetasConst, asset ~ fact,  value = "beta")


testPortfoliosKernel = merge(testPortfolios[, list(asset, date, rx)], dtBetasKernel, by = c("asset", "date"))

testPortfoliosMean = testPortfolios[, list(rx = mean(rx) * 12), keyby = "asset"]

testPortfoliosMean = merge(testPortfoliosMean, dtBetasConst, by = "asset")

m = lm(rx~ MFX, testPortfoliosMean)
setkey(testPortfoliosMean, MFX)
summary(m)
plot(testPortfoliosMean[, list(MFX, rx)])
lines(testPortfoliosMean[, list(MFX, predict(m, newdata = .SD))])
text(testPortfoliosMean$MFX, testPortfoliosMean$rx, testPortfoliosMean$asset)


m = lm(rx * 12 ~ MKT, testPortfoliosMean)
setkey(testPortfoliosMean, MKT)
summary(m)
plot(testPortfoliosMean[, list(MKT, rx * 12)])
lines(testPortfoliosMean[, list(MKT, predict(m, newdata = .SD))])
text(testPortfoliosMean$MKT, testPortfoliosMean$rx * 12, testPortfoliosMean$asset)


m = lm(rx ~ -1 + MFX + MKT, testPortfoliosMean)
setkey(testPortfoliosMean, MKT)
summary(m)
coef(m) * c(12/100, 12)
plot(testPortfoliosMean[, list(MKT, rx)])
lines(testPortfoliosMean[, list(MKT, predict(m, newdata = .SD))])
text(testPortfoliosMean$MKT, testPortfoliosMean$rx, testPortfoliosMean$asset)

setkey(testPortfoliosMean, rx)
plot(testPortfoliosMean$rx,predict(m, newdata = testPortfoliosMean))
text(testPortfoliosMean$rx,predict(m, newdata = testPortfoliosMean),  testPortfoliosMean$asset)
abline(0,1)

plot(testPortfoliosMean$rx*12 - predict(m, newdata = testPortfoliosMean)*12,  type = "o")
text(testPortfoliosMean$rx*12 - predict(m, newdata = testPortfoliosMean)*12,  testPortfoliosMean$asset)


dtFactorsMonthly[!is.na(MKT) & !is.na(MFX)][, mean(MKT, na.rm = TRUE)*12/100]
dtFactorsMonthly[!is.na(MKT) & !is.na(MFX)][, mean(MFX, na.rm = TRUE)*12]



m = lm(rx ~ -1 + MFX + MKT, testPortfoliosKernel)
summary(m)
  
