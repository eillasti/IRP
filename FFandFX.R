# dtFF[, Portfolio := paste0("S",PortfolioSize, "_P",PortfolioPrice)]
# # setnames(dtFF, "date", "Date")
# # dtFF[, Date := as.character(Date)]
# setnames(dtPortfoliosFX, "date", "Date")
# # setnames(dtPortfoliosFX, "Portfolios", "Portfolio")
# class(dtFF$Date)
# dtFX = merge(dtPortfoliosFX, dtFF[, list(SMB = SMB[1], HML = HML[1], MKT = MKT[1]), keyby = "Date"], by = "Date")

# dtFF1 = fread("../Data/FF.csv")
# dtFF1[, Date := as.Date(as.character(Date), format ="%Y%m%d")]
# setnames(dtFF1, "Mkt", "MKT")

dtSP500 = fread("../Data/SP500.csv")
dtSP500[, Date := as.Date(Date, "%m/%d/%Y")]
setkey(dtSP500, Date)
dtSP500[, SP500 := c(NA, log(Close[2:.N]) - log(Close[1:(.N-1)]))]

# install.packages("bit64")
library(bit64)
dtFX= merge(dtPortfoliosFX, dtFF1, by = "Date")
dtFX= merge(dtDatastream, dtFF1, by = "Date")
dtFX = merge(dtFX, dtSP500[, list(Date, SP500)],by = "Date")

dtCrncy = dtFX[substr(portfolio, 1, 3) != "Mom" & substr(portfolio, 1, 3) != "Car"]
# dtCrncy = dtFX[substr(portfolio, 1, 3) == "Car"]
# dtCrncy = dtCrncy[Date > "1995-01-01" & Date < "2009-01-01"]
dtCrncy[, ym := as.yearmon(Date)]
dtCrncy = dtCrncy[, list(
#   dVol = mean(dVol), 
                         rx = mean(rx), MKT = mean(MKT), SMB = mean(SMB), 
                         HML = mean(HML), SP500 = mean(SP500)), keyby = c("portfolio", "ym")]
dtCrncy[, date := ym]
setkey(dtCrncy, portfolio, date)


dtCrncy[, rx := exp(rx) - 1]

crncy = as.list(unique(dtCrncy$portfolio))
names(crncy) = as.vector(crncy)

library(plyr)
models = llply(crncy, function(crncy) {
  dt1 = dtCrncy[portfolio == crncy]
  #   lm(rx * 100~ dVol + SMB + HML + MKT, data = dt1[Date > "2004-01-01"])
  #   lm(rx ~ dVol + MKT, data = dt1)
#   lm(rx ~ dVol + SMB + HML + MKT, data = dt1)
#   lm(rx ~ MKT, data = dt1)
#   lm(rx ~ SP500, data = dt1)

lm(rx ~ SMB + HML + MKT, data = dt1)
# lm(rx ~ SMB + HML, data = dt1)
#   lm(rx ~ dVol, data = dt1)
})
# 
# m1 = lm(dVol ~ MKT, data = dtCrncy)
# summary(m1)
summary(models[["AUD"]])
# anova(models[["AUD"]])
# 
# models[["AUD"]]$coef["SMB"]

dtCrncyCS = dtCrncy[, list(rx = mean(rx, na.rm = TRUE),
                           betaVOL = models[[portfolio[1]]]$coef["dVol"],
                           betaSP500 = models[[portfolio[1]]]$coef["SP500"],
                           betaSMB = models[[portfolio[1]]]$coef["SMB"],
                           betaHML = models[[portfolio[1]]]$coef["HML"],
                           betaMKT = models[[portfolio[1]]]$coef["MKT"] 
), by = "portfolio"]
dtCrncyCS
# lmCS = lm(rx * 252 ~ -1 + betaHML, dtCrncyCS)
# lmCS = lm(rx * 252 ~ betaVOL, dtCrncyCS)

setkey(dtCrncyCS, betaMKT)
lmCS = lm(rx * 252 ~ betaMKT, dtCrncyCS)
# lmCS = lm(rx * 252 ~ betaSP500, dtCrncyCS)
# lmCS = lm(rx * 252 ~ betaSMB + betaHML, dtCrncyCS)
summary(lmCS)

# plot(dtCrncyCS$rx * 252, predict(lmCS), xlim = c(-0.02, +0.05), ylim =  c(-0.02, +0.05))
# text(dtCrncyCS$rx * 252, predict(lmCS), dtCrncyCS$portfolio)
# abline(0, 1)

plot(predict(lmCS), dtCrncyCS$rx * 252, xlim = c(-0.02, +0.05), ylim =  c(-0.02, +0.05))
text(predict(lmCS), dtCrncyCS$rx * 252, dtCrncyCS$portfolio)
abline(0, 1)


plot(dtCrncyCS$betaMKT * 252, dtCrncyCS$rx * 252)
lines(dtCrncyCS$betaMKT * 252, predict(lmCS))
text(dtCrncyCS$betaMKT * 252, dtCrncyCS$rx * 252, dtCrncyCS$portfolio)
# 
# plot(dtCrncyCS$betaVOL, dtCrncyCS$rx * 252)
# lines(dtCrncyCS$betaVOL, predict(lmCS))
# text(dtCrncyCS$betaVOL, dtCrncyCS$rx * 252, dtCrncyCS$portfolio)


dtCrncy[, mean(rx)* 252, by = "portfolio"]

summary(lmCS)
# anova(lmCS)
