dt = fread("../Data/empiricalFinance.csv")

setnames(dt, c("Date", "AUD", "CAD", "CHF", "DEM", "GBP", "JPY", "NOK", "NZD", "SEK", "Sp", "Ty"))
dt1[, Date := as.Date(Date, "%m/%d/%Y")]
dt = dt[!is.na(AUD)]
dt1 = data.table(melt(dt))
setnames(dt1, c("Date", "portfolio", "rx"))
dt1 = merge(dt1, dt[, list(Date, Sp, Ty)], by = "Date")
# dt1 = dt1[ !(portfolio == "Sp" | portfolio == "Ty")]
dt1 = dt1[!(portfolio == "Ty")]


models = list("AUD", "CAD", "CHF", "DEM", "GBP", "JPY", "NOK", "NZD", "SEK", "Sp")#, "Ty")
names(models) = as.vector(models)

models = llply(models, function(crncy){
#   dt[, rx := dt[, crncy, with = FALSE]]
  dt2 = dt1[portfolio == crncy]
#   lm(rx ~ Sp + Ty, dt2)
  lm(rx ~ Sp, dt2)
})
  
summary(models[["AUD"]])

dt3 = dt1[, list(rx = mean(rx),
                 betaSp = models[[portfolio]]$coef["Sp"],
                 betaTy = models[[portfolio]]$coef["Ty"]
                 ), by = "portfolio"]
setkey(dt3, betaSp)

# lmCS = lm(rx * 252 ~ betaSp + betaTy, dt3)
library(MASS)
lmCS = lm(rx * 252 ~ betaSp , dt3)
summary(lmCS)

# dt1[, sd(Sp) * sqrt(252), by = "portfolio"]

plot(predict(lmCS), dt3$rx * 252, xlim = c(-0.03, +0.02), ylim =  c(-0.03, +0.02))
plot(predict(lmCS), dt3$rx * 252)
text(predict(lmCS), dt3$rx * 252, dt3$portfolio)
abline(0, 1)

plot(dt3$betaSp , dt3$rx * 252)
lines(dt3$betaSp, predict(lmCS))
lines(dt3$betaSp, predict(lmCS), col = "red")
text(dt3$betaSp , dt3$rx * 252, dt3$portfolio)
# 
# library(gmm)
# z = as.matrix(dt[, 2:10, with = FALSE])
# zm = as.matrix(dt[, 11:12, with = FALSE])
# 
# mGMM <- gmm(z ~ zm, x = zm)
# summary(mGMM)
# 
# library(car)
# R1 <- cbind(diag(9), matrix(0,9,9), matrix(0,9,9))
# c1 <- rep(0,9)
# linearHypothesis(mGMM, R1, c1, test = "Chisq")
