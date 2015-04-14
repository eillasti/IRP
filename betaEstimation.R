load("../Data/dtFactors.RData")
load("../Data/dailyReturns.RData")

dtFactors
dailyReturns

dtReturns = merge(dailyReturns, dtFactors, by = "date", all.x = TRUE, all.y = FALSE)
setkey(dtReturns, asset, date)
# facts = c("DOL", "rVixFX")
facts = c("MFX", "MKT")

#################
###Constant betas
#################
dtBetasConst = dtReturns[, list(fact = facts,beta = {
  #   dt = dtReturns[asset == "AUD"]
  SD = .SD[, c("rx", facts), with = FALSE]
  m = lm(rx ~ ., data = SD)
  as.numeric(coef(m)[-1])
}), by = "asset"]

dtBetasConst = cast(dtBetasConst, asset ~ fact,  value = "beta")


#################
####Time-varying betas
#################

library(plyr)
dtReturns[, t1 := as.numeric(date)]
# dtReturns = dtReturns[asset %in% c("EUR", "GBP", "CHF", "JPY", "NZD", "AUD", "CAD", "SEK", "NOK", "DKK")]
dtBetasKernel = ddply(dtReturns[complete.cases(dtReturns[, list(MKT, MFX)])], .(asset), function(dt){
  print(1)
  modelNp = npscoef(rx ~ MKT + MFX | t1, data = dt, bws = 252, betas = TRUE)
  data.table(coef(modelNp)[, -1])[, date := dt$date]
})
dtBetasKernel = data.table(dtBetasKernel)
setkey(dtBetasKernel, asset, date)

# save(dtBetasKernel, file = "../Data/dtBetasKernel.RData")


# dtBetasGam= dtReturns[, list(fact = facts, beta = {
#   dt = dtReturns[asset == "AUD"]
#   #   SD = .SD[, c("rx", facts), with = FALSE]
#   m = gam(rx ~ s(t1, by = rVixFX) + s(t1, by = MKT),  data = dt)  
#   
#   as.numeric(coef(m)[-1])
# }), by = "asset"]
# 
# dtBetasConst = cast(dtBetasConst, asset ~ fact,  value = "beta")



#################
####STOCKS
#################
load("../Data/dtPortfoliosFF.RData")
dtBetasFFConst = merge(dtPortfoliosFF, dtFactors, by = "date")
facts = c("DOL", "rVixFX", "MKT")

###Constant betas
dtBetasFFConst = dtBetasFFConst[, list(fact = facts,beta = {
  #   dt = dtReturns[asset == "AUD"]
  cols = c("rx", facts)
  SD = .SD[, cols, with = FALSE]
  m = lm(rx ~ ., data = SD)
  as.numeric(coef(m)[-1])
}), by = "asset"]

dtBetasFFConst = cast(dtBetasFFConst, asset ~ fact,  value = "beta")



# 
# 
# dtReturns[, qrf := ntile(rf, 5), by = "date"]
# dtReturns[, rx1 := rx - DOL]
# # dt1 = dtReturns[qrf == 5, list(rx = mean(rx), rVixFX = rVixFX[1], DOL = DOL[1]), by = t1]
# dt1 = dtReturns[asset == "AUD" & !is.na(rVixFX) & !is.na(MKT)]
# dt1[, MKT := MKT / 252]
# # dt1 = dt1[!is.na(rVixFX)]
# # dt1[, rx := residuals(lm(rx ~ DOL)) + mean(rx)]
# summary(lm(rx ~ DOL, dt1))
# library(mgcv)
# mGAM = gam(rx ~ s(t1, by = rVixFX) + s(t1, by = MKT) + s(t1, by = DOL),  data = dt1)
# modelNp = npscoef(rx ~ MKT + rVixFX + DOL| t1, data = dt1, bws = 252, betas = TRUE)
# summary(mGAM)
# m = lm(rx ~ MKT + rVixFX + DOL,  dt1)
# summary(m)
# anova(m)
# plot(mGAM)
# 
# plot((predict(mGAM, type = "terms")[, 1])/dt1$rVixFX, type = "l")
# plot(predict(mGAM, type = "terms")[, 2]/dt1$MKT, type = "l")
# 
# plot(coef(modelNp)[,4], col = "red")
# dt1[, mean(rx) * 252]
# dt1[, mean(MKT) * 252]
# 
# lines(coef(modelNp)[,2], col = "red")
# 
# 
# plot(predict(mGAM, type = "terms")[,2]/dt1$rVixFX +
#        predict(mGAM, type = "terms")[,1]/dt1$rVixFX  , type = "l")
# plot(predict(mGAM, type = "terms")[,"s(t1):rVixFX"]/dt1$rVixFX, type = "l")
# , col = "red", type = "l", ylim = c(-4, 4))
# lines(predict(mGAM, type = "terms")[,2]/dt1$rVixFX, col = "red", type = "l")
# plot(predict(mGAM, type = "terms")[,2]/dt1$rVixFX +
#        predict(mGAM, type = "terms")[,1]/dt1$rVixFX  , type = "l")
# # plot(predict(mGAM, data.table(rf = -0.01:0.05:0.001)))
# 
# lines(coef(modelNp)[,2], col = "red")
# 
# plot(mGAM)
# 
# ###Time-varying smooth betas
# dtReturns[, t1 := as.numeric(date - min(date))]
# 
# 
# 
# library(np)
# bwNp = npscoefbw(rx ~ CT | t1, dtReturns[asset == "AUD"], bws = 120)
# predict(bwNp)
# dt1 = dtReturns[asset == "AUD" & !is.na(CT)]
# 
# bw = npscoefbw(bws = 240, 
#                xdat = dt1$CT, 
#                ydat = as.numeric(dt1$rx), 
#                zdat = dt1$t1, 
#                bandwidth.compute = FALSE)
# modelNp = npscoef(bw,  betas = TRUE)
# summary(modelNp)
# 
# modelNp = npscoef(bw, txdat = dt1$CT, tydat = dt1$rx, tzdat = dt1$t1, betas = TRUE)
# 
# bw = npscoefbw(rx ~ CT | t1 , data = dt1, bws = 120, bandwidth.compute = FALSE)
# modelNp = npscoef(bw,  betas = TRUE)
# 
# modelNp = npscoef(rx ~ rVixFX | t1, data = dt1, bws = 240, betas = TRUE)
# modelNp = npscoef(rx ~ CT | t1, data = dt1, bws = c(120,120), betas = TRUE)
# modelNp = npscoef(bws = 120, txdat = matrix(dt1$CT), tydat = dt1$rx, tzdat = dt1$t1, betas = TRUE)
# 
# 
# summary(lm(dt1$rx ~ x))
# xpred = predict(modelNp)
# plot(cumsum(dtReturns[asset == "AUD" & !is.na(CT)]$rx), type = "l")
# lines(cumsum(coef(modelNp)[,2] * dt1$CT + mean(coef(modelNp)[,1])), type = "l", col = "green")
# 
# lines(cumsum(xpred), type = "l", col = "red")
# lines(cumsum(predict(mGAM)), type = "l", col = "blue")
# # plot(cumsum(dtReturns[asset == "AUD" & !is.na(CT)]$CT), type = "l")
# lines(cumsum(x), type = "l", col = "red")
# 
# ?npscoef
# 
# summary(lm(coef(modelNp)[, 1] ~ dt1$rx))
# 
# 
# summary(mGAM)
# plot(predict(mGAM, type = "terms")/dt1$CT, type = "l")
# lines(coef(modelNp)[,2], col = "red")
# 
# data(wage1)
# model.ols = lm(lwage ~ female + married + educ + exper + tenure, data = wage1)
# wage1.augmented = wage1
# wage1.augmented$dfemale <- as.integer(wage1$female == "Male")
# wage1.augmented$dmarried <- as.integer(wage1$married == "Notmarried")
# 
# model.scoef = npscoef(lwage ~ dfemale + dmarried + educ + exper + tenure | dfemale,
#                       betas = TRUE, data = wage1.augmented)
# summary(model.scoef)
# colMeans(coef(model.scoef))
# coef(model.scoef)[1:10,2]
# wage1[1:10,"female"]
# 
# 
# 
# 
