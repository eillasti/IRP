#                       

# dtWide[, SMB := SMB / 100]
# dtWide = merge(dtWide, dtPortfolios[, list(DOL = DOL[1]), keyby = date], all.x = TRUE)
# dtWide = merge(dtWide, dtVIX[, list(date = as.Date(Date, "%m/%d/%Y"), dV = dV)], all.x = TRUE, all.y = FALSE)
# assets = dtWide[1:1000,7:106, with = FALSE ]
# assets = dtWide[1:1000,7:10, with = FALSE ]
# dtWide[, FMP := FMP]
assets = dtWide[,2:6, with = FALSE ]
assets = assets - dtWide$DOL
# factors = dtWide[1:2000][, CT]
factors = dtWide[, list(dV, MKT, SMB, HML)]
factors = dtWide[, list(VOL, DOL)]
# names(factors) = "CT"
bws = 120

# summary(lm(dtWide$DOL ~ ., data = assets))

# mean(dtWide$dV) * 252
# sd(dtWide$dV) * sqrt(252)

#TS regressions
assetNames = colnames(assets)

library(plyr)
lsBetas = llply(names(assets),.fun = function(assetName){
  asset = assets[[assetName]]
  print("Ho!")
  as.data.table(npscoef(bws = bws, 
             xdat = factors,
             ydat = asset,
             zdat = 1:nrow(assets),
             betas = TRUE)$beta[, -1])[, asset := assetName]
})
betas = rbindlist(lsBetas)
setnames(betas,c(colnames(factors), "asset"))
plot(rep(0, nrow(assets)), type = "l", ylim = c(-0.3, 0.3))
plot(rep(0, nrow(assets)), type = "l", ylim = c(-1, 1))
l_ply(lsBetas, function(x) lines(x[[1]], type = "l"))
# plot(lsBetas[[1]][[1]], type = "l")

# plot(betas, type = "l")

#CS regression
mAssets = as.vector(as.matrix(assets))
mBetas = as.matrix(betas)
mData = cbind(mAssets, mBetas)
mCS = lm(mAssets * 252 ~ . - asset, data.frame(mData))
summary(mCS)
anova(mCS)

#CS regression on means
betasAvg = cbind(data.table(mAssets), betas)[, list(rx = mean(mAssets), 
                                    VOL = mean(VOL), 
                                    DOL = mean(DOL)), by = asset]
mCSAvg = lm(rx * 252~ VOL, betasAvg)
summary(mCSAvg)
plot(betasAvg$rx, predict(mCSAvg))
text(betasAvg$rx, predict(mCSAvg), betasAvg$asset)
abline(0, 252)

#CS regression on beta-sorted
dtData = cbind(data.table(mAssets), betas)
dtData[, betaQ := ntile(VOL, 20)]
betasAvg = dtData[, list(rx = mean(mAssets), 
                         VOL = mean(VOL), 
                         DOL = mean(DOL)), by = betaQ]
library(MASS)
mCSBetas = lm(rx * 252~ VOL, betasAvg)
summary(mCSBetas)
plot(betasAvg$rx, predict(mCSBetas))
# text(betasAvg$rx, predict(mCSBetas), betasAvg$betaQ)
abline(0, 252)

#GMM
library(gmm)
assets = data.table(cast(dtPortfoliosFX[substr(portfolio, 1, 3) == "Car", list(date, portfolio, rx - DOL)], date ~ portfolio))
factors = dtPortfoliosFX[, list(dVol = dVol[1], DOL = DOL[1]), by = date]
x = as.matrix(assets[, -1, with = FALSE]) * 252
z = as.matrix(factors[, -1, with = FALSE]) * 252
mGMM = gmm(x ~ z, z)
summary(mGMM)
# library(car)


install.packages("randomForest")
library(randomForest)

# mRF = 
# colMeans(assets) * 252

# 
# #Factor mimicking portfolio
# 
# mFMP = npscoef(bws = bws, 
#                xdat = assets,
#                ydat = dtWide$dV,
#                zdat = 1:nrow(factors),
#                betas = TRUE)
# 
# # summary(mFMP)
# head(mFMP$beta)
# FMP = predict(mFMP) - mFMP$beta[, 1]
# mean(FMP) * 252
# sd(FMP) * sqrt(252)
# 
# mean(FMP)/sd(FMP) * sqrt(252)
# 
# plot((cumsum(-0.1 * FMP)), type = "l")
