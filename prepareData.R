source("importBloomberg.R")
source("VIX.R")
source("RV2.R")

dt = merge(dtBloomberg, dtVIX, all.x = TRUE, all.y = FALSE)
# dt = dt[!(crncy %in% c("AUD", "NZD", "JPY"))]
dt = merge(dt, dtRV, all.x = TRUE, all.y = FALSE)
setkey(dt, crncy, date)
dt = dt[, list(crncy, date, rx, dVol, dRVol, rf)]
dtDOL = dt[, list(DOL = mean(rx)), keyby = date]

dtPortfolios = dt[, list(portfolio = crncy, date, rx, rf)]

source("momAll.R")
dtPortfolios = rbind(dtPortfolios, dtMomPortfolios, use.names = TRUE)
# 
source("carryTrade.R")
dtPortfolios = rbind(dtPortfolios, dtCT, use.names = TRUE)

# dtPortfolios = rbind(dtPortfolios, dtBetaPortfolios)


setkey(dtPortfolios, date, portfolio)
dtPortfolios = merge(dtPortfolios, dtRV[, list(date, dRVol)], by = "date", all.x = TRUE, all.y = FALSE)
dtPortfolios = merge(dtPortfolios, dtVIX[, list(date, dV = dV,  dVol = dVol)], by = "date", all.x = TRUE, all.y = FALSE)
dtPortfolios = merge(dtPortfolios, dtDOL, by = "date", all.x = TRUE, all.y = FALSE)

dtPortfoliosFX = dtPortfolios
save(dtPortfoliosFX, file = "../Data/dtPortfoliosFX.RData")
dtPortfoliosFX[, mean(rx) * 252, by = "portfolio"]
# dtPortfolios[, mean(rx) *252, by = "portfolio"]

# sd(dtRV$RVol) * sqrt(252)
# sd(dtRV$RV)
