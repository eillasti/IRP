library(zoo)
library(dplyr)
library(plyr)

calcMomPortfolio = function(dt, l) {
  dtMom = copy(dt)
  
  dtMom[, ym := as.yearmon(date)]
  dtMom_m = dtMom[, list(rxm = sum(rx)), keyby = c("crncy", "ym")]
  dtMom_m[, mom := ma(rxm, l), by = "crncy"]
  dtMom_m = dtMom_m[!is.na(mom)]
  dtMom_m[, portfolio := paste("Momentum", l, ntile(mom, 5), sep = "_"), by = "ym"]
  
  dtMom_m[, mean(rxm) * 12, by = "portfolio"]
  
  
  dtMom = merge(dtMom, dtMom_m, by = c("crncy", "ym"), all.x = FALSE, all.y = FALSE)
  setkey(dtMom, crncy, date)
  dtMom = dtMom[, list(crncy, date, rx, portfolio, dVol, dRVol, rf)]
  
  dtMomPortfolios = dtMom[, list(rx = mean(rx), rf = mean(rf)), keyby = c("date", "portfolio")]
  
}

lsMomPortfolios = llply(list(1,3,6,12), function(l) calcMomPortfolio(dt, l))
dtMomPortfolios = rbindlist(lsMomPortfolios)

dtMomPortfolios[, mean(rx)/sd(rx) * sqrt(252), keyby = "portfolio"]
dtMomPortfolios[, mean(rx)* (252), keyby = "portfolio"]
# l = 1
