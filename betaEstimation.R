load("../Data/dtFactors.RData")
load("../Data/dailyReturns.RData")

dtFactors
dailyReturns

dtReturns = merge(dailyReturns, dtFactors, by = "date", all.x = TRUE, all.y = FALSE)

facts = c("DOL", "CT")
dtBetasConst = dtReturns[, list(fact = facts,beta = {
#   dt = dtReturns[asset == "AUD"]
  SD = .SD[, c("rx", facts), with = FALSE]
  m = lm(rx ~ ., data = SD)
  as.numeric(coef(m)[-1])
}), by = "asset"]

dtBetasConst = cast(dtBetasConst, asset ~ fact,  value = "beta")

