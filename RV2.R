# IMPORT REALIZED VOLATILIES

library(xts)
load("../Data/GlobalFX_RV.RData")
xtsRV = xtsRV[xtsRV > 0 & xtsRV < 0.005]

library(data.table)
dtRV = data.table(date = as.Date(index(xtsRV)), RV = sqrt(as.numeric(xtsRV)))
dtRV[, dRV := c(0, diff(RV))]

ma <- function(x,n=5){c(NA, stats::filter(x[1:(length(x)-1)],rep(1/n,n), sides=1))}
dtRV[, RV1 := ma(RV, 1)]
dtRV[1, RV1:= dtRV[1, RV]]
dtRV[, RV5 := ma(RV, 5)]
dtRV[1:5, RV5:= mean(.SD[, RV])]
dtRV[, RV22 := ma(RV, 22)]
dtRV[1:22, RV22:= mean(.SD[, RV])]
dtRV[1:10]

model = lm(log(RV) ~ log(RV1) + log(RV5) + log(RV22), data = dtRV)
summary(model)
dtRV[, RV_pred := exp(predict(model))]
dtRV[, dRV := RV - exp(predict(model))]
dtRV[, RVol := sqrt(RV)]
dtRV[, dRVol := sqrt(RV) - sqrt(exp(predict(model)))]



plot(dtRV$date, dtRV$RV, type = "l")
lines(dtRV$date, dtRV$RV_pred, col = "red")

# save(dtRV, file = "../Data/dtRV.RData")


plot(sqrt(dtRV$RV))

# dt[crncy == "CHF" & date == "1990-01-03"]
