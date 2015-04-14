library(data.table)
library(dplyr)
dtVIX = fread("../Data/VIX.csv")
dtVIX[, date := as.Date(Date, "%m/%d/%Y")]
setkey(dtVIX, date)
dtVIX[, Close := Close / 100 ]
dtVIX[, V := Close * Close]

ma <- function(x,n=5){c(NA, stats::filter(x[1:(length(x)-1)],rep(1/n,n), sides=1))}
dtVIX[, V1 := ma(V, 1)]
dtVIX[1, V1:= dtVIX[1, V]]
dtVIX[, V5 := ma(V, 5)]
dtVIX[1:5, V5:= mean(.SD[, V])]
dtVIX[, V22 := ma(V, 22)]
dtVIX[1:22, V22:= mean(.SD[, V])]


model = lm(log(V) ~ log(V1) + log(V5) + log(V22), data = dtVIX)
# model = lm(log(V) ~ log(V1), data = dtVIX)
summary(model)
dtVIX[, V_pred := exp(predict(model))]
dtVIX[, dV := V - exp(predict(model))]
dtVIX[, Vol := sqrt(V)]
dtVIX[, dVol := sqrt(V) - sqrt(exp(predict(model)))]
dtVIX[, dV := log(V) - log(V_pred)]

# 
# 
# 
# dtVIX[, dlV := log(V) - log(lag(V))]
# dtVIX[, dV := (V) - (lag(V))]
# dtVIX[, Vol := Close]
# dtVIX[, dVol := Vol - lag(Vol)]
# dtVIX = dtVIX[-1, list(date, V, dV, dlV, Vol, dVol)]

# save(dtVIX, file = "R/dtVIX.Rdata")

# # 
# plot(cumsum(dtVIX$dV), type = "l")
# plot(dtVIX$Vol, type = "l")
# plot(Vol, type = "l")
# sd(dtVIX$dV)
# # acf(dtVIX$dVol)
# # 
# # 
# arima(dtVIX$V, c(1,0,0))
