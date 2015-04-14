# Predicts returns conditional only on past info
# data.table => precicted returns

modelEstimatorLm = function(dt){
  #   f = formula(rx * 12 ~ mom1 + mom3 + mom6 + mom12 + rf + qrf)
  f = formula(rx * 12 ~ -1 + rf)
  lm(f, data = dt)
}
modelEstimator0 = modelEstimatorLm

predictReturns = function(dt, step, dateStart = as.yearmon("Dec 1980"),
                          modelEstimator = modelEstimator0, dtIndex = "date"){
  predictReturnsInner = function(dtKnown, dtNew){
    m = modelEstimator(dtKnown)
    predict(m, newData = dtNew)
  }
  
  dateFirst = min(dt$date)
  dateEnd = max(dt$date)
  dateStart = as.yearmon(ifelse(dateStart < dateFirst, dateFirst, dateStart))
  #Put NAs for the predictions when not enough info is available
  pred = rep(NA, nrow(dt[date <= as.yearmon(dateStart + step)]))
  dateLastKnown = dateStart + step
  dateLastForecast = as.yearmon(min(dateLastKnown + step + 1/12, dateEnd))
  while(dateLastKnown < dateEnd){
    dateLastForecast = as.yearmon(min(dateLastKnown + step, dateEnd))
    print(paste0("Estimating on ", dateStart, " to ", dateLastKnown, 
                 ", forecasting ", as.yearmon(dateLastKnown + 1/12), " to ", dateLastForecast))
    
    dtKnown = dt[date <= dateLastKnown]
    dtTest = dt[dateLastKnown < date & date <= dateLastForecast]
    m = modelEstimator(dtKnown)
    predCurrent = predict(m, newdata = as.data.frame(dtTest))
#     predCurrent = dtTest$asset
#     predCurrent = paste(predCurrent, as.yearmon(dtTest$date))
    if (length(predCurrent) != nrow(dtTest)) {
      print(summary(m))
      stop("Incorrect predictions length!")
    }
    pred = append(pred, predCurrent)
    
    dateLastKnown = as.yearmon(dateLastKnown + step)
  }
  
  pred
}

library(testthat)
dates = as.yearmon(as.numeric(as.yearmon("Jan 1990")) + (1:240 - 1) / 12)
dates
assets = c("AUD", "JPY")
dt1 = merge(data.table("date" = dates, "k" = 1), data.table("asset" = assets, "k" = 1), by = "k", allow.cartesian = TRUE)
dt1[, k := NULL]
dt1[, rf := rnorm(.N)]
dt1[, rx := 0.8 * rnorm(.N) + 0.2 * rf]
dt1[, rx_pred := predictReturns(.SD, 1)]


# dt1[, mean(rx - rx_pred)]

# 
# data.table(date, asset, rx_pred) => w
portfolioWeights = function(dt, q = 5){
  dt = copy(dt)
  dt[, qrx_pred := ntile(rx_pred, q), by = "date"]
  dt[, N := q, by = "date"]
  dt[, w := 0]
  dt[qrx_pred == N, w := 1]
  dt[qrx_pred == 1, w := -1]
  dt$w
}

# 
dt1[, rx_pred := predictReturns(.SD, 1, modelEstimator = modelEstimatorLm)]

# dt1 = copy(monthlyReturns[date > "Jan 1989"])
# rx_pred = predictReturns(dt1, 1)
dt1[, rx_pred := predictReturns(.SD, 1)]
dt1[, w := portfolioWeights(.SD, q = 3)]

dtMarket = dt1[, list(rx = sum(w * rx)), by = date]
dtMarket[, crx := cumsum(rx)]
plot(dtMarket$date, dtMarket$crx, type = "l")
# 
# # # 
# # # 
# # # 
# # # 
# # # date0 = as.yearmon("Jan 1991")
# # # date1 = date0
# # # dt1[,pos := 0]
# # # while(date1 < "Jan 2015"){
# # #   m = lm(rx * 12 ~ mom1 + mom3 + mom6 + mom12 + rf + qrf, data = dt1[date < date1])
# # #   summary(m)
# # #   dt1[date > date1, rx_pred := predict(m, newdata = .SD)]
# # #   dt1[date > date1, pos := 0]
# # #   dt1[date > date1, qrx_pred := ntile(rx_pred, 5), by = "date"]
# # #   dt1[date > date1, N := 5L, by = "date"]
# # #   dt1[date > date1 & qrx_pred == N, pos := 1]
# # #   dt1[date > date1 & qrx_pred == 1, pos := -1]
# # #   date1 = date1 + 1
# # # }