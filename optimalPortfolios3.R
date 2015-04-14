monthlyReturns2 = copy(monthlyReturns)
# monthlyReturns2 =  monthlyReturns[asset %in% top10]

monthlyReturns2[, sgn := as.numeric(rf > 0)]
monthlyReturns2[, fd := (rf - mean(rf))/sd(rf), by = "date"]
monthlyReturns2[is.na(fd), fd := 0]
monthlyReturns2[, mom := ma(rx, 3), by = "asset"] 
monthlyReturns2[is.na(mom), mom := 0]

setkey(monthlyReturns2, "asset", "date")

CRRA = function(c, gamma) {
  (1 + c) ^ (1 - gamma) / (1 - gamma)
}

strategy = function(theta, dt) {
  tsgn = theta[1]
  tfd = theta[2]
  tmom = theta[3]
  dt[, list(rx = mean((tsgn * sgn + tfd * fd + tmom * mom) * rx)), by = "date"]
}

U = function(theta, dt) {
  rx = strategy(theta, dt)
  -sum(CRRA(rx, 1.2))
}

optimalStrategy = function(dtKnown){
  opt = optim(theta0, U, dt = dtKnown)
  function(dt) {
    strategy(theta, dt)$rx
  }
}

?optim
theta0 = c(0, 0, 0)
x = optim(theta0, U, dt = monthlyReturns2[date < "Jan 1996"])


# dt1 = strategy(c(1, 0, 0), monthlyReturns2)
# dt1[, SR(rx, 12)]
# dt1 = strategy(c(0, 1, 0), monthlyReturns2)
# dt1[, SR(rx, 12)]
# dt1 = strategy(c(0, 0, 1), monthlyReturns2)
# dt1[, SR(rx, 12)]
dt1 = strategy(x$par, monthlyReturns2[date >= "Jan 1996"])
dt1[, crx := cumsum(rx)]
dt1[, SR(rx, 12)]
plot(dt1$date, (dt1$crx), type = "l")
