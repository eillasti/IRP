dtCT = copy(dt)

dtCT[, portfolio := paste0("Carry_", ntile(rf, 5)), by = date]
setkey(dtCT, portfolio, date)

dtCT[, mean(rx*252)/sd(rx*sqrt(252)), keyby = "portfolio"]

# plot(dtCT[, mean(rx*252), keyby = "portfolio"]$V1)

# plot(dtCT[portfolio == "Carry_9"]$date,dtCT[portfolio == "Carry_9", cumsum(rx)], type = "l")

table(dtCT$crncy, dtCT$portfolio)

dtCT = dtCT[, list(rx = mean(rx), rf = mean(rf)), by = c("portfolio", "date")]
