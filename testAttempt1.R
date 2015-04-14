library(data.table)


dtFF
dtFF1
dtPortfoliosFX


dtCT = dtPortfoliosFX[substr(portfolio, 1, 3) == "Car", list(date, portfolio, rx)]
dtFF2 = dtFF[, list(date = Date, 
                    portfolio = paste0("S",PortfolioSize, "_P",PortfolioPrice),
                    rx = Returns)]
dtFF2 = dtFF2[!is.na(rx)]
# setnames(dtFF1, "Date", "date")
# str(dtCT)
str(dtFF2)
dtAll = rbind(dtCT, dtFF2, use.names = TRUE)
# dtAll = dtCT
dtAll = dtAll[date > "1990-01-01"]





library(reshape)
dtWide = data.table(cast(dtAll, date ~ portfolio))
dtWide = merge(dtWide, dtFF1, by = "date")
setkey(dtWide, "date")
dtWide[, CT := Carry_5 - Carry_1]
dtWide = dtWide[!is.na(CT)]
dtWide[, mean(CT * 252, na.rm = TRUE)]
dtWide[, mean(Carry_1 * 252, na.rm = TRUE)]
dtWide[, mean(Carry_5 * 252, na.rm = TRUE)]
dtWide[, hCT := CT - predict(m)]
dtWide[, t1 := .I]

library(mgcv)
m = gam(CT ~ -1 + s(t1, by = MKT) + s(t1, by = SMB) + s(t1, by = HML), data = dtWide[1:3000])
# m = gam(CT ~ -1 + HML, data = dtWide)
summary(m)
# plot(m)
dtWide[, hCT := CT - predict(m, newdata = .SD)]
dtWide[3001:nrow(dtWide), mean(hCT, na.rm = TRUE) / sd(hCT, na.rm = TRUE) * sqrt(252)]
dtWide[3001:nrow(dtWide), mean(CT, na.rm = TRUE) / sd(CT, na.rm = TRUE) * sqrt(252)]

m1 = lm(hCT ~ -1 + MKT + SMB + HML, dtWide[1001:3000])
summary(m1)


m = lm(CT ~ -1 + . - Carry_1 - Carry_2 - Carry_3 - Carry_4 - Carry_5 - date - hCT, dtWide[1:3000])
summary(m)
mean(predict(m) * 252, newdata = dtWide)

dtWide[, hCT := CT - predict(m, newdata = .SD)]
dtWide[1001:nrow(dtWide), mean(hCT, na.rm = TRUE) * 252]
dtWide[3001:nrow(dtWide), sd(hCT, na.rm = TRUE) * sqrt(252)]

dtWide[, mean(CT, na.rm = TRUE) * 252]
dtWide[, sd(CT, na.rm = TRUE) * sqrt(252)]

plot(dtWide$date, exp(cumsum(dtWide$CT)), type = "l")


dtWide[001:nrow(dtWide), mean(hCT, na.rm = TRUE) / sd(hCT, na.rm = TRUE) * sqrt(252)]
dtWide[001:nrow(dtWide), mean(CT, na.rm = TRUE) / sd(CT, na.rm = TRUE) * sqrt(252)]

dtWide[1:1000, mean(hCT, na.rm = TRUE) * 252]
dtWide[001:nrow(dtWide), mean(CT, na.rm = TRUE) / sd(CT, na.rm = TRUE) * sqrt(252)]

sd(predict(m, newdata = dtWide), na.rm = TRUE)*sqrt(252)
# dtWide = da

m1 = lm(hCT ~ -1 + . - Carry_1 - Carry_2 - Carry_3 - Carry_4 - Carry_5 - date - CT, dtWide[3000:4000])
summary(m1)

install.packages("glmnet")
library(glmnet)

dtWide[is.na(dtWide)] = 0
mX = as.matrix(dtWide[, 7:106, with = FALSE])
m = glmnet(mX, as.matrix(dtWide$CT), family = "gaussian")
summary(m)
mean(predict(m,mX, type = "link" ) * 252)

dtWide[, hCT := CT - predict(m, mX)]



