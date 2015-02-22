library(data.table)
dtFF = fread("../data/FF100_processed.csv", sep = ",")

# dtFF[, HML := ifelse(PortfolioPrice == 1)]
dtFF[, HML := 0]
dtFF[PortfolioPrice == 1, HML := mean(Returns, na.rm = TRUE), by = c("Date", "PortfolioPrice")]
dtFF[PortfolioPrice == 10, HML := -mean(Returns, na.rm = TRUE), by = c("Date", "PortfolioSize")]
dtFF[, HML := sum(HML, na.rm = TRUE), by = Date]

dtFF[, SMB := 0]
dtFF[PortfolioSize == 1, SMB := mean(Returns, na.rm = TRUE), by = c("Date", "PortfolioSize")]
dtFF[PortfolioSize == 10, SMB := -mean(Returns, na.rm = TRUE), by = c("Date", "PortfolioSize")]
dtFF[, SMB := sum(SMB, na.rm = TRUE), by = Date]

dtFF[, betaHML := coef(lm(Returns ~ HML + SMB, .SD))[2], by = c("PortfolioPrice", "PortfolioSize")]
dtFF[, betaFMB := coef(lm(Returns ~ HML + SMB, .SD))[3], by = c("PortfolioPrice", "PortfolioSize")]

m = lm(Returns ~ betaHML + betaFMB, dtFF)

m = lm(Returns * 252 ~ PortfolioSize + PortfolioPrice, dtFF)
summary(m)

dtFF

dtFF[Date == "1926-07-01"]
