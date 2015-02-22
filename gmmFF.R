library(data.table)
dtFF = fread("../data/FF100_processed.csv", sep = ",")

#Calculate factors
dtFF[, HML := 0]
dtFF[PortfolioPrice == 1, HML := mean(Returns, na.rm = TRUE), by = c("Date", "PortfolioPrice")]
dtFF[PortfolioPrice == 10, HML := -mean(Returns, na.rm = TRUE), by = c("Date", "PortfolioSize")]
dtFF[, HML := sum(HML, na.rm = TRUE), by = Date]

dtFF[, SMB := 0]
dtFF[PortfolioSize == 1, SMB := mean(Returns, na.rm = TRUE), by = c("Date", "PortfolioSize")]
dtFF[PortfolioSize == 10, SMB := -mean(Returns, na.rm = TRUE), by = c("Date", "PortfolioSize")]
dtFF[, SMB := sum(SMB, na.rm = TRUE), by = Date]

dtFF[, MKT := mean(Returns, na.rm = TRUE), by = "Date"]

#Estimate betas
dtFF[, betaMKT := coef(lm(Returns ~ MKT + HML + SMB, .SD))[2], by = c("PortfolioPrice", "PortfolioSize")]
dtFF[, betaHML := coef(lm(Returns ~ MKT + HML + SMB, .SD))[3], by = c("PortfolioPrice", "PortfolioSize")]
dtFF[, betaFMB := coef(lm(Returns ~ MKT + HML + SMB, .SD))[4], by = c("PortfolioPrice", "PortfolioSize")]

m = lm(Returns ~ betaMKT + betaHML + betaFMB, dtFF)

summary(m)
