library(data.table)
dtFF = fread("../data/FF100_processed.csv", sep = ",")
dtFF[, Date := as.Date(Date)]
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
# dtFF[, betaMKT := coef(lm(Returns ~ MKT))[2], by = c("PortfolioPrice", "PortfolioSize")]
dtFF[, betaHML := coef(lm(Returns ~ MKT + HML + SMB, .SD))[3], by = c("PortfolioPrice", "PortfolioSize")]
dtFF[, betaSMB := coef(lm(Returns ~ MKT + HML + SMB, .SD))[4], by = c("PortfolioPrice", "PortfolioSize")]

#Pooled cross-section
m = lm(Returns ~ betaMKT + betaHML + betaSMB, dtFF)
summary(m)

#By portfolio
gmean = function(x, na.rm = FALSE) {
  exp(mean(log(x + 1), na.rm = na.rm)) - 1
}
dtFF_CS = dtFF[, list(Returns = gmean(Returns, na.rm = TRUE),
                      betaMKT = betaMKT[1],
                      betaHML = betaHML[1],
                      betaSMB = betaSMB[1]),
               keyby = c("PortfolioPrice", "PortfolioSize")]
modelCS = lm(Returns * 252 ~ betaMKT + betaHML + betaSMB, dtFF_CS)
summary(modelCS)

bySMB = dtFF_CS[, list(betaMKT = mean(betaMKT), Returns = mean(Returns)), by = "PortfolioSize"]
byHML = dtFF_CS[, list(betaMKT = mean(betaMKT), Returns = mean(Returns)), by = "PortfolioPrice"]
plot(bySMB$betaMKT, bySMB$Returns * 252)
plot(byHML$betaMKT, byHML$Returns * 252)
plot(dtFF_CS$betaMKT, dtFF_CS$Returns * 252)
