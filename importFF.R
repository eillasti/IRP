library(data.table)
library(reshape2)
dtWide = fread("../data/100_Portfolios_10x10_Daily.csv", sep = ",")
setnames(dtWide, "V1", "Date")
dtLong = melt(dtWide, id.vars = "Date", variable.name = "Portfolio", value.name = "Returns")
dtLong[, Portfolio := substring(Portfolio, 2)]
dtLong[, Portfolio := as.integer(Portfolio) - 2L]
dtLong[, PortfolioSize := Portfolio %/% 10 + 1]
dtLong[, PortfolioPrice := Portfolio %% 10 + 1]
dtLong[, Portfolio := NULL]
dtLong[Returns == - 99.99, Returns := NA]
dtLong[, Returns := Returns/100]
dtLong[, Date := as.Date(as.character(Date), format = "%Y%m%d")]
dtLong

write.table(dtLong, file = "../data/FF100_processed.csv", row.names = FALSE, sep = ",")

