
library(plyr)
library(dplyr)
library(reshape)
library(data.table)
library(zoo)
options(digits = 4)
source("libImportAndProcess.R")


strPath = "../Data/"
#strName <- "Data datastream and Bloomberg.xlsx"
strName1 <- "DS_SPOT.csv"
strPathName1 <- paste(strPath, strName1, sep = "")
strName2 <- "DS_FWD.CSV"
strPathName2 <- paste(strPath, strName2, sep = "")

dtDatastream <- join(MyImport(strPathName1, "Spot"), MyImport(strPathName2, "Fwd"))
dtIndirect = data.table("Crncy" = c("EUR", "JPY", "GBP", "CAD", "AUD", "NZD", "CHF", "DKK", "NOK", "SEK")
                        ,"Indirect" = c(1, -1, 1, -1, 1, 1, -1, -1, -1, -1))
dtIndirect = data.table("Crncy" = c("EUR", "JPY", "GBP", "CAD", "AUD", "NZD", "CHF", "DKK", "NOK", "SEK")
                        ,"Indirect" = -c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

dtDatastream = merge(dtDatastream, dtIndirect, by = "Crncy")
dtDatastream[, Spot := Spot ^ Indirect]
dtDatastream[, Fwd := Fwd ^ Indirect]

dtDatastream = dtDatastream[!is.na(Spot) & !is.na(Fwd)]
setkey(dtDatastream, Crncy, Date)
dtDatastream[, rf := -(log(Fwd) - log(Spot)) * 12]
dtDatastream[, rx := c(NA, log(Spot[2:.N]) - log(Spot[1:(.N-1)]))
             + rf * c(NA, Date[2:.N] - Date[1:(.N - 1)] ) / 365.25
             , by = "Crncy"]
dtDatastream = dtDatastream[!is.na(rx)]


# dtDatastream[, rx := rx * Indirect]

dtDatastream[Date > "1990-01-01", mean(rx) * 252 , by = "Crncy"]
dtDatastream[Date > "1990-01-01", mean(rf), by = "Crncy"]

setnames(dtDatastream, "Crncy", "portfolio")
# dtPortfolios[, mean(rf) * 100, by = "portfolio"]
# dtDatastream[Crncy== "AUD" & Date > "2000-01-01"]
# plot(dtDatastream[Crncy == "GBP", Spot], type = "l")
# plot(dtDatastream[Crncy == "JPY", rf], type = "l")
# abline(1, 0)
