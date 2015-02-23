
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

dtData0 <- join(MyImport(strPathName1, "Spot"), MyImport(strPathName2, "Fwd"))
dtPortfolios0 <- MyProcess(dtData0)

dtVIX = fread(paste(strPath,"VIX.csv", sep = ""))
dtVIX[, Date := as.Date(Date, format = "%m/%d/%Y")]
dtVIX[, VIX := Close]
dtVIX = dtVIX[, list(Date, VIX)]
