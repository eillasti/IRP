MyImport <- function(strPathName, strKPI)
{
  dfImport <- read.table(strPathName, header = TRUE, sep = ",")
  dfImport$Date = as.Date(dfImport$Date)
  dtImport <- as.data.table(dfImport)
  dtImport = melt(dtImport, id.vars = c("Date"), 
                      variable.name = Curncy, value.name = FX)
  dtImport = as.data.table(dtImport)
  setnames(dtImport, old = c("variable", "value"), new = c("Crncy", strKPI))
  #dtImport$KPI = as.factor(strKPI)
  setkey(dtImport, "Date", "Crncy")
  return(dtImport)
}

MyProcess <- function(dtData)
{
  dtData = dtData0[Date <= "2009-08-03"]
  dtData <- filter(dtData, is.na(Fwd)==FALSE)
  dtData <- mutate(dtData, YearMon = as.yearmon(Date))
  dtData <- mutate(dtData, s = log(Spot), f = log(Fwd))
  dtData <- mutate(group_by(dtData, Crncy, add = FALSE), ret = lag(s, 1, order_by = Date)-s)
  # dtData <- mutate(group_by(dtData, Crncy, YearMon), dplyr::last(Date, Date), 
  #                  Vol = var(ret, na.rm = TRUE))
  dtDataMon <- summarise(group_by(dtData, Crncy, YearMon, add = FALSE), 
                         Spot = dplyr::last(Spot, Date), 
                         Fwd = dplyr::last(Fwd, Date),
#                          vol = var(ret, na.rm = TRUE)
                         vol = mean(abs(ret), na.rm = TRUE)
                         )
  
  dtDataMon <- mutate(dtDataMon, s = log(Spot), f = log(Fwd))
  dtDataMon <- mutate(dtDataMon, fd = f - s)
  dtDataMon <- mutate(group_by(dtDataMon, Crncy, add = FALSE), 
                      rx = lag(f, 1, order_by = YearMon)-s)
  dtDataMon
  
  #dtDataMon$Portfolio <- as.factor(ntile(dtDataMon$fd, 5))
  dtDataMon <- mutate(group_by(dtDataMon, YearMon, add = FALSE), 
                      Portfolio = (paste("p", ntile(fd, 5), sep = "")))
  dtDataMon$Portfolio <- (dtDataMon$Portfolio)
  # summary(filter(dtDataMon, Crncy =="AUD"))
  # is.factor(dtDataMon$Portfolio)
  
  dtPortfolios <- summarise(group_by(dtDataMon, YearMon, Portfolio, add = FALSE), 
                            rx = mean(rx))
  dtPortfolios <- cast(dtPortfolios, YearMon ~ Portfolio, value = "rx")
  dtPortfolios <- data.table(dtPortfolios, key = "YearMon")
  dtVol <- summarise(group_by(dtData, YearMon, add = FALSE), 
#                      vol = var(ret, na.rm = TRUE)
                     vol = mean(abs(ret), na.rm = TRUE)
                     ) 
  dtVol <- mutate(dtVol, dVol = arima(vol, order = c(1, 1, 0))$residuals)
  setkey(dtVol, YearMon)
  dtPortfolios <- merge(dtPortfolios, dtVol)
  plot(12 * (dtPortfolios$vol), type = "l")

dtDataMon = merge(dtDataMon, dtVol, by = "YearMon")

model = lm("p5 ~ dVol", data = dtPortfolios)
AUD = dtDataMon[Crncy == "AUD"]$rx
dVol = dtPortfolios$dVol
model = lm("rx ~ dVol", data = dtDataMon)
  summary(model)
  # summarise(group_by(dtPortfolios, Portfolio, add = FALSE), 
  #           mean(rx*12*100, na.rm = TRUE))
  # summarise(group_by(dtDataMon, Crncy, add = FALSE), 
  #           mean(rx*12*100, na.rm = TRUE))
  
  
  return(dtPortfolios)
}