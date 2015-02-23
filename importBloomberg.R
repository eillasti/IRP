options(digits = 4)
source("libImportAndProcess.R")

library(data.table)
library(dplyr) #used for lag
bloomberg = fread("../Data/Bloomberg.csv")

bloomberg_translate = fread("../data/Bloomberg_translate.csv")
setkey(bloomberg_translate, Crncy)

dt = copy(bloomberg)
i = 5
n_crncy = (ncol(dt) / 4)
ls_cum = list()
for (i in 1:n_crncy){
  crncy = substr(colnames(dt)[i * 2 - 1], 1, 3)
  
  cols_spot = (i * 2 - 1):(i * 2)
  dt_spot_raw = dt[, cols_spot, with = FALSE]
  setnames(dt_spot_raw, c("date", "value"))
  dt_spot = dt_spot_raw[!is.na(value) & !is.na(date), list(
    "date" = as.Date(date, format = "%m/%d/%Y"),
    "spot" = value ^ bloomberg_translate[J(crncy)]$Indirect)]
  setkey(dt_spot, date)
  
  cols_fwd = ((i + n_crncy) * 2 - 1):((i + n_crncy) * 2)
  dt_fwd_raw = dt[, cols_fwd, with = FALSE]
  setnames(dt_fwd_raw, c("date", "value"))
  dt_fwd = dt_fwd_raw[!is.na(value) & !is.na(date), list(
    "date" = as.Date(date, format = "%m/%d/%Y"),
    "fwd_pips" = value * bloomberg_translate[J(crncy)]$Mult)]
  setkey(dt_spot, date)
  
  dt_merge = merge(dt_spot, dt_fwd, all.x = FALSE, all.y = FALSE)
  dt_merge[, crncy := crncy]
  dt_merge[, fwd := (spot + fwd_pips) ^ bloomberg_translate[J(crncy)]$Indirect]
  dt_merge[, spot := spot ^ bloomberg_translate[J(crncy)]$Indirect]
  dt_merge[, s := log(spot)]
  dt_merge[, f := log(fwd)]
  dt_merge[, rf := (s - f) * 12]
  dt_merge = dt_merge[abs(rf) < 0.2]
  dt_merge[, rx := (s - lag(s) + rf / 365.25 * as.numeric(date - lag(date)))]
  dt_merge = dt_merge[abs(rx) < 0.1]
#   dt_merge[, rx := rx * 252]
  dt_merge = dt_merge[-1]
  
  ls_cum[[i]] = dt_merge
}
dt = rbindlist(ls_cum)
setkey(dt, date, crncy)
dtBloomberg = dt[, list(rx, rf), keyby = c("crncy", "date")]
# save(dtBloomberg, file = paste0(getwd(),"/R/dtBloomberg.RData"))
