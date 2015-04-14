# 
# x = vix[!is.na(vixFX)]$vixFX
# xcov = vix[!is.na(vixFX), list(weekday = relevel(factor(weekdays(date)), "Monday")
#                                #                   , month = relevel(factor(months(date)), "January")
# )]
# x = log(vix[]$vixSP)
# xcov = vix[, list(weekday = relevel(factor(weekdays(date)), "Monday")
# #                                                  , month = relevel(factor(months(date)), "January")
# )]



HARres = function(x, xcov = data.table(x = x), returns = FALSE){
  ma <- function(x,n){
    c(NA, stats::filter(x[1:(length(x)-1)],rep(1/n,n), sides=1))
  }
  
  xcov = copy(xcov)
  xcov[, x := x]
  xcov[, x1 := ma(x, 1)]
  xcov[, x5 := ma(x, 5)]
  xcov[, x22:= ma(x, 22)]
  
  model = lm(x ~ ., xcov)
  summary(model)
  anova(model)
  if (!returns){
    x - predict(model, newdata = xcov)    
  } else {
    (x - predict(model, newdata = xcov)) / x
  }
}


# dtFactors[, rrVixFX := HARres(vixFX, returns = TRUE)]
# m = lm(rrVixFX ~ rVixFX, dtFactors)
# summary(m)
# mean(dtFactors$rrVixFX, na.rm = TRUE) /sd(dtFactors$rVixFX, na.rm = TRUE) * sqrt(nrow(dtFactors))
# plot(x, type = "l")
# plot(HARres(x, xcov), col = "green", type = "l")
# length(HARres(x, xcov))
