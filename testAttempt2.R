dtWide
m = gam(CT ~ -1 + s(t1, by = MKT) + s(t1, by = SMB) + s(t1, by = HML), data = dtWide[1:3000], gamma = 0.2)
# m = gam(CT ~ -1 + HML, data = dtWide)
m = lm(CT ~ -1 + MKT + SMB + HML, data = dtWide[2500:3000])
m = linearRidge(CT ~ -1 + MKT + SMB + HML, data = dtWide[2500:3000])
summary(m)
plot(m)
dtWide[, hCT := CT - predict(m, newdata = .SD)]
dtWide[3001:nrow(dtWide), mean(hCT, na.rm = TRUE) / sd(hCT, na.rm = TRUE) * sqrt(252)]
dtWide[3001:nrow(dtWide), mean(CT, na.rm = TRUE) / sd(CT, na.rm = TRUE) * sqrt(252)]

m1 = lm(hCT ~ -1 + MKT + SMB + HML, dtWide[3001:nrow(dtWide)])
summary(m1)
m2 = lm(CT ~ -1 + MKT + SMB + HML, dtWide[3001:nrow(dtWide)])
summary(m2)


m1 = lm(hCT ~ -1 + MKT + SMB + HML, dtWide[3001:3500])
summary(m1)

# install.packages("ridge")
# library(ridge)
# dtWide[, t1 := .I]
m2 = linearRidge(CT ~ -1 + MKT + SMB + HML, data = dtWide)
summary(m2)

install.packages("np")
library(np)
bw = npscoefbw(CT ~  MKT | t1, data = dtWide[1:200], bws = 300)
summary(bw)
plot(bw)

mnp = npscoef(CT ~ MKT + SMB + HML, data = dtWide, betas = TRUE)
mnp = npscoef(bws = 100, 
              txdat = dtWide[,list(MKT, SMB, HML)],
              tydat = as.numeric(dtWide$CT),
              tzdat = as.numeric(dtWide$date))

mnp = npscoef(bws = 100, 
              xdat = dtWide[,list(MKT, SMB, HML)],
              ydat = as.numeric(dtWide$CT),
              zdat = as.numeric(dtWide$date))



mnp = npscoef(bws = 200, 
              xdat = dtWide[,list(MKT, SMB, HML)],
              ydat = as.numeric(dtWide$CT),
              zdat = as.numeric(dtWide$date),
              betas = TRUE)

summary(mnp)
head(mnp$beta)

plot(mnp$beta[, 1], type = "l")
predict(mnp)

dtWide[, hCT := CT - predict(mnp, newdata = .SD) + mnp$beta[, 1]]
dtWide[1:nrow(dtWide), mean(hCT, na.rm = TRUE) / sd(hCT, na.rm = TRUE) * sqrt(252)]
dtWide[001:nrow(dtWide), mean(CT, na.rm = TRUE) / sd(CT, na.rm = TRUE) * sqrt(252)]

m1 = lm(hCT ~ -1 + MKT + SMB + HML, dtWide[001:200])
summary(m1)
m2 = lm(CT ~ -1 + MKT + SMB + HML, dtWide[3001:nrow(dtWide)])
summary(m2)

plot(exp(cumsum(dtWide$hCT)), type = "l")
lines(exp(cumsum(dtWide$CT)), type = "l", col = "red")

m1 = lm(hCT ~ -1 + MKT + SMB + HML, dtWide[3001:3500])
summary(m1)

summary(linearRidge(fFF, data = dtWide[1:1000], lambda = 8))

dtWide[, hCT := 0]
dtWide = dtWide[complete.cases(dtWide)]
S = 100
# S2 = 
N = nrow(dtWide)
fFF = formula(CT ~ -1 + . - Carry_1 - Carry_2 - Carry_3 - Carry_4 - Carry_5 - date - hCT - t1)
# colnames(dtWide)
# fFF = formula(CT ~ -1 + MKT + SMB + HML)
for (i in 1:(N %/% S)){
  print(i)
  indTrain = ((i - 1) * S + 1) : min((i * S), N)
  model = tryCatch(
    linearRidge(fFF, data = dtWide[indTrain]),
    error = function(e) {
      print(e)
#       browser()
      model
    })
#   model = linearRidge(fFF, data = dtWide[indTrain])
  indPredict = ((i) * S + 1) : min(((i + 1) * S), N)
  dtWide[indPredict, hCT := CT - predict(model, newdata = .SD)]
  
}
# i = 25
# dtWide[is.na(hCT)]
model = linearRidge(fFF, data = dtWide)
dtWide[, hCT := CT - predict(model, newdata = .SD)]

m1 = lm(hCT ~ -1 + . - Carry_1 - Carry_2 - Carry_3 - Carry_4 - Carry_5 - date - hCT - t1 - CT, dtWide)
summary(m1)
m1 = lm(fFF, dtWide)
summary(m1)

dtWide[, mean(hCT, na.rm = TRUE) / sd(hCT, na.rm = TRUE) * sqrt(252)]
dtWide[, mean(CT, na.rm = TRUE) / sd(CT, na.rm = TRUE) * sqrt(252)]

sum(coef(model))

mean(log(1+dtWide$MKT/100)) * 252
mean(log(1+dtWide$SMB/100)) * 252
mean(log(1+dtWide$HML/100)) * 252
mean(dtWide$SMB)
mean(dtWide$HML)

