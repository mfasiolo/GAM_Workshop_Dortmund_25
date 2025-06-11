library(testGam)
data("gefcom_small")
head(gefcom_small)  

plot(gefcom_small$NetDemand)

train <- gefcom_small[gefcom_small$Year < 2011, ]
test <- gefcom_small[gefcom_small$Year == 2011, ]

library(mgcv)
fit1 <- gam(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                      wM + wM_s95 + s(Posan, bs = "cc"), 
            data = train)

plot(fit1) 

plot(fit1, all.terms = TRUE, pages = 1)

plot(train$wM, residuals(fit1))

fit2 <- gam(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                       s(wM) + s(wM_s95) + s(Posan, bs = "cc"), 
             data = train)

plot(train$wM, residuals(fit2))

gam.check(fit2)

fit3 <- gam(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                       s(wM, k = 15) + s(wM_s95) + s(Posan, bs = "cc"), 
             data = train)

AIC(fit1, fit2, fit3)

summary(fit3)

qq.gam(fit3, pch = 1)

preds <- predict(fit3, newdata = test)
head(preds)

plot(test$NetDemand)
lines(preds, col = 2)

