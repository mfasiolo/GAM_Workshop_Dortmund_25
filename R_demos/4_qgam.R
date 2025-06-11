library(MASS)
data(mcycle)
plot(mcycle)

library(qgam)
library(mgcViz)

fit09 <- qgam(accel ~ s(times, k=20, bs="ad"), 
              data = mcycle, 
              qu = 0.9)

pred <- predict(fit09, se = TRUE)

plot(mcycle, ylim = c(-140, 85))
lines(mcycle$times, pred$fit)
lines(mcycle$times, pred$fit + 2 * pred$se, col = 2)
lines(mcycle$times, pred$fit - 2 * pred$se, col = 2)

fit09_2 <- qgam(list(accel ~ s(times, k=20, bs="ad"),
                           ~ s(times)),
                 data = mcycle, 
                 qu = 0.9)

pred <- predict(fit09_2, se = TRUE)

plot(mcycle, ylim = c(-140, 90))
lines(mcycle$times, pred$fit)
lines(mcycle$times, pred$fit + 2 * pred$se, col = 2)
lines(mcycle$times, pred$fit - 2 * pred$se, col = 2)

class(fit09_2)

summary(fit09_2)

fit09_2 <- getViz(fit09_2, nsim = 100) # Note nsim does not make sense here! 

fitMQ <- mqgam(list(accel ~ s(times, k=20, bs="ad"),
                          ~ s(times)),
               data = mcycle, 
               qu = c(0.1, 0.25, 0.5, 0.75, 0.9)) 

qdo(fitMQ, 0.1, summary)

qdo(fitMQ, 0.1, plot)

fitMQ_viz <- getViz(fitMQ)

plot(fitMQ_viz)

plot(mcycle, ylim = c(-150, 90))
for(ii in 1:5) lines(mcycle$times, predict(fitMQ_viz[[ii]]), col = 2)


################### Modelling Rainfall
library(testGam)
data("parana")
head(parana)

plot(parana[parana$TIME==1, ]$LO, parana[parana$TIME==1, ]$LA, xlab = "LO", ylab = "LA") 

library(mgcViz)
fit <- qgamV(PREC ~ s(LO, LA, k = 25) + s(seaDist) + s(TIME, bs = "cc") + s(EL),
             aQgam = list(discrete = TRUE),
             data = parana,
             qu = 0.8)

print(plot(fit), pages = 1)

qq(fit, CI = "normal") 

mean( (parana$PREC - fit$fitted.values) < 0 )

check1D(fit, x = "EL") + l_gridQCheck1D() 

check2D(fit, x1 = "LO", x2 = "LA") + l_gridQCheck2D() 


fitM <- mqgamV(PREC ~ s(LO, LA, k = 25) + s(seaDist) + s(TIME, bs = "cc") + s(EL),
               aQgam = list(discrete = TRUE),
               data = parana,
               qu = seq(0.1, 0.9, length.out = 5))

plot(fitM, select = 1)
print(plot(fitM, select = 2:4), pages = 1)

plotRGL(sm(fitM[[5]], 1))
# rglwidget()

fit4 <- qgamV(PREC ~ te(LO, LA, TIME, d = c(2, 1), k = c(20, 10), bs = c("tp", "cc")) + s(EL),
              aQgam = list(discrete = TRUE),
              data = parana,
              qu = 0.9)

plotSlice(sm(fit4, 1),
          fix = list("TIME" = round(seq(1, 53, length.out = 6))))

# clear3d()
plotRGL(sm(fit4, 1), fix = c("TIME" = 11))
# rglwidget()

