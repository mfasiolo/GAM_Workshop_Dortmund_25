
library(testGam)
data(dbbmi)

plot(bmi ~ age, data = dbbmi)

set.seed(151)
dbbmi <- dbbmi[sample(1:nrow(dbbmi), 2000), ]
dbbmi <- dbbmi[order(dbbmi$age), ]

library(mgcViz)
fit1 <- gamV(bmi ~ s(age, bs = "ad", k = 20), 
             data = dbbmi, 
             aViz = list(nsim = 50))

check1D(fit1, "age") + l_gridCheck1D() 

check1D(fit1, "age") + l_gridCheck1D( sd )

fit2 <- gamV(list(bmi ~ s(age, bs = "ad", k = 20), # Location 
                      ~ s(age), # Scale
                      ~ 1,      # Skewness
                      ~ 1),     # Kurtosis
             data = dbbmi, 
             family = shash,
             aViz = list(nsim = 50))

check1D(fit2, "age") + l_gridCheck1D( sd )

check1D(fit2, "age") + l_gridCheck1D( gridFun = function(x) mean(x^3) ) 

fit3 <- gamV(list(bmi ~ s(age, bs = "ad", k = 20), ~ s(age), ~ s(age), ~ 1),   
             data = dbbmi, 
             family = shash,
             aViz = list(nsim = 50))

check1D(fit3, "age") + l_gridCheck1D( gridFun = function(x) mean(x^3) ) 

print(plot(fit3), pages = 1)

plot(bmi ~ age, data=dbbmi, col = "grey")
pr <- predict(fit3)
for(.q in c(0.01, 0.25, 0.5, 0.75, 0.99)){
  q_hat <- fit3$family$qf(.q, pr)
  lines(dbbmi$age, q_hat, col = 2)
}


################ SCM example with GEF14 data set ----------------------------

library(SCM)
library(mgcViz)

data(GEF14_d4)
print(head(GEF14_d4[ , c("year", "doy", "dow", "load_h17", "load_h20", "load24_h17", 
                         "load24_h20", "temp_h17", "temp_h20")]), digits = 2)

fit1 <- gam_scm(list(load_h17 ~ dow + s(doy, k = 15) + load24_h17 + s(temp_h17), 
                     load_h20 ~ dow + s(doy, k = 15) + load24_h20 + s(temp_h20)), 
                family = mvn_scm(d=2),
                data = GEF14_d4)
fit1 <- getViz(fit1, nsim = 25)

print(plot(fit1), pages = 1)

pred <- predict(fit1, type = "response")
head(pred)

check1D(fit1, "doy") + l_gridCheck1D(gridFun = function(.x){ 
  sd(.x[ , 1])
 }
)

check1D(fit1, "doy") + l_gridCheck1D(gridFun = function(.x){ 
  cor(.x)[1, 2]
 }
)

fit2 <- gam_scm(list(load_h17 ~ dow + s(doy, k = 15) + load24_h17 + s(temp_h17), 
                     load_h20 ~ dow + s(doy, k = 15) + load24_h20 + s(temp_h20), 
                     Th_11 ~ s(doy), 
                     Th_22 ~ s(doy), 
                     Th_12 ~ s(doy)), 
                family = mvn_scm(d=2), data = GEF14_d4)
fit2 <- getViz(fit2, nsim = 25)

check1D(fit2, "doy") + l_gridCheck1D(gridFun = function(.x){ 
  sd(.x[ , 1])
 }
)

check1D(fit2, "doy") + l_gridCheck1D(gridFun = function(.x){ 
  cor(.x)[1, 2]
 }
)

preds <- predict(fit2, type = "response")
head(preds)

print(plot(fit2), pages = 1)

plot(ALE(fit2, oind = 3, x = "doy", type = "response"))

plot(ALE(fit2, oind = 5, x = "doy", type = "response"))


# Visualise covariance
library(mvnfast)
ii <- 1
sig_1 <- sqrt(preds[ii, 3])
sig_2 <- sqrt(preds[ii, 4])
corr_12 <- preds[ii, 5]
x     <- seq(-2, 2, length.out = 100) * max(sqrt(preds[ , 3:4]))
y     <- seq(-2, 2, length.out = 100) * max(sqrt(preds[ , 3:4]))
mu    <- c(0, 0)
sigma <- matrix(c(sig_1^2, corr_12*sig_1*sig_2, corr_12*sig_1*sig_2, sig_2^2), nrow = 2)
f     <- function(x, y) dmvn(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)

contour(x, y, z, main = paste0("Day of year = ", GEF14_d4$doy[ii]), 
        xlab = "Demand at 5pm", ylab = "Demand at 8pm")
ii <- ii + 10


################# Stacking with gamFactory
library(gamFactory)
library(mgcViz)
library(dplyr)
library(magrittr)

data("smart_dat")

plot(exp(smart_dat$ldem), type = 'l')

head(round(smart_dat, 2)[ , c("ldem", "doy", "tod", "mu_g", "mu_l",
                              "lp_g", "lp_l", "mse_g_48", "mse_l_48", "test")])


train <- smart_dat[smart_dat$test == FALSE, ]
test <- smart_dat[smart_dat$test == TRUE, ]

plot(smart_dat$ldem, type = "l")
abline(v = nrow(train), lty = 2, col = 2, lwd = 2)

idx <- 2500:6000
plot(train$ldem[idx], type = "l", col = "grey")
lines(train$mu_g[idx], col = 4, lwd = 2)
lines(train$mu_l[idx], col = 2, lwd = 2)
legend(col = c(4, 2), legend = c("GAM", "Lagged Gaussian"), 
       x = "bottomleft", lty = 1, lwd = 2)

library(gamFactory)
library(mgcViz)
stack <- gam(list(ldem ~ s(mse_g_48, k = 5) + s(mse_l_48, k = 5)), 
             family = fam_stackProb(cbind(train$lp_g, train$lp_l)), 
             data = train)
stack <- getViz(stack)

head( predict(stack, type = "response") )

plot(ALE(stack, oind = 1, x = "mse_g_48", type = "response"))

plot(ALE(stack, oind = 1, x = "mse_l_48", type = "response"))

test$weights <- predict(stack, type = "response", newdata = test)

test$mu_s <- with(test, weights[ , 1] * mu_g + weights[ , 2] * mu_l)

par(mfrow = c(2, 1), mar = c(5.1, 4.1, 1.1, 1.1))
plot(test$ldem, type = "l", col = "grey", ylab = "ldem")
lines(test$mu_s, type = "l", col = 4, lwd = 2)
plot(test$weights[ , 1], col = 1, ylab = "weight", type = 'l')

mses <- test %>% group_by(doy) %>% summarise(mse_s = mean((ldem-mu_s)^2))
test <- test %>% left_join(mses)

colMeans(test[ , c("mse_g", "mse_l", "mse_s")])

