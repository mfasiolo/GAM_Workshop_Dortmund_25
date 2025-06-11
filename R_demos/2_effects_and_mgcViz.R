
library(testGam)
data("gefcom_small")
head(gefcom_small)  

train <- gefcom_small[gefcom_small$Year < 2011, ]
test <- gefcom_small[gefcom_small$Year == 2011, ]

library(mgcv)
fit1 <- gam(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                      wM + wM_s95 + s(Posan, bs = "cc"), 
            data = train)

plot(fit1, all.terms = TRUE, pages = 1)

class(fit1)

args(plot.gam)

library(mgcViz)
fit1 <- getViz(fit1, nsim = 100)

class(fit1)

print(plot(fit1, allTerms = TRUE), pages = 1)

plot(fit1, select = 1) + l_fitLine()

plot(fit1, select = 1) + l_fitDens() + l_fitLine()

plot(train$wM, residuals(fit1))

#### START: Only for demonstration
x <- train$wM
y <- residuals(fit1)
n <- 20
grid <- seq(min(x), max(x), length.out = n)
inX <- findInterval(x, grid, rightmost.closed = T)

plot(train$wM, residuals(fit1))
rug(grid, lwd = 2)

points(train$wM, residuals(fit1), col = inX)

grX <- tapply(x, inX, mean)   
grY <- tapply(y, inX, mean)

points(grX, grY, pch = 16, cex = 1.5)
#### END

check1D(fit1, "wM") + l_gridCheck1D()

fit2 <- gam(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                       s(wM) + s(wM_s95) + s(Posan, bs = "cc"), 
             data = train)

fit2 <- getViz(fit2, nsim = 100)

check1D(fit2, "wM") + l_gridCheck1D()

# gamV() = gam() + getViz()
fit2 <- gamV(formula = NetDemand ~ NetDemand.24 + Dow + Trend + 
                       s(wM) + s(wM_s95) + s(Posan, bs = "cc"), 
             data = train, 
             aViz = list(nsim = 100))


check(fit2)

qq(fit2)


######################################## Larynx cancer in Germany

library(testGam)
library(mgcv)
data("Larynx")

head(Larynx)

data("german.polys")
str(german.polys[1:5])

X <- t( sapply(german.polys, colMeans, na.rm=TRUE) )
Larynx$lo <- X[ , 1]
Larynx$la <- X[ , 2]

fit1 <- gamV(Y ~ s(x, k = 20),
             family = poisson, 
             data = Larynx, 
             aViz = list(nsim = 100))

check2D(fit1, x1 = Larynx$lo, x2 = Larynx$la) + l_gridCheck2D()

fit2 <- gamV(Y ~ s(x, k=20) + offset( log(E) ),
             family = poisson, 
             data = Larynx, 
             aViz = list(nsim = 100))
# E(Y) = exp( s(x) ) 
# E(Y) = exp( s(x) ) * Population = exp( s(x) + log(population) )

check2D(fit2, x1 = Larynx$lo, x2 = Larynx$la) + l_gridCheck2D()

fit3 <- gamV(Y ~ s(region, k = 200, bs="mrf", xt=list(polys=german.polys)) + 
                 s(x, k=20) + offset( log(E) ),
             family = poisson, 
             data = Larynx, 
             aViz = list(nsim = 100))

check2D(fit3, x1 = Larynx$lo, x2 = Larynx$la) + l_gridCheck2D()

plot(fit3, select = 1)

fitISO <- gamV(Y ~ s(lo, la, k = 200) + 
                 s(x, k=20) + offset( log(E) ),
             family = poisson, 
             data=Larynx)

plot(fitISO, select = 1)


plotRGL( sm(fitISO, 1), residuals = TRUE )
rglwidget()


