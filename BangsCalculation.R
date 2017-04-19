#Linear fit for Bangs Beads


year <- c(2000 ,   2001  ,  2002  ,  2003 ,   2004)
rate <- c(9.34 ,   8.50  ,  7.62  ,  6.93  ,  6.60)
plot(year,rate,
     main="Commercial Banks Interest Rate for 4 Year Car Loan",
     sub="http://www.federalreserve.gov/releases/g19/20050805/")
cor(year,rate)
fit <- lm(rate ~ year)
fit 
attributes(fit)
##The equation is 
#rate=(slope)year+(intercept)
#estimate for year 2015
fit$coefficients[[2]]*2015+fit$coefficients[[1]]

