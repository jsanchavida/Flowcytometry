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

stds <- read.csv("/Users/josesancho/Documents/FlowCytometry/Standards.csv",header= TRUE, sep = ",")
stds <-stds[,-1]
lin <- lm(log(stds$MFI) ~ log(stds$ABC))
summary(lin)
plot (log(stds$MFI),log(stds$ABC))
abline(log(lin))

lin <- lm(stds$MFI ~ stds$ABC)
plot (stds$MFI, stds$ABC)
abline(lm(stds$MFI ~ stds$ABC))
data_MFI <- read.csv("/Users/josesancho/Documents/FlowCytometry/16APR13_MFI.csv",header= TRUE, sep = ",")
data_MFI
data_ABC <-do.call(cbind,lapply(data_MFI [,-1],function(x)(lin$coefficients[[2]]) * x + lin$coefficients[[1]]))


par(mgp=c(2,1,0), mar=c(3,3,1,1))
# Fit regression line
require(stats)
reg<-lm(dist ~ speed, data = cars)
coeff=coefficients(reg)
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(cars, main=eq)
abline(reg, col="blue")
fit1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
summary(fit1)
plot(Sepal.Length ~ Petal.Width, data = iris)
abline(fit1)
library(ggplot2)

ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 4),
                       "Intercept =",signif(fit$coef[[1]],4 ),
                       " Slope =",signif(fit$coef[[2]], 4),
                       " P =",signif(summary(fit)$coef[2,4], 4)))
}
ggplotRegression(lm(MFI ~ ABC, data = stds))

ggplot(stds, aes(x = MFI, y = ABC)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

