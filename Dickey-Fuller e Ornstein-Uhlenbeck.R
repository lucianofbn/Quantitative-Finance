library(quantmod)
library(xts)
library(tseries)
library(quantmod)
library(fUnitRoots)
library(urca)

# Import Data
from.dat <- as.Date("01/01/2018", format="%d/%m/%Y") 
to.dat <- as.Date("31/10/2018", format="%d/%m/%Y") 
getSymbols("EURUSD=X", src="yahoo", from = from.dat, to = to.dat)
getSymbols("GBPUSD=X", src="yahoo", from = from.dat, to = to.dat)
periodicity(`EURUSD=X`)
periodicity(`GBPUSD=X`)


# Select Data Y-X
pair <- merge(Ad(`GBPUSD=X`), Ad(`EURUSD=X`), all=FALSE)
# Rename columns xts
colnames(pair) = c("y","x")
# Transform to dataframe
pairs <- as.data.frame(pair)
# Rename columns datafrafe
names(pairs) = c("y", "x")

#correl
cor(pair$x, pair$y)
# Plot par price
plot.zoo(pair, plot.type = "single", col = c("red", "blue"), xlab = "Date", ylab = "Stock Price")
# Plot price diff
plot.zoo(pair[,1] - pair[,2], main = "Price Differences", lwd=1, ylab="Y-X", xlab= "Date") 


# Linear Regression EURUSD with Date 
plot(index(pair$y), pair$y, type="l", lwd=1, las=1, ylab="Y", xlab= "Date")
m <- lm(pair$y ~ index(pair$y))
abline(m, col="red", lty=2, lwd=1)  


# Test Dickey-Fuller(ADF)
cat("Date range is", format(start(pair)), "to", format(end(pair)), "\n")
modelADF <- lm(pair$y ~ pair$x)
summary(modelADF)
b <- modelADF$coefficients[2]
r <- modelADF$residuals
cat("Assumed hedge ratio is", b, "\n")
# index by number, since we won't always know the colnames
plot(pairs$x, pairs$y, ylab = "EURUSD", xlab = "GBPUSD")
abline(modelADF)
sprd <- pair[,1] - b*pair[,2]
ht <- adf.test(sprd, alternative="stationary", k=0)
cat("ADF p-value is", ht$p.value, "\n")
plot.zoo(sprd, type="l", ylab="Z", xlab= "Date")
# Result ADF
summary(ur.df(sprd, type="none"))
adf.test(sprd, alternative = "stationary")
if (ht$p.value < 0.05) {
  cat("The spread is likely mean-reverting\n")
} else {
  cat("The spread is not mean-reverting.\n")
}


# Linear regression in dataframe
model <- lm(pairs$y ~ pairs$x, data=pairs)
beta <- model$coefficients[2]
residual <- resid(model)
# Plot dispersion graph and adjusted line
plot(pairs$x, pairs$y, ylab = "Y", xlab = "X")
abline(model)
#Plot Residuals and Standard deviation
plot.zoo(residual, col = 'navyblue', xlab = 'Periods', ylab = 'Z', ylim = c(-0.04, 0.04))
dp = sd(residual)
abline(h = 2 * dp, col= "red")
abline(h = 0, col= "blue", lty=2)
abline(h = -2 * dp, col="green")


# ADF-TEST
# Z <- pair[,1] - beta*pair[,2]  
adf.test(residual, alternative = "stationary", k=0)
summary(ur.df(residual, type="none")) #teste aumentado apenas para regressões múltiplas
# Result
adf.test(residual, alternative = "stationary")
summary(ur.df(residual, type="none")) #teste aumentado apenas para regressões múltiplas
# Plot Residual with real scale
plot.zoo(residual, type="l", ylab="Z", xlab="Date")
abline(h = 2 * dp, col= "red")
abline(h = 0, col= "blue", lty=2)
abline(h = -2 * dp, col="green")


# Ornstein-Uhlenbeck - Calculate half life of mean reversion
y <- residual
y.lag <- lag(y, -1)
delta.y <- diff(y)
df <- as.data.frame(cbind(y, y.lag, delta.y))
df <- df[-1 ,] #remove first row with NAs
regress.results <- lm(delta.y ~ y.lag, data = df)
lambda <- summary(regress.results)$coefficients[2]
half.life <- -log(2)/lambda
half.life