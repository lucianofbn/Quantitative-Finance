options(warn=-1)
library(quantmod)
library(xts)
library(tseries)
library(quantmod)
library(fUnitRoots)
library(urca)

# INPUT PAIR
Y <- "EURCZK=X"
X <- "EURSGD=X"


# Import Data
from.dat <- as.Date("01/01/2018", format="%d/%m/%Y") 
to.dat <- as.Date("02/11/2018", format="%d/%m/%Y") 
getSymbols(Y, src="yahoo", from = from.dat, to = to.dat)
getSymbols(X, src="yahoo", from = from.dat, to = to.dat)


# Select Column Data Y-X
pair <- na.omit(merge(Cl(`EURCZK=X`), Cl(`EURSGD=X`), all=FALSE))


# Rename columns xts
colnames(pair) = c("ycl","xcl")
# Transform to dataframe
pairs <- as.data.frame(pair)
# Rename columns datafrafe
names(pairs) = c("y", "x")


# Plot par price
plot.zoo(pair, plot.type = "single", col = c("red", "blue"), xlab = "Date", ylab = "Stock Price")
# Plot price diff
plot.zoo(pair[,1] - pair[,2], main = "Price Differences", lwd=1, ylab="Y-X", xlab= "Date") 


#Correlation
dfcor <- as.data.frame(pair$ycl)
dfcor$xcl <- pair$xcl
dfcor$ylag <- lag(pair$ycl, k=1)
dfcor$xlag <- lag(pair$xcl, k=1)
dfcor$deltay <- ((dfcor$ycl/dfcor$ylag)-1)*100
dfcor$deltax <- ((dfcor$xcl/dfcor$xlag)-1)*100
correl <- cor(na.omit(dfcor$deltay), na.omit(dfcor$deltax))
correl


# Linear Regression Y with Date 
plot(index(pair$ycl), pair$ycl, type="l", lwd=1, las=1, ylab="Y", xlab= "Date")
m <- lm(pair$y ~ index(pair$y))
abline(m, col="red", lty=2, lwd=1)  
# Linear Regression X with Date 
plot(index(pair$xcl), pair$xcl, type="l", lwd=1, las=1, ylab="X", xlab= "Date")
m <- lm(pair$xcl ~ index(pair$xcl))
abline(m, col="red", lty=2, lwd=1)  


# Test Dickey-Fuller(ADF)
cat("Date range is", format(start(pair)), "to", format(end(pair)), "\n")
modelADF <- lm(pair$y ~ pair$x)
summary(modelADF)
b <- modelADF$coefficients[2]
r <- modelADF$residuals
cat("Assumed hedge ratio is", b, "\n")
# index by number, since we won't always know the colnames
plot(pairs$x, pairs$y, ylab = "Y", xlab = "X")
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


# Linear regression in dataframe Y~X
model <- lm(pairs$y ~ pairs$x, data=pairs)
beta <- model$coefficients[2]
residual <- resid(model)
# Plot dispersion graph and adjusted line
plot(pairs$x, pairs$y, ylab = "Y", xlab = "X")
abline(model)
#Plot Residuals and Standard deviation
plot.zoo(residual, xlab = 'Periods', ylab = 'Z')
dp = sd(residual)
abline(h = 2 * dp, col= "blue")
abline(h = 0, col= "blue", lty=2)
abline(h = -2 * dp, col="blue")


# ADF-TEST
# Z <- pair[,1] - beta*pair[,2]  
adf.test(residual, alternative = "stationary", k=0)
summary(ur.df(residual, type="none")) #teste aumentado apenas para regressões múltiplas
# Result
adf.test(residual, alternative = "stationary")
summary(ur.df(residual, type="none")) #teste aumentado apenas para regressões múltiplas
# Plot Residual with real scale
plot.zoo(residual, type="l", ylab="Z", xlab="Periods")
abline(h = 2 * dp, col= "blue")
abline(h = 0, col= "blue", lty=2)
abline(h = -2 * dp, col="blue")


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