options(warn=-1)
library(quantmod)
library(xts)
library(tseries)
library(quantmod)
library(fUnitRoots)
library(urca)


#download do csv via url
url <- "https://www.alphavantage.co/query?
function=FX_INTRADAY&from_symbol=EUR&to_symbol=SGD&interval=5min&apikey=G4WQU6VUA9GXX6AZ&outputsize=full&datatype=csv"
setwd(file.path("C:/Users/Luciano/Desktop/Quantitative Analysis"))
local <- file.path("intradaydb", "EURSGD5")
download.file(url,local)


#input dataframes
dfy = read.csv(file.choose(), header = TRUE, sep = ",") 
dfx = read.csv(file.choose(), header = TRUE, sep = ",")
#selecionar um dia específico do df:
#atimeframe <- subset(df, timestamp >= as.POSIXct ("2018-11-06 00:00:00") & timestamp <= as.POSIXct ("2018-11-06 14:15:00"))


#order column
dfy$timestamp <- as.POSIXct (dfy$timestamp)
dfy <- dfy[nrow(dfy):1,]

dfx$timestamp <- as.POSIXct (dfx$timestamp)
dfx <- dfx[nrow(dfx):1,]


#convert to xts
xtsy <- xts(dfy[,-1], order.by=dfy[,1])
xtsx <- xts(dfx[,-1], order.by=dfx[,1])
summary(xtsy)


#rename columns xts
colnames(xtsy) = c("y.open","y.high","y.low","y.close")
colnames(xtsx) = c("x.open","x.high","x.low","x.close")


#merge xts y and x
pair <- na.omit(merge(xtsy$y.close, xtsx$x.close, all=FALSE))
#merge in dataframe
pairs <- as.data.frame(pair)


# Plot par price
plot.zoo(pair, plot.type = "single", col = c("red", "blue"), xlab = "Date", ylab = "Stock Price")
# Plot price diff
plot.zoo(pair[,1] - pair[,2], main = "Price Differences", lwd=1, ylab="Y-X", xlab= "Date") 


#Correlation
dfcor = as.data.frame(pair$y.close)
dfcor$x.close = pair$x.close
dfcor$ylag = lag(pair$y.close, k=1)
dfcor$xlag = lag(pair$x.close, k=1)
dfcor$deltay = ((dfcor$y.close/dfcor$ylag)-1)*100
dfcor$deltax = ((dfcor$x.close/dfcor$xlag)-1)*100
correl = cor(na.omit(dfcor$deltay), na.omit(dfcor$deltax))
correl


# Linear Regression Y with Date 
plot(index(pair$y.close), pair$y.close, type="l", lwd=1, las=1, ylab="Y", xlab= "Date")
m <- lm(pair$y.close ~ index(pair$y.close))
abline(m, col="blue", lty=2, lwd=1)  
# Linear Regression X with Date 
plot(index(pair$x.close), pair$x.close, type="l", lwd=1, las=1, ylab="X", xlab= "Date")
m <- lm(pair$x.close ~ index(pair$x.close))
abline(m, col="red", lty=2, lwd=1)  


# Test Dickey-Fuller(ADF)
cat("Date range is", format(start(pair)), "to", format(end(pair)), "\n")
modelADF <- lm(pair$y.close ~ pair$x.close)
summary(modelADF)
b <- modelADF$coefficients[2]
r <- modelADF$residuals
# index by number, since we won't always know the colnames
plot(pairs$x.close, pairs$y.close, ylab = "Y", xlab = "X")
abline(modelADF)
cat("Assumed hedge ratio is", b, "\n")
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
model <- lm(pairs$y.close ~ pairs$x.close, data=pairs)
beta <- model$coefficients[2]
residual <- resid(model)
# Plot dispersion graph and adjusted line
plot(pairs$x.close, pairs$y.close, ylab = "Y", xlab = "X")
abline(model)
#Plot Residuals and Standard deviation
plot.zoo(residual, xlab = 'Periods', ylab = 'Z')
dp = sd(residual)
abline(h = 2 * dp, col= "blue")
abline(h = 0, col= "green", lty=2)
abline(h = -2 * dp, col="blue")


# Ornstein-Uhlenbeck - Calculate half life of mean reversion
y <- residual
y.lag <- lag(y, -1)
delta.y <- diff(y)
halflife <- as.data.frame(cbind(y, y.lag, delta.y))
halflife <- halflife[-1 ,] #remove first row with NAs
regress.results <- lm(delta.y ~ y.lag, data = halflife)
lambda <- summary(regress.results)$coefficients[2]
half.life <- -log(2)/lambda
half.life
