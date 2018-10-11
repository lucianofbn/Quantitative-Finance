# > Bitcoin Machine Learning - Neural Network

library(quantmod)   
#Selecao do Periodo
startDate = as.Date("2017-01-01")   
endDate = as.Date("2018-04-21")  
#Selecao dos Ativos
tickers = c('BTCUSD=X')  
#Captura dos dados  
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)  

bitcoin <- as.data.frame(`BTCUSD=X`)
names(bitcoin) <- c("OPEN","HIGH","LOW","CLOSE","ADJ","VOL")

price <- (bitcoin$CLOSE)
HLC <- matrix(c(bitcoin$HIGH, bitcoin$LOW, bitcoin$CLOSE),nrow=length(bitcoin$HIGH))

# calculate log returns
bitcoin.lr<-diff(log(price))

#TTR package 
library(TTR)

#Technical indicators 
rsi <- RSI(price)
MACD <- MACD(price)
macd <- MACD[,1]
will <- williamsAD(HLC)
cci <- CCI(HLC)
STOCH <- stoch(HLC)
stochK <- STOCH[,1]
stochD <- STOCH[,1]

#Create the Input and Target matrix for training and validation dataset
Input<-matrix(c(rsi[1:250], cci[1:250], macd[1:250], will[1:250], stochK[1:250], stochD[1:250]),nrow=250)
Target<-matrix(c(bitcoin.lr[2:251]), nrow=250)

trainingdata <- cbind(Input,Target)
colnames(trainingdata) <- c("RSI","CCI","MACD","WILL","STOCHK","STOCHD", "Return")
trainingdata[is.na(trainingdata)] <- 0


#caret package
library(caret)

# split the dataset 90-10% ratio
trainIndex <- createDataPartition(bitcoin.lr[2:251], p=.9, list=F)
bitcoin.train <- trainingdata[trainIndex, ]
bitcoin.test <- trainingdata[-trainIndex, ]

#nnet package
library(nnet)

# derive the best neural network model using rmse criteria 
set.seed(17)
best.network<-matrix(c(5,0.5))
best.rmse <- 1
for (i in 3:20) for (j in 1:10) {
  bitcoin.fit <- nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD, data = bitcoin.train, 
                      maxit=1000, size=i, decay=0.01*j, linout = 1)
  bitcoin.predict <- predict(bitcoin.fit, newdata = bitcoin.test)
  bitcoin.rmse <- sqrt(mean((bitcoin.predict - bitcoin.lr[228:251])^2))
  print(bitcoin.rmse)
  if(is.na(bitcoin.rmse)){
    bitcoin.rmse <- 1}
  if (bitcoin.rmse < best.rmse) {
    best.network[1,1]<-i
    best.network[2,1]<-j
    best.rmse <- bitcoin.rmse 
  }
}

# create the Input and Target matrix for test
InputTest<-matrix(c(rsi[251:338], cci[251:338], macd[251:338], will[251:338], stochK[251:338], stochD[251:338]),nrow=88)
TargetTest<-matrix(c(bitcoin.lr[252:339]), nrow=88)

Testdata <- cbind(InputTest,TargetTest)
colnames(Testdata) <- c("RSI","CCI","MACD","WILL","STOCHK","STOCHD", "Return")
Testdata[is.na(Testdata)] <- 0
# fit the best model on test data
bitcoin.fit <- nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD, data = trainingdata, 
                    maxit=1000, size=best.network[1,1], decay=0.1*best.network[2,1], linout = 1) 

bitcoin.predict1 <- predict(bitcoin.fit, newdata = Testdata)

# repeat and average the model 20 times  
for (i in 1:20) {
  bitcoin.fit <- nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD, data = trainingdata, 
                      maxit=1000, size=best.network[1,1], decay=0.1*best.network[2,1], linout = 1) 
  
  bitcoin.predict<- predict(bitcoin.fit, newdata = Testdata)
  bitcoin.predict1<-(bitcoin.predict1+bitcoin.predict)/2
}

# calculate the buy-and-hold benchmark strategy and neural network profit on the test dataset
money <- matrix(0,88)
money2 <- matrix(0,88)
money[1,1] <- 100
money2[1,1] <- 100

for (i in 2:88) {
  if (bitcoin.predict1[i-1]<0) {
    direction1<--1  
  } else {
    direction1<-1}
  if (TargetTest[i-1]<0) {
    direction2<--1  
  } else {
    direction2<-1 }
  if ((direction1-direction2)==0) {
    money[i,1]<-money[i-1,1]*(1+abs(TargetTest[i-1]))  
  } else {
    money[i,1] <- money[i-1,1]*(1-abs(TargetTest[i-1])) }
  money2[i,1] <- 100*(price[252+i-1]/price[252])
}

#plot benchmark and neural network profit on the test dataset
x<-1:89
matplot(cbind(money, money2), type = "l", xaxt = "n", ylab = "Retorno",main = "Redes Neurais - BTC")
legend("topright", legend = c("Neural network","Buy and Hold"), pch = 19, col = c("black", "red"))
axis(1, at = c(45), lab  = c("2018"))
