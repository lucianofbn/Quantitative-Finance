library(quantmod)

#Select time for analysis
startDate = as.Date("2017-01-01")
endDate = as.Date("2017-11-22")

#Select actives
tickers = c("EURUSD=X", 'BTCUSD=X')

#Capturing data
getSymbols(tickers, src="yahoo", from = startDate, to = endDate)

#Calculations of returns
EURUSD_RET <- dailyReturn(`EURUSD=X`)
BTC_RET <- dailyReturn(`BTCUSD=X`)

#Auxiliary functions
index(EURUSD_RET) #retorna as datas(que estão como índice das linhas)
coredata(EURUSD_RET) #retorna os dados principais, ou seja, os valores

length(EURUSD_RET) #230
length(BTC_RET) #233

#Número de dias alinhados entre EURUSD e BTCUSD
length(EURUSD_RET[index(EURUSD_RET)%in%index(BTC_RET)]) #230

### Para plotar os mesmos dias dos retornos
#Atribui os valores que são conjuntos
EURUSD_SAME <- EURUSD_RET[index(EURUSD_RET)%in%index(BTC_RET)] 
BTC_SAME <- BTC_RET[index(BTC_RET)%in%index(EURUSD_RET)] 

plot(coredata(BTC_SAME), coredata(EURUSD_SAME)) #check correlation

fit_RET_EURUSD_BTC <- lm(EURUSD_SAME~BTC_SAME)
summary(fit_RET_EURUSD_BTC)
#Multiple R-squared:  0.0003136 = don't exist correlation
abline(h=median(EURUSD_SAME), col = "blue")
abline(v=median(BTC_RET), col = "red")
abline(fit_RET_EURUSD_BTC, col="green")

#Cumulative return
EURUSD_ACCUM <- cumsum(EURUSD_RET)
plot(index(EURUSD_ACCUM), coredata(EURUSD_ACCUM), 
    type = 'l',
    main = 'EUR/USD acumulado'
)

BTC_ACCUM <- cumsum(BTC_RET)
plot(index(BTC_ACCUM), coredata(BTC_ACCUM), 
     type = 'l',
     main = 'BTC/USD acumulado'
)

#Return of mondays
BTC_SEG <- BTC_RET[weekdays(index(BTC_RET)) == "segunda-feira"]
plot(BTC_SEG)
BTC_ACCUM <- cumsum(BTC_SEG) #acumulado só nas segundas-ferias
plot(index(BTC_ACCUM), coredata(BTC_ACCUM), 
     type = 'l',
     main = 'BTC/USD acumulado'
)
       

