library(quantmod)

### Indicador - Estratégia da barra anterior ###
# -> Na compra: se os preços do dia romperem a máxima do dia anterior e sai no fechamento da barra
# -> Na venda: se os preços do dia romperem a mínima do dia anterior e sai no fechamento da barra

### Petrobrás ###

#Seleção do período de análise
startDate = as.Date("2017-01-01")
endDate = as.Date("2017-12-05")

#Seleção das ações
tickers = c('^BVSP', 'BTCUSD=X', 'PETR4.SA')

#Captura dos dados
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

#Visualizando Petrobrás
summary(PETR4.SA) 
PETR4.SA #format xts

#Plot, escala normal e log
chartSeries(PETR4.SA, TA=NULL)
chartSeries(PETR4.SA, TA=NULL, log.scale=TRUE)

#Convert xts to dataframe
PETR4 <- data.frame(PETR4.SA)

#Ajuste de colunas
names(PETR4) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

#Selecionado colunas
PETR4 <- PETR4[ , (names(PETR4)%in% c("Open", "High", "Low", "Close"))]

#Estratégia
PETR4$High_Shift <- PETR4$High #clone column
PETR4['High_Shift'] <- c(NA, head(PETR4['High_Shift'], dim(PETR4)[1] - 1)[[1]]) #desloca para pegar a max do dia anterior
PETR4$Low_Shift <- PETR4$Low
PETR4['Low_Shift'] <- c(NA, head(PETR4['Low_Shift'], dim(PETR4)[1] - 1)[[1]]) #desloca para pegar a min do dia anterior
PETR4$Bull <- ifelse(PETR4$High > PETR4$High_Shift, (PETR4$Close - PETR4$High_Shift)*100,0) #*100 = 1 LOTE
PETR4$Bull[is.na(PETR4$Bull)] <- 0
PETR4$Bear <- ifelse(PETR4$Low < PETR4$Low_Shift, (PETR4$Low_Shift - PETR4$Close)*100,0)
PETR4$Bear[is.na(PETR4$Bear)] <- 0
PETR4_Results <- cumsum(PETR4$Bull + PETR4$Bear)
plot(index(PETR4_Results),coredata(PETR4_Results),
    type = 'l',
    col = "blue",
    main = "PETR4 - Estratégia da Barra Anterior",
    xlab = "Dias em 2017",
    ylab = "Retorno em Reais"
)
text(175,50,paste("Retorno total: ","R$", round(tail(PETR4_Results,1),2), " ")) #custos operacionais não inclusos

### BITCOIN ###

#Seleção do período de análise
startDate = as.Date("2017-01-01")
endDate = as.Date("2017-12-05")

#Seleção das ações
tickers = c('^BVSP', 'BTCUSD=X', 'PETR4.SA')

#Captura dos dados
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

#Visualizando Bitcoin
summary(`BTCUSD=X`) 
`BTCUSD=X` #format xts

#Plot, escala normal e log
chartSeries(`BTCUSD=X`, TA=NULL)
chartSeries(`BTCUSD=X`, TA=NULL, log.scale=TRUE)

#Convert xts to dataframe
BTC <- data.frame(`BTCUSD=X`)

#Ajuste de colunas
names(BTC) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

#Selecionado colunas
BTC <- BTC[ , (names(BTC)%in% c("Open", "High", "Low", "Close"))]

#Estratégia
BTC$High_Shift <- BTC$High #clone column
BTC['High_Shift'] <- c(NA, head(BTC['High_Shift'], dim(BTC)[1] - 1)[[1]]) #desloca para pegar a max do dia anterior
BTC$Low_Shift <- BTC$Low
BTC['Low_Shift'] <- c(NA, head(BTC['Low_Shift'], dim(BTC)[1] - 1)[[1]]) #desloca para pegar a min do dia anterior
BTC$Bull <- ifelse(BTC$High > BTC$High_Shift, (BTC$Close - BTC$High_Shift)*0.1,0) #*0.1 = 1 LOTE
BTC$Bull[is.na(BTC$Bull)] <- 0
BTC$Bear <- ifelse(BTC$Low < BTC$Low_Shift, (BTC$Low_Shift - BTC$Close)*0.1,0)
BTC$Bear[is.na(BTC$Bear)] <- 0
BTC_Results <- cumsum(BTC$Bull + BTC$Bear)
plot(index(BTC_Results),coredata(BTC_Results),
     type = 'l',
     col = "blue",
     main = "BITCOIN - Estratégia da Barra Anterior",
     xlab = "Dias em 2017",
     ylab = "Retorno em Dólares"
)
text(175,50,paste("Retorno total: ","$", round(tail(BTC_Results,1),2), " "))

### IBOVESPA ###

#Seleção do período de análise
startDate = as.Date("2017-01-01")
endDate = as.Date("2017-12-05")

#Seleção das ações
tickers = c('^BVSP', 'BTCUSD=X', 'PETR4.SA')

#Captura dos dados
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

#Visualizando Bitcoin
summary(BVSP) 
BVSP #format xts

#Plot, escala normal e log
chartSeries(BVSP, TA=NULL)
chartSeries(BVSP, TA=NULL, log.scale=TRUE)

#Convert xts to dataframe
BVSP <- data.frame(BVSP)

#Ajuste de colunas
names(BVSP) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

#Selecionado colunas
BVSP <- BVSP[ , (names(BVSP)%in% c("Open", "High", "Low", "Close"))]

#Estratégia
BVSP$High_Shift <- BVSP$High #clone column
BVSP['High_Shift'] <- c(NA, head(BVSP['High_Shift'], dim(BVSP)[1] - 1)[[1]]) #desloca para pegar a max do dia anterior
BVSP$Low_Shift <- BVSP$Low
BVSP['Low_Shift'] <- c(NA, head(BVSP['Low_Shift'], dim(BVSP)[1] - 1)[[1]]) #desloca para pegar a min do dia anterior
BVSP$Bull <- ifelse(BVSP$High > BVSP$High_Shift, (BVSP$Close - BVSP$High_Shift)*0.2,0) #*0.2 = 1 mini-índice 20 centavos
BVSP$Bull[is.na(BVSP$Bull)] <- 0
BVSP$Bear <- ifelse(BVSP$Low < BVSP$Low_Shift, (BVSP$Low_Shift - BVSP$Close)*0.2,0)
BVSP$Bear[is.na(BVSP$Bear)] <- 0
BVSP_Results <- cumsum(BVSP$Bull + BVSP$Bear)
plot(index(BVSP_Results),coredata(BVSP_Results),
     type = 'l',
     col = "blue",
     main = "IBOV - Estratégia da Barra Anterior",
     xlab = "Dias em 2017",
     ylab = "Retorno em Reais"
)
text(175,50,paste("Retorno total: ","R$", round(tail(BVSP_Results,1),2), " ")) #descontar gastos operacionais

