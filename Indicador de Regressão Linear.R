### Indicador de Regressão Linear

library(quantmod)   
#Seleção do perído de análise
startDate = as.Date("2017-01-01")   
endDate = as.Date("2017-12-27")  
#Seleção dos ativos  
tickers = c('^BVSP','BTCUSD=X','PETR4.SA','ITUB4.SA')  
#Captura dos dados  
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)  

#Cálculo dos retornos  
BVSP_RET <- dailyReturn(BVSP)  
BTC_RET <- dailyReturn(`BTCUSD=X`)  
PETR_RET <- dailyReturn(`PETR4.SA`) 
ITUB_RET <- dailyReturn(`ITUB4.SA`) 

#Atribui os valores que são conjuntos  
BVSP_SAME <- BVSP_RET[index(BVSP_RET)%in%index(ITUB_SAME)]  
BTC_SAME <- BTC_RET[index(BTC_RET)%in%index(ITUB_SAME)]  
PETR_SAME <- PETR_RET[index(PETR_RET)%in%index(ITUB_SAME)] 
ITUB_SAME <- ITUB_RET[index(ITUB_RET)%in%index(ITUB_SAME)] 

length(BVSP_SAME) #216  
length(BTC_SAME) #216
length(PETR_SAME) #216
length(ITUB_SAME) #216

BVSP <- data.frame(BVSP)
BTC <- data.frame(`BTCUSD=X`)
PETR4 <- data.frame(PETR4.SA)
ITUB4 <- data.frame(ITUB4.SA)

names(BVSP) <- c("Open","High","Low","Close","Volume","Adjusted")
BVSP <- BVSP[ , (names(BVSP) %in% c("Open","High","Low","Close"))]
names(BTC) <- c("Open","High","Low","Close","Volume","Adjusted")
BTC <- BTC[ , (names(BTC) %in% c("Open","High","Low","Close"))]
names(PETR4) <- c("Open","High","Low","Close","Volume","Adjusted")
PETR4 <- PETR4[ , (names(PETR4) %in% c("Open","High","Low","Close"))]
names(ITUB4) <- c("Open","High","Low","Close","Volume","Adjusted")
ITUB4 <- ITUB4[ , (names(ITUB4) %in% c("Open","High","Low","Close"))]

PETR4$Close_Shift <- PETR4$Close
PETR4['Close_Shift'] <- c(NA, head(PETR4['Close_Shift'], dim(PETR4)[1] - 1)[[1]])
PETR4$Return <- (PETR4$Close/PETR4$Close_Shift - 1)
PETR4$Return_Shift <- PETR4$Return
PETR4['Return_Shift'] <- c(NA, head(PETR4['Return_Shift'], dim(PETR4)[1] - 1)[[1]])

ITUB4$Close_Shift <- ITUB4$Close
ITUB4['Close_Shift'] <- c(NA, head(ITUB4['Close_Shift'], dim(ITUB4)[1] - 1)[[1]])
ITUB4$Return <- (ITUB4$Close/ITUB4$Close_Shift - 1)
ITUB4$Return_Shift <- ITUB4$Return
ITUB4['Return_Shift'] <- c(NA, head(ITUB4['Return_Shift'], dim(ITUB4)[1] - 1)[[1]])

BTC$Close_Shift <- BTC$Close
BTC['Close_Shift'] <- c(NA, head(BTC['Close_Shift'], dim(BTC)[1] - 1)[[1]])
BTC$Return <- (BTC$Close/BTC$Close_Shift - 1)
BTC$Return_Shift <- BTC$Return
BTC['Return_Shift'] <- c(NA, head(BTC['Return_Shift'], dim(BTC)[1] - 1)[[1]])


BVSP$Close_Shift <- BVSP$Close
BVSP['Close_Shift'] <- c(NA, head(BVSP['Close_Shift'], dim(BVSP)[1] - 1)[[1]])
BVSP$Return <- (BVSP$Close/BVSP$Close_Shift - 1)
BVSP$Return <- (BVSP$Close/BVSP$Close_Shift - 1)
BVSP$Return_Shift <- BVSP$Return
BVSP['Return_Shift'] <- c(NA, head(BVSP['Return_Shift'], dim(BVSP)[1] - 1)[[1]])


BVSP$Return_Petr4 <- PETR4$Return_Shift
BVSP$Return_BTC <- BTC$Return_Shift[index(BTC$Return_Shift)%in%index(BVSP
)] 
BVSP$Return_ <- ITUB4$Return_Shift[index(ITUB4$Return_Shift)%in%index(BVSP)] 
plot(BVSP$Return ,BVSP$Return_BTC)

fit_RET_BVSP_PETR <- lm(BVSP$Return ~ BVSP$Return_BTC + PETR4$Return_Shift + BVSP$Return_Shift + BVSP$Return_ITUB4)  
summary(fit_RET_BVSP_PETR)  
plot(fit_RET_BVSP_PETR)

y_predicted <- predict(fit_RET_BVSP_PETR,BVSP)

write.csv(BVSP,"BVSP.csv")