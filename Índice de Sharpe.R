### Índice de Sharpe ###
# Avaliação de perfomance;
# Sharpe = media(R+)/desv.padrão(R+);
# Quanto maior o risco, menor o Sharper, ou seja maior é melhor;
# Interessante para comparar fundos de investimentos.

library(quantmod)
options("getSymbols.warning4.0" = F,
        "getSymbols.auto.assign" = F)

#Loading S&P 500 data
SPY <- suppressWarnings(getSymbols(c("SPY"), from = "2012-01-01"))
SPY <- as.numeric(SPY$SPY.Close)

#Simulating other curves
set.seed(123)

#Time index
t <- 1:(length(SPY)-1)

#Initial Balance
Vt <- c(rep(1000, length(t)))

#Benchmark Return - S&P
Rb <- rep(NA, length(t))
for(i in 2:length(t)){
  Rb[i] <- (SPY[i]/SPY[i-1])-1
}

#Benchmark Equity - based on S&P Returns
Eb <- rep(NA, length(t))
Eb[1] <- Vt[1]
for(i in 2:length(t)){
  Eb[i] <- Eb[i-1]*(1 + Rb[i])
}

#Random Return Series 1
Rt <- rep(NA, length(t))
for(i in 2:length(t)){
  Rt[i] <- Rb[i] + rnorm(n = 1,
                         mean = 0.24/length(t),
                         sd = 2.5*sd(Rb,na.rm = T))
}

#Random Return Series 2
Rt2 <- rep(NA, length(t))
for(i in 2:length(t)){
  Rt2[i] <- Rb[i] + rnorm(n = 1,
                          mean = 0.02/length(t),
                          sd = 0.75*sd(Rb,na.rm = T))
}

#Equity curve from Random Series 1 
Et <- rep(NA, length(t))
Et[1] <- Vt[1]
for(i in 2:length(t)){
  Et[i] <- Et[i-1]*(1 + Rt[i])
}

#Equity curve from Random Series 2 
Et2 <- rep(NA, length(t))
Et2[1] <- Vt[1]
for(i in 2:length(t)){
  Et2[i] <- Et2[i-1]*(1 + Rt2[i])
}

#Plotting Equity Curves
plot(y = Et, x = t, type = "l", col=1,
     xlab = "Time",
     ylab = "Equity ($)",
     main = "Random Equity Curves and S&P500 Equity")
grid()
abline(h=10000)
lines(y = Et2, x = t, col = 2)
lines(y = Eb, x = t, col = 8)
legend(x = "topleft", col = c(1,2,8), lwd = 2,
       legend = c("Curve 1", "Curve 2", "S&p 500"))

#Sharpe Ratio
SR <- mean(Rt, na.rm = T)/sd(Rt, na.rm = T)
SR2 <- mean(Rt2, na.rm = T)/sd(Rt2, na.rm = T)
SRb <- mean(Rb, na.rm = T)/sd(Rb, na.rm = T)

#Plotting Equity Curves adding Sharp Ratio
plot(y = Et, x = t, type = "l", col=1,
     xlab = "Time",
     ylab = "Equity ($)",
     main = "Random Equity Curves and S&P500 Equity")
grid()
abline(h=10000)
lines(y = Et2, x = t, col = 2)
lines(y = Eb, x = t, col = 8)
legend(x = "topleft", col = c(1,2,8), lwd = 2,
       legend = c(paste0("SR = ", round(SR,3)),
                  paste0("SR2 = ", round(SR2,3)),
                  paste0("SRb = ", round(SRb,3))
                  )
       )
