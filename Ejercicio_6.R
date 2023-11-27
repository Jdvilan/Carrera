# 6. Modelar un portafolio con al menos 10 activos. Explicar cada paso del 
# procedimiento.

# A) Importar librerias

library(quantmod)
library(fPortfolio)
library(dygraphs)

# B) Importar series de precios de activos

precios <- c("AAPL","MSFT","AMZN","TSLA","WMT","JNJ","HD","COST","KO","NFLX")
getSymbols(precios,from = "2022-01-01", periodicity = "daily")

# C) Generar la grafica con los datos historicos de los activos

Historico <- cbind(AAPL$AAPL.Close,MSFT$MSFT.Close,AMZN$AMZN.Close,
                    TSLA$TSLA.Close,WMT$WMT.Close,JNJ$JNJ.Close,HD$HD.Close,
                    COST$COST.Close,KO$KO.Close,NFLX$NFLX.Close)
dygraph(Historico)%>%
dySeries("AAPL.Close", label = "Apple") %>%
dySeries("MSFT.Close", label = "Microsoft") %>%
dySeries("AMZN.Close", label = "Amazon") %>%
dySeries("TSLA.Close", label = "Tesla") %>%
dySeries("WMT.Close", label = "Walmart") %>%
dySeries("JNJ.Close", label = "Johnson") %>%
dySeries("HD.Close", label = "Depot") %>%
dySeries("COST.Close", label = "Costco") %>%
dySeries("KO.Close", label = "Coca") %>%
dySeries("NFLX.Close", label = "Netflix")

# D) Ajuste de los datos 

R.aapl <- diff(log(AAPL$AAPL.Close))*100
R.msft <- diff(log(MSFT$MSFT.Close))*100
R.amzn <- diff(log(AMZN$AMZN.Close))*100
R.tsla <- diff(log(TSLA$TSLA.Close))*100
R.wmt <- diff(log(WMT$WMT.Close))*100
R.jnj <- diff(log(JNJ$JNJ.Close))*100
R.hd <- diff(log(HD$HD.Close))*100
R.cost <- diff(log(COST$COST.Close))*100
R.ko <- diff(log(KO$KO.Close))*100
R.nflx <- diff(log(NFLX$NFLX.Close))*100

R.aapl <- R.aapl[-1,]
R.msft <- R.msft[-1,]
R.amzn <- R.amzn[-1,]
R.tsla <- R.tsla[-1,]
R.wmt <- R.wmt[-1,]
R.jnj <- R.jnj[-1,]
R.hd <- R.hd[-1,]
R.cost <- R.cost[-1,]
R.ko <- R.ko[-1,]
R.nflx <- R.nflx[-1,]

# E) Grafica de volatilidad de cada activo

Volatilidad <- cbind(R.aapl,R.msft,R.amzn,R.tsla,R.wmt,R.jnj,R.hd,R.cost,R.ko,R.nflx)
dygraph(Volatilidad)%>%
dySeries("AAPL.Close", label = "Apple") %>%
dySeries("MSFT.Close", label = "Microsoft") %>%
dySeries("AMZN.Close", label = "Amazon") %>%
dySeries("TSLA.Close", label = "Tesla") %>%
dySeries("WMT.Close", label = "Walmart") %>%
dySeries("JNJ.Close", label = "Johnson") %>%
dySeries("HD.Close", label = "Depot") %>%
dySeries("COST.Close", label = "Costco") %>%
dySeries("KO.Close", label = "Coca") %>%
dySeries("NFLX.Close", label = "Netflix")

# F) Retorno y varianza del portafolio

mu <- cbind(mean(R.aapl),mean(R.msft),mean(R.amzn),mean(R.tsla),mean(R.wmt),
            mean(R.jnj),mean(R.hd),mean(R.cost),mean(R.ko),mean(R.nflx))
colnames(mu) <- c("aapl","msft","amzn","tsla","wmt","jnj","hd","cost",
                  "ko","nflx");mu

Data <- data.frame(R.aapl,R.msft,R.amzn,R.tsla,R.wmt,R.jnj,R.hd,R.cost,R.ko,
                   R.nflx);Data
S <- cov(Data);S

# G) Modelo de minima varianza

ones <- matrix(c(rep(1,10)),1);ones
w <- c(ones%*%solve(S))/c(ones%*%solve(S)%*%t(ones));w

W <- matrix(w,1)
colnames(W) <- c("aapl","msft","amzn","tsla","wmt","jnj","hd","cost",
                 "ko","nflx");W

mu_R <- W%*%t(mu);mu_R
s_R <- sqrt(W%*%S%*%t(W));s_R
