rm(list = ls())
gc()
#wczytanie bibliotek
library(quantmod)
library(dplyr)
library(magrittr)
library(nortest)
library(car)
library(tseries)
#wczytanie snp
getSymbols("^GSPC",
           src = "yahoo",
           from = Sys.Date() - 5 * 365,
           to = Sys.Date())
#wczytanie dax
getSymbols("^GDAXI",
           src = "yahoo",
           from = Sys.Date() - 5 * 365,
           to = Sys.Date())
#NASDAQ
getSymbols("^IXIC",
           src = "yahoo",
           from = Sys.Date() - 5 * 365,
           to = Sys.Date())
#dow jones industrial avrage
getSymbols("^DJI",
           src = "yahoo",
           from = Sys.Date() - 5 * 365,
           to = Sys.Date())
tickers <- list(GSPC, GDAXI, IXIC, DJI)

GSPC %<>% as.data.frame()
GDAXI %<>% as.data.frame()
IXIC %<>% as.data.frame()
DJI %<>% as.data.frame()

GSPC %<>% select(4) %>% transmute(pnl = diff(c(0, GSPC.Close))) %>% tail(-1)
GDAXI %<>% select(4) %>% transmute(pnl = diff(c(0, GDAXI.Close))) %>% tail(-1)
IXIC %<>% select(4) %>% transmute(pnl = diff(c(0, IXIC.Close))) %>% tail(-1)
DJI %<>% select(4) %>% transmute(pnl = diff(c(0, DJI.Close))) %>% tail(-1)
tickers <- list(GSPC, GDAXI, IXIC, DJI)
ticker_names <- c("GSPC", "GDAXI", "IXIC", "DJI")
PNL <- GSPC
for (i in 2:4){
  PNL %<>% merge(tickers[[i]], by = "row.names", all = TRUE)
  row.names(PNL) <- (PNL[, 1])
  PNL %<>% select(-1)
  colnames(PNL) <- ticker_names[1:i]
}


Pnl_GSPC <- pull(PNL, GSPC)
Pnl_GDAXI <- pull(PNL, GDAXI)
Pnl_IXIC <- pull(PNL, IXIC)
Pnl_DJI <- pull(PNL, DJI)

par(mfrow = c(1, 2))

empirical_VaR <- function(returns, n, alpha = 0.01) {
  new_returns <- embed(returns, n)
  return(apply(new_returns, 1, function(x) -sort(x)[floor(alpha * n) + 1]))
}

#plot(empirical_VaR(Pnl_GSPC,500))

#empirical VaR esitmator
empirical_VaR_for <- function(returns, n, alpha = 0.01) {
  caly_var <- c()
  for (i in (n + 1):length(returns)) {
    caly_var[i - n] <- -sort(returns[(i - n):i])[floor(alpha * n) + 1]
  }
  return(caly_var)
}


plot(empirical_VaR_for(Pnl_GSPC, 100))


normal_VaR_for <- function(returns, n, alpha = 0.01) {
  caly_var <- c()
  for (i in (n + 1):length(returns)) {
    caly_var[i - n] <- -(mean(returns[(i - n):i]) +
    sd(returns[(i - n):i]) * qnorm(alpha))
  }
  return(caly_var)
}

unbiased_normal_VaR <- function(returns, n, alpha = 0.01){
  caly_var_un <- c()
  for (i in (n + 1):length(returns)) {
    ret_from_period <- tail(returns[(i - n):i], n)
    caly_var_un[i-n] <- -(mean(ret_from_period) + sd(ret_from_period) *
     sqrt((n + 1) / n) * qt(alpha, n - 1))
  }
  return(caly_var_un)
}
