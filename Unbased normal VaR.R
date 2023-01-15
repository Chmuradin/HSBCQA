rm(list = ls())
gc()
#wczytanie bibliotek
library(quantmod)
library(dplyr)
library(magrittr)
library(nortest)
library(car)
library(tseries)
library(ggplot2)
library(ggpubr)
library(fBasics)

#data for S&P
end_date=as.Date("2022-12-31")
getSymbols("^GSPC", src = "yahoo", from = end_date - 5 * 365,to = end_date)
GSPC_closing_prices <- GSPC[, "GSPC.Close"]
PNL_GSPC <- c(diff(GSPC_closing_prices))

#data for GDAXI
getSymbols("^GDAXI", src = "yahoo", from = end_date - 5 * 365,to = end_date)
GDAXI_closing_prices <- GDAXI[, "GDAXI.Close"]
PNL_GDAXI <- c(diff(GDAXI_closing_prices))

PNL_GDAXI
#data for IXIC
getSymbols("^IXIC", src = "yahoo", from = end_date - 5 * 365,to = end_date)
IXIC_closing_prices <- IXIC[, "IXIC.Close"]
PNL_IXIC <- c(diff(IXIC_closing_prices))

#data for DJI
getSymbols("^DJI", src = "yahoo", from = end_date - 5 * 365,to = end_date)
DJI_closing_prices <- DJI[, "DJI.Close"]
PNL_DJI <- c(diff(DJI_closing_prices))

GDAXI
# NOTE HERE IS UNBASED NORMAL VAR!!!
normal_VaR <- function(returns, n, alpha = 0.01){
  new_returns <- embed(returns, n + 1)
  return(apply(new_returns, 1, function(x) -(mean(x,na.rm=TRUE) + sd(x,na.rm=TRUE) * 
                                               sqrt(( n + 1) / n) * qt(alpha, n-1))))
}
VR_GSPC = normal_VaR(PNL_GSPC, 250)
VR_GDAXI = normal_VaR(PNL_GDAXI, 250)
VR_IXIC = normal_VaR(PNL_IXIC, 250)
VR_DJI = normal_VaR(PNL_DJI, 250)

PNL_GSPC = PNL_GSPC[-c(1:250)]
PNL_GDAXI = PNL_GDAXI[-c(1:250)]
PNL_IXIC = PNL_IXIC[-c(1:250)]
PNL_DJI = PNL_DJI[-c(1:250)]
# P&L calculation for all indexes
T_stat <- function(PNL, VaR)
{
  y <- c()
  n <- length(PNL)
  for (i in 1:n) {
    y[i] <- PNL[i] + VaR[i]
  }
  T <- c()
  for (i in 1:n) {
    sm <- 0
    for (j in 1:i) 
    {
      if (is.na(y[j]) || y[j]<0)
      {
        sm <- sm + 1
      }
    }
    T[i] <- sm/i
  }
  return(T)
}

PV_GSPC = T_stat(PNL_GSPC,VR_GSPC)
PV_GDAXI = T_stat(PNL_GDAXI,VR_GDAXI)
PV_IXIC = T_stat(PNL_IXIC,VR_IXIC)
PV_DJI = T_stat(PNL_DJI,VR_DJI)


T_GSPC = time(GSPC)
T_GSPC <- T_GSPC[-c(1:250)]
T_GDAXI = time(GDAXI)
T_GDAXI <- T_GDAXI[-c(1:250)]
T_IXIC = time(IXIC)
T_IXIC <- T_IXIC[-c(1:250)]
T_DJI = time(DJI)
T_DJI <- T_DJI[-c(1:250)]
# plot for all indexes
# plot for all indexes

data_pv_red_GSPC <- data.frame(T_GSPC = T_GSPC[PV_GSPC >= 0.04], PV = PV_GSPC[PV_GSPC >= 0.04])
data_pv_yellow_GSPC <- data.frame(T_GSPC = T_GSPC[(PV_GSPC < 0.04) & (PV_GSPC >= 0.02)], PV = PV_GSPC[(PV_GSPC < 0.04) & (PV_GSPC >= 0.02)])
data_pv_green_GSPC <- data.frame(T_GSPC = T_GSPC[PV_GSPC < 0.02], PV = PV_GSPC[PV_GSPC < 0.02])

data_pv_red_GDAXI <- data.frame(T_GDAXI = T_GDAXI[PV_GDAXI >= 0.04], PV = PV_GDAXI[PV_GDAXI >= 0.04])
data_pv_yellow_GDAXI <- data.frame(T_GDAXI = T_GDAXI[(PV_GDAXI < 0.04) & (PV_GDAXI >= 0.02)], PV = PV_GDAXI[(PV_GDAXI < 0.04) & (PV_GDAXI >= 0.02)])
data_pv_green_GDAXI <- data.frame(T_GDAXI = T_GDAXI[PV_GDAXI < 0.02], PV = PV_GDAXI[PV_GDAXI < 0.02])

data_pv_red_IXIC <- data.frame(T_IXIC = T_IXIC[PV_IXIC >= 0.04], PV = PV_IXIC[PV_IXIC >= 0.04])
data_pv_yellow_IXIC <- data.frame(T_IXIC = T_IXIC[(PV_IXIC < 0.04) & (PV_IXIC >= 0.02)], PV = PV_IXIC[(PV_IXIC < 0.04) & (PV_IXIC >= 0.02)])
data_pv_green_IXIC <- data.frame(T_IXIC = T_IXIC[PV_IXIC < 0.02], PV = PV_IXIC[PV_IXIC < 0.02])

data_pv_red_DJI <- data.frame(T_DJI = T_DJI[PV_DJI >= 0.04], PV = PV_DJI[PV_DJI >= 0.04])
data_pv_yellow_DJI <- data.frame(T_DJI = T_DJI[(PV_DJI < 0.04) & (PV_DJI >= 0.02)], PV = PV_DJI[(PV_DJI < 0.04) & (PV_DJI >= 0.02)])
data_pv_green_DJI <- data.frame(T_DJI = T_DJI[PV_DJI < 0.02], PV = PV_DJI[PV_DJI < 0.02])


# create plot for GSPC
plot_GSPC <-ggplot() +
  geom_line(data = data.frame(T_GSPC, PV_GSPC), aes(x = T_GSPC, y = PV_GSPC)) +
  geom_rect(data = data_pv_red_GSPC, aes(xmin = T_GSPC, xmax = T_GSPC + 1, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2) +
  geom_rect(data = data_pv_yellow_GSPC, aes(xmin = T_GSPC, xmax = T_GSPC + 1, ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.2) +
  geom_rect(data = data_pv_green_GSPC, aes(xmin = T_GSPC, xmax = T_GSPC + 1, ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.2)


# create plot for GDAXI
plot_GDAXI <- ggplot() +
  geom_line(data = data.frame(T_GDAXI, PV_GDAXI), aes(x = T_GDAXI, y = PV_GDAXI)) +
  geom_rect(data = data_pv_red_GDAXI, aes(xmin = T_GDAXI, xmax = T_GDAXI + 1, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2) +
  geom_rect(data = data_pv_yellow_GDAXI, aes(xmin = T_GDAXI, xmax = T_GDAXI + 1, ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.2) +
  geom_rect(data = data_pv_green_GDAXI, aes(xmin = T_GDAXI, xmax = T_GDAXI + 1, ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.2)


# create plot for IXIC
plot_IXIC <- ggplot() +
  geom_line(data = data.frame(T_IXIC, PV_IXIC), aes(x = T_IXIC, y = PV_IXIC)) +
  geom_rect(data = data_pv_red_IXIC, aes(xmin = T_IXIC, xmax = T_IXIC + 1, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2) +
  geom_rect(data = data_pv_yellow_IXIC, aes(xmin = T_IXIC, xmax = T_IXIC + 1, ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.2) +
  geom_rect(data = data_pv_green_IXIC, aes(xmin = T_IXIC, xmax = T_IXIC + 1, ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.2)


# create plot for DJI
plot_DJI <- ggplot() +
  geom_line(data = data.frame(T_DJI, PV_DJI), aes(x = T_DJI, y = PV_DJI)) +
  geom_rect(data = data_pv_red_DJI, aes(xmin = T_DJI, xmax = T_DJI + 1, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2) +
  geom_rect(data = data_pv_yellow_DJI, aes(xmin = T_DJI, xmax = T_DJI + 1, ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.2) +
  geom_rect(data = data_pv_green_DJI, aes(xmin = T_DJI, xmax = T_DJI + 1, ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.2)


# arrange plots in a grid
ggarrange(plot_GSPC, plot_GDAXI, plot_IXIC, plot_DJI, nrow = 2, ncol = 2)
