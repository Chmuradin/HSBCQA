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
tickers <- list(GSPC,GDAXI,IXIC,DJI)
#par(mfrow=c(2,2))

#chart_Series(GSPC)
#chart_Series(GDAXI)
#chart_Series(IXIC)
#chart_Series(DJI)

GSPC %<>% as.data.frame() 
GDAXI %<>% as.data.frame()
IXIC %<>% as.data.frame()
DJI %<>% as.data.frame()

GSPC %<>% select(4) %>% transmute(pnl=diff(c(0,GSPC.Close))) %>% tail(-1)
GDAXI %<>% select(4) %>% transmute(pnl=diff(c(0,GDAXI.Close))) %>% tail(-1)
IXIC%<>% select(4) %>% transmute(pnl=diff(c(0,IXIC.Close))) %>% tail(-1)
DJI %<>% select(4) %>% transmute(pnl=diff(c(0,DJI.Close))) %>% tail(-1)
tickers <- list(GSPC,GDAXI,IXIC,DJI)
ticker_names <- c("GSPC","GDAXI","IXIC","DJI")
PNL <- GSPC
for(i in 2:4){
  PNL %<>% merge(tickers[[i]],by='row.names',all=TRUE) 
  row.names(PNL) <-( PNL[,1])
  PNL %<>% select(-1) 
  colnames(PNL) <- ticker_names[1:i]
}
rm(DJI,GDAXI,GSPC,IXIC,tickers,ticker_names,i)
