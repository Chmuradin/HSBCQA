rm(list = ls())
dev.off()
gc()
library(quantmod)
library(dplyr)
library(magrittr)
library(nortest)
library(car)
#CZESC 1 POBRANIE I WIZUALIZACJA DANYCH
{
  #wczytanie bibliotek

  #pobranie danych z ostatnich 5 lat dla snp500
  getSymbols("^GSPC",
             src = "yahoo",
             from = Sys.Date() - 5 * 365,
             to = Sys.Date())
  #wykres swiecowy
  chartSeries(GSPC)
  #wykres logarytmicznych stop zwrotu
  plot(periodReturn(GSPC, period = "daily", type = "log"))
  # wyciagniecie cen zamkniecia i zrobienie PnLa
  PnL <-
    GSPC %>% as.data.frame() %>% select(GSPC.Close) %>% as.vector() %>% extract2(1)
  names(PnL) <- rownames(as.data.frame(GSPC))
  PnL %<>% diff()
  #tu można jakieś ploty porobic jeszcze
}
# CZESC 2 - TESTY DLA ROZKLADU PNL
##2.1 normalnosc
PnL %>% density() %>% plot()
m <- mean(PnL)
s=sd(PnL)
#dodanie dopasowanego rozkladu normalnego
curve(dnorm(x,m,s),add=TRUE,col="red")
# qqplot
qqPlot(PnL)
#tu dopisac testy normalnosci i powiedziec ze nie dziala

##2.2 testy autokorelacji
acf(PnL)
#tu sie wywraca
acf(abs(PnL))
acf(PnL^2)
#mozna dopisac test statystyczny (chyba durbin watson)
##2.3 testy narownosc wariancji w podokresach
#wariancja w poszczegolnych latach
splited_by_year <- split(PnL,format((as.Date(names(PnL))),"%Y"))
lapply(splited_by_year, sd) %>% unlist() #to trzeba splotowac ladnie 
#tutaj znalezc test na rownosc wariancji w n grupach i powiedziec ze pvalue jest male
rm(splited_by_year)
# rownosc wariancji na poniedzialkach i nieponiedzialkach
splited_by_monday <- split(PnL,weekdays((as.Date(names(PnL))))=="poniedziałek")
lapply(splited_by_monday, sd) %>% unlist()
#test rownosci wariancji
var.test(splited_by_monday[[1]],splited_by_monday[[2]])#pv=0.03
#mozna dodac test na rownosc rozkladow

