rm(list = ls())
gc()
#wczytanie bibliotek
library(quantmod)
library(dplyr)
library(magrittr)
library(nortest)
library(car)
library(tseries)
library(ggh4x)
library(fitdistrplus)
library(cowplot)
#wczytanie snp
end_date=as.Date("2022-12-31")
getSymbols("^GSPC",
           src = "yahoo",
           from = end_date - 5 * 365,
           to = end_date)
#wczytanie dax
getSymbols("^GDAXI",
           src = "yahoo",
           from = end_date - 5 * 365,
           to = end_date)
#NASDAQ
getSymbols("^IXIC",
           src = "yahoo",
           from = end_date - 5 * 365,
           to = end_date)
#dow jones industrial avrage
getSymbols("^DJI",
           src = "yahoo",
           from = end_date - 5 * 365,
           to = end_date)
tickers <- list(GSPC,GDAXI,IXIC,DJI)
par(mfrow=c(2,2))

chart_Series(GSPC)
chart_Series(GDAXI)
chart_Series(IXIC)
chart_Series(DJI)

GSPC %<>% as.data.frame() 
GDAXI %<>% as.data.frame()
IXIC %<>% as.data.frame()
DJI %<>% as.data.frame()

GSPC %<>% dplyr::select(4) %>% transmute(pnl=diff(c(0,GSPC.Close))) %>% tail(-1)
GDAXI %<>% dplyr::select(4) %>% transmute(pnl=diff(c(0,GDAXI.Close))) %>% tail(-1)
IXIC%<>% dplyr::select(4) %>% transmute(pnl=diff(c(0,IXIC.Close))) %>% tail(-1)
DJI %<>% dplyr::select(4) %>% transmute(pnl=diff(c(0,DJI.Close))) %>% tail(-1)
tickers <- list(GSPC,GDAXI,IXIC,DJI)
ticker_names <- c("GSPC","GDAXI","IXIC","DJI")
PNL <- GSPC
for(i in 2:4){
  PNL %<>% merge(tickers[[i]],by='row.names',all=TRUE) 
  row.names(PNL) <-( PNL[,1])
  PNL %<>% dplyr::select(-1) 
  colnames(PNL) <- ticker_names[1:i]
}
rm(DJI,GDAXI,GSPC,IXIC,tickers,ticker_names,i)
###### czesc 1
#data frame do ggplota
PNL2 <- PNL %>% mutate(date=row.names(PNL))
plot_data <- NULL
for( i in 1:(dim(PNL2)[2]-1)){
  if (i==1){
    plot_data <- PNL2 %>% dplyr::select(i,date) %>% cbind(colnames(PNL2)[i]) %>% as.matrix()
  }
  if(i!=1){
plot_data <- PNL2 %>% dplyr::select(i,date) %>% cbind(colnames(PNL2)[i]) %>% as.matrix() %>% rbind(plot_data)
  }
}
colnames(plot_data) <- c("PnL","date","index")
plot_data %<>% as.data.frame() 
plot_data$PnL %<>% as.numeric() 
plot_data$date %<>% as.Date() 
plot_data$index %<>% as.factor() 
plot_data %<>% mutate(after_covid=date>as.Date("2020-03-01"))
## plot pnlow
plot_data %>% ggplot(aes(x=date,y=PnL)) +
  geom_bar(stat="identity")+ggh4x::facet_grid2(cols=vars(index),independent = T,scales="free")
## porownanie gestosci
plot_data %>% ggplot(aes(x=PnL))+geom_density(aes(color='density estimator'))+ggh4x::facet_grid2(cols=vars(index),scales="free",independent = T)+
  stat_theodensity(aes(color='fitted normal density'))+
  scale_color_manual(name='',
                     breaks=c('density estimator','fitted normal density'),
                     values=c('density estimator'='black','fitted normal density'='red'))+
   theme(legend.position="bottom")
#
plot_data %>% group_by(index,after_covid) %>% summarise(sd = sd(PnL, na.rm=TRUE)) %>% 
  ggplot(aes(x=after_covid,y=sd)) +
  geom_bar(stat="identity")+facet_grid(cols=vars(index))+
  labs(x="after covid",y="standard deviation")
### testy normalnosci dla miesiecy
indeksy=levels(plot_data$index)
pvdf <- data.frame()
for(i in 1:(length(indeksy))){
splited <- plot_data %>% filter(index==indeksy[i])
splited %<>%   split(substr(as.character(splited$date),1,7))
df <- lapply(splited,function(x){(x %>% dplyr::select(PnL) %>% as.vector() %>% extract2(1) %>% 
                              na.omit() %>% shapiro.test())$p.value}) %>% unlist() %>% as.data.frame()
df <- cbind(df,row.names(df))
df %<>% cbind(indeksy[i]) 
colnames(df) <- c("pvalue","month","index")
df %<>% mutate("kolor"=(pvalue<0.05))
pvdf %<>% rbind(df) 
}

pvdf %>% ggplot(aes(x=month,y=pvalue,colour=kolor))+geom_bar(stat="identity",show.legend = FALSE)+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red")+theme(axis.text.x = element_text(size=3,angle = 90, vjust = 0.5, hjust=1))+facet_grid(cols=vars(index))
rm(df,pvdf,i,splited)
## acf
acfdf <- data.frame()
for (i in 1:length(indeksy)){
a <- PNL %>% dplyr::select(i) %>% na.omit() %>% acf(lag=30,plot=F) %>% extract2(1)
a %<>%  cbind(0:30)
a %<>% cbind(indeksy[i]) 
colnames(a) <- c("corelation","lag","index")
acfdf %<>% rbind(a)
rm(a)
}
acfdf$corelation %<>% as.numeric() 
acfdf$lag %<>% as.numeric() 
acfdf %>% ggplot(aes(x=lag,y=corelation))+ geom_bar(stat="identity") +
  facet_grid(cols=vars(index))
rm(acfdf,i)
## abs value
acfdf <- data.frame()
for (i in 1:length(indeksy)){
  a <- PNL %>% dplyr::select(i) %>% na.omit() %>% abs() %>% acf(lag=30,plot=F) %>% extract2(1)
  a %<>%  cbind(0:30)
  a %<>% cbind(indeksy[i]) 
  colnames(a) <- c("corelation","lag","index")
  acfdf %<>% rbind(a)
  rm(a)
}
acfdf$corelation %<>% as.numeric() 
acfdf$lag %<>% as.numeric() 
acfdf %>% ggplot(aes(x=lag,y=corelation))+ geom_bar(stat="identity") +
  facet_grid(cols=vars(index))
rm(acfdf,i)
##### homogenity test
for (i in 1:length(indeksy)){
homotest <- plot_data %>% filter(index==indeksy[1]) %>% 
  mutate(month=substr(date,1,7)) 
stats::fligner.test(PnL~month,homotest) %>% print()
rm(homotest)
}
