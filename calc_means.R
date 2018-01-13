library(tidyverse)
library(ggplot2)
library(TTR)
library(scales)

options(scipen=999)


load("IOTABTC.Rdata")


data$price_close_sma5 <- SMA(data$price_close, n=5)
data$price_close_sma10 <- SMA(data$price_close, n=10)
data$price_close_sma20 <- SMA(data$price_close, n=20)
data$price_close_sma50 <- SMA(data$price_close, n=50)

data$price_close_ema12 <- EMA(data$price_close, n=12)
data$price_close_ema26 <- EMA(data$price_close, n=26)
data$price_close_macd <- data$price_close_ema12 - data$price_close_ema26
data$price_close_sig <- EMA(data$price_close_macd, n=9)



data %>%
  dplyr::filter(time_open >= '2018-01-09 07:00:00') %>%
  #select(time_open, price_close, starts_with("price_close_")) %>%
  select(time_open, price_close) %>%
  gather(type, value, 2) %>%
  ggplot(aes(x=time_open, y=value, group=type, color=type)) +
    geom_line() +
    scale_x_datetime(breaks = date_breaks("hour"), labels=date_format("%H:%M", tz="Europe/Oslo")) +
    theme(axis.text.x=element_text(angle=90, hjust=1))



data %>%
  dplyr::filter(time_open >= '2018-01-08 00:00:00', time_open <= '2018-01-10 00:00:00') %>%
  select(time_open, price_close, price_close_macd, price_close_sig) %>%
  gather(type, value, 3:4) %>%
  mutate(value=value) %>%
  ggplot(aes(x=time_open, y=value, group=type, color=type)) +
    geom_line() +
    geom_hline(yintercept=0) +
    scale_x_datetime(breaks = date_breaks("hour"), labels=date_format("%H:%M", tz="Europe/Oslo")) +
    theme(axis.text.x=element_text(angle=90, hjust=1))
  

a <- data %>%
  dplyr::filter(time_open >= '2018-01-01 00:00:00') %>%
  select(time_open, price_close, price_close_macd, price_close_sig) %>%
  mutate(diff=price_close_macd-price_close_sig,
         action="NO",
         IOTA=100,
         BTC=0,
         fee=0)

buy <- function(btc, price, fee=0.001) {
  c((btc-(btc*fee))/price, btc*fee/price)
}

sell <- function(iota, price, fee=0.001) {
  c((iota-(iota*fee))*price, iota*fee)
}

for(i in 2:nrow(a)) {
  if(a$diff[i-1]<0 & a$diff[i]>0 & abs(a$diff[i]>0.00)) a$action[i]="BUY"
  if(a$diff[i-1]>0 & a$diff[i]<0) a$action[i]="SELL"
  
  if(a$action[i]=="SELL" & a$IOTA[i-1] !=0) {
    res <- sell(a$IOTA[i], a$price_close[i], fee=0.0005)
    a$BTC[i:nrow(a)] <- res[1]
    a$fee[i:nrow(a)] <- a$fee[i-1] + res[2]
  } else if(a$action[i]=="BUY" & a$BTC[i-1] !=0) {
    res <- buy(a$BTC[i], a$price_close[i], fee=0.0005)
    a$IOTA[i:nrow(a)] <- res[1]
    a$fee[i:nrow(a)] <- a$fee[i-1] + res[2]
  }
}

