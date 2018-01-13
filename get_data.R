library(digest)
library(httr)
library(lubridate)
library(futile.logger)
library(purrr)
library(tibble)
library(tidyverse)

get_data <- function(date, ticker, interval) {
  
  url <- "https://api.binance.com/api/v1/klines"  
  
  format_data <- function(data) {
    milli_to_dttm <- function(milli) {
      milli %>% 
        substr(1, 10) %>% 
        as.integer() %>%
        as.POSIXct(origin="1970-01-01 00:00:00")
    }
    data <- data %>%
      unlist() %>% 
      matrix(ncol=12, byrow=TRUE) %>% 
      as_tibble() %>%
      select(1:11) %>%
      setNames(c("time_open", "price_open", "price_high", "price_low",
                 "price_close", "volume", "time_close", "asset_volume",
                 "nof_trades", "taker_buy_base", "taker_sell_base")) %>%
      mutate(time_open=milli_to_dttm(time_open),
             time_close=milli_to_dttm(time_close))
    data[map_lgl(data, is.character)] <- data[map_lgl(data, is.character)] %>% map(as.numeric)
    return(data)
  }
  
  date <- date %>% as.character() %>% as.POSIXct()
  
  flog.info("Fetching %s from %s", ticker, as.character(date))
  paste0(url, "?symbol=", ticker,"&interval=", interval, "m",
         "&startTime=", as.integer(date), "000",
         "&endTime=", as.integer(date+days(1)-1), "000") %>%
    GET() %>%
    content() %>%
    format_data()
}


from <- as.Date("2017-11-01")
to <- Sys.Date()

dates <- seq(from, to, by="days")

data <- map_df(dates, get_data, ticker="IOTABTC", interval=15)
save(data, file="IOTABTC.Rdata")

data <- map_df(dates, get_data, ticker="XRPBTC", interval=5)
save(data, file="XRPBTC.Rdata")

data <- map_df(dates, get_data, ticker="ETHBTC", interval=15)
save(data, file="ETHBTC.Rdata")

