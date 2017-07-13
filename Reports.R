setwd("/Users/Alex/Programacion/R/PROYECTO")
library(dplyr)
library(highcharter)
source("./scripts/group_sum.R")

log <- read.csv("input/Master.csv")
colnames(log) <- list( "src", "dst", "dcontext", "clid", "channel", "dstchannel", "lastapp", "lastdata", "start", "answer", "end", "duration", "billsec", "disposition", "amaflags", "uniqueid" )
callfailed <- log[log$answer == "" & log$dst != "123" , ]
callsupport <- log[log$dst == "123", ]

data <-  group_sum(callfailed)

hc <- highchart() %>% 
 hc_xAxis(categories = data$fecha) %>% 
 hc_add_series(name = "Busy", data = data$busy) %>% 
 hc_add_series(name = "Congestion", data = data$congestion) %>% 
 hc_add_series(name = "No Answer",data = (data$no_answer)/2)

hc



''' library("highcharter")
library("quantmod")

usdjpy <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)
eurkpw <- getSymbols("EUR/KPW", src = "oanda", auto.assign = FALSE)

hc <- highchart(type = "stock") %>% 
  hc_title(text = "Charting some Symbols") %>% 
  hc_subtitle(text = "Data extracted using quantmod package") %>% 
  hc_add_series(usdjpy, id = "usdjpy") %>% 
  hc_add_series(eurkpw, id = "eurkpw")

hc

hc <- highchart() %>% 
  hc_xAxis(categories = citytemp$month) %>% 
  hc_add_series(name = "Tokyo", data = callfailed$disposition) %>% 
  hc_add_series(name = "London", data = citytemp$london) %>% 
  hc_add_series(name = "Other city",data = (citytemp$tokyo + citytemp$london)/2) '''

