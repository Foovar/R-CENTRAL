setwd("/Users/Alex/Programacion/R/PROYECTO")
library(dplyr)
library(highcharter)
source("./scripts/group_sum.R")
library(zoo)
library(xts)
library(dygraphs)

log <- read.csv("input/Master.csv")
colnames(log) <- list( "src", "dst", "dcontext", "clid", "channel", "dstchannel", "lastapp", "lastdata", "start", "answer", "end", "duration", "billsec", "disposition", "amaflags", "uniqueid" )
callfailed <- log[log$answer == "" & log$dst != "123" , ]
callsupport <- log[log$dst == "123", ]

xx <- hours(callfailed)
tsdata = xts(x = xx[,-1], order.by = strptime(xx[,1], format='%Y-%m-%d %H'))


dygraph(tsdata[,1], main = "All", group = "lung-deaths")
dygraph(tsdata[,2], main = "Male", group = "lung-deaths")
dygraph(tsdata[,3], main = "Female", group = "lung-deaths")


dygraph(tsdata, main = "Reporte. Llamadas sin exito.") %>%
  dyOptions( drawPoints = T, pointSize = 2, colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector(dateWindow = c(as.character( xx$fecha[length(xx$fecha) - 168] ),as.character(tail(xx[,1],1))))

#  dyHighlight(hideOnMouseOut = FALSE, highlightSeriesBackgroundAlpha = 0.2) %>%
ts(1:10, frequency = 4, start = c(1959, 2))
