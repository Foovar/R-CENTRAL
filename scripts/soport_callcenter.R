library(plotly)
library(dplyr)
library(dygraphs)
library(zoo)
library(xts)
log <- read.csv("input/Master.csv")
colnames(log) <- list( "src", "dst", "dcontext", "clid", "channel", "dstchannel", "lastapp", "lastdata", "start", "answer", "end", "duration", "billsec", "disposition", "amaflags", "uniqueid" )
callsupport <- log[log$dst == "123", ]
callsupport$start <- gsub("([0-9]+:[0-9]+:[0-9]+)","", callsupport$start)
callsupport_repot <- data.frame(callsupport$start, c(0))
colnames(callsupport_repot)<-list("date", "value")
callsupport_repot <-callsupport_repot %>%
  group_by(date) %>%
  summarise(n())
colnames(callsupport_repot)<-list("date", "value")
##convertir a tipo date
#callsupport_repot$date <-strptime(as.character(callsupport_repot$date), "%Y-%m-%d")
callsupport_repot$date <- as.Date(callsupport_repot$date)
soporteTS<- xts(x=callsupport_repot$value,order.by = callsupport_repot$date, format='%Y-%M-%d')

dygraph(soporteTS, main = "N° de llamadas a atencion cliente por dia") %>%
  dyOptions(drawPoints = T, pointSize = 2,fillGraph = TRUE, fillAlpha = 0.4) %>%
  dyRangeSelector(height = 40)


