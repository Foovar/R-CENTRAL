group_sum <- function(x) {
  x$start <- as.Date(x$start)
  order(x$start)
  
  group <- x %>% group_by(start) %>% summarise(n())
  ceros <- rep.int(0, length(group$start))
  j <- 1
  fecha <- c(x$start[j])
  busy <- ceros
  congestion <- ceros
  no_answer <- ceros
  
  for(i in 1:length(x$start)){
    if(fecha[j] != x$start[i]){
      fecha[j + 1] = x$start[i]
      j = j + 1
    }
    
    if( x$disposition[i] == "BUSY"){
      busy[j] = busy[j] +1
    }else if(x$disposition[i] == "CONGESTION"){
      congestion[j] = congestion[j] +1
    }else{
      no_answer[j] = no_answer[j] +1
    }
  }
  
  return(data.frame(fecha, busy, congestion, no_answer))
}

hours <- function(x) {
  x$start <- gsub("([0-9\\-]+) ([0-9]+):([0-9]+):([0-9]+)", "\\1 \\2:00:00", x$start)
  group <- x %>% group_by(start) %>% summarise(n())
  ceros <- rep.int(0, length(group$start))
  j <- 1
  fecha <- c(x$start[j])
  busy <- ceros
  congestion <- ceros
  no_answer <- ceros
  
  for(i in 1:length(x$start)){
    if(fecha[j] != x$start[i]){
      fecha[j + 1] = x$start[i]
      j = j + 1
    }
    
    if( x$disposition[i] == "BUSY"){
      busy[j] = busy[j] +1
    }else if(x$disposition[i] == "CONGESTION"){
      congestion[j] = congestion[j] +1
    }else{
      no_answer[j] = no_answer[j] +1
    }
  }
  
  return(data.frame(fecha, busy, congestion, no_answer))
}

