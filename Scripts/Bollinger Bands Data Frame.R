library("timeSerie")

bollinger.band.data <- function(x, ts=20){
  
  ts_n <- ts - 1
  
  l <- NULL
  
  for (n in 1:(nrow(x) - ts_n)){ y <- x
  
    MEAN = mean(y[n:(n + ts_n),])
    SD = sd(y[n:(n + ts_n),])
    
    l = rbind.data.frame(l, cbind(MEAN, MEAN - SD, MEAN + SD)) }
    
  rownames(l) <- rownames(x)[ts:nrow(x)]
  
  p <- cbind(x, l)
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p)[2:4] <- c(sprintf("MA%s", ts), "-SD", "+SD") 
  
  p
}
bollinger.band.data(stock_data[,1])
