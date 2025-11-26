library("timeSeries")

bollinger.band.data <- function(x, ts=20){
  
  ts_n <- ts - 1
  
  df <- NULL
  
  for (i in 1:ncol(x)){ l <- NULL
  
    for (n in 1:(nrow(x) - ts_n)){ y <- x[,i]
    
      MEAN = mean(y[n:(n + ts_n),])
      SD = sd(y[n:(n + ts_n),])
      
      l = rbind.data.frame(l, cbind(MEAN, MEAN - SD, MEAN + SD)) }
      
    rownames(l) <- rownames(y)[ts:nrow(y)]
      
    p <- cbind(y, l)
      
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
      
    colnames(p)[2:4] <- c("MA20", "-SD", "+SD") 
      
    if (is.null(df)){ df <- list(p) } else { df[[i]] <- p } }
    
  names(df) <- colnames(x)
  
  df
}
bollinger.band.data(stock_data)
