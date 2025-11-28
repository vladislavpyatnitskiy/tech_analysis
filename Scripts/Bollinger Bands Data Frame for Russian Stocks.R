lapply(c("timeSeries", "xts", "moexer"), require, character.only = T) # lib

bollinger.band.data.rus <- function(x, s=NULL, e=NULL, ts=20, data=T){
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  getData <- function(A, s, e) { 
    if (is.null(s) && is.null(e))
      return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
    if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
    if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
    return(get_candles(A, from = s, till = e, interval = 'daily')) 
  }
  for (A in x){ D <- as.data.frame(getData(A, s, e)[,c(3,8)])
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    D <- xts(D[,1], order.by = as.Date(D[,2])) # Move dates to row names
    
    colnames(D) <- A # Put the tickers in data set
    
    D <- as.timeSeries(D) # Make it time series
    
    if (A == "BELU" & isTRUE(split) &
        (isTRUE(s < "2024-08-15" | e < "2024-08-15") |
         isTRUE(is.null(s) | is.null(e)))){
      
      f <- which(rownames(D) == "2024-08-15")
      
      D[c(1:f),] <- D[c(1:f),] / 8 } # Adjustments for Novabev stock
    
    p <- cbind(p, D) } # Merge
    
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  x <- as.timeSeries(p)
  
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
bollinger.band.data.rus(c("OZON"), s="2022-01-01")
