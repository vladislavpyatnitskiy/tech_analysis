lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

bollinger.band.data <- function(x, s=NULL, e=NULL, ts=20, data=T){
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  if (data){ for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in column names
    
    x <- as.timeSeries(p) } # Make it time series and display
  
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
bollinger.band.data(c("GOOGL", "NVDA"), s="2022-01-01")
