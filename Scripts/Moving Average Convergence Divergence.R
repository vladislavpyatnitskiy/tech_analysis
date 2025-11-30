lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

MACD <- function(x, s=NULL, e=NULL, ts = 14, data=T){
 
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
  
  DF <- NULL # Where to contain data frames
  
    for (i in 1:ncol(x)){ y <- x[,i]
    
    MA <- NULL # Where to put all moving averages
    
    for (m in 1:length(ts)){ l <- NULL # Get averages for all day periods
    
      for (n in 1:(nrow(y) - ts[m] + 1)){
        
        l = rbind.data.frame(l, mean(y[n:(n + ts[m] - 1),])) }
      
      l <- cbind.data.frame(rownames(y)[ts[m]:nrow(y)], l)
      
      colnames(l) <- c("Date", sprintf("MA%s", ts[m]))
      
      if (is.null(MA)){ MA <- l } else { MA <- merge(MA, l, by="Date") } }
      
      df <- data.frame(rownames(y), y)
      
      colnames(df) <- c("Date", colnames(y))
      
      MA <- merge(df, MA, by="Date")
      
      dates <- MA[,1] # Assign variable for dates
      
      MA <- MA[,-1] # Reduce column with dates
      
      rownames(MA) <- dates # set up dates as row names
      
      MA$MACD <- MA[,2] - MA[,3]
      
      f <- NULL
  
      for (n in 1:(nrow(MA)-8)){ f = rbind.data.frame(f, mean(MA[n:(n + 8),4])) }
       
      f <- data.frame(rownames(MA)[9:nrow(MA)], f)
      
      colnames(f) <- c("Date", "Signal")
      
      MA <- data.frame(rownames(MA), MA)
      
      colnames(MA)[1] <- "Date"
      
      MA <- merge(MA, f, by="Date")
      
      dates <- MA[,1] # Assign variable for dates
      
      MA <- MA[,-1] # Reduce column with dates
      
      rownames(MA) <- dates # set up dates as row names
      
      MA$Histogram <- MA[,4] - MA[,5]
      
      MA <- as.timeSeries(MA)
      
      if (is.null(DF)){ DF <- list(MA) } else { DF[[i]] <- MA } } # Put into list
    
  names(DF) <- colnames(x) 
  
  DF
}
MACD("AAPL", s="2025-01-01", ts = c(12,26))
