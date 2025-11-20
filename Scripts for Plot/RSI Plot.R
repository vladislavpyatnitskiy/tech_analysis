lapply(c("quantmod", "timeSeries"), require, character.only = T) # lib

RSI <- function(x, s=NULL, e=NULL, data=T){
  
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
  
  x <- diff(log(x))[-1,]
  
  df <- NULL
  d <- NULL
  
  for (m in 1:ncol(x)){ y <- x[,m]
    
    gains <- ifelse(y > 0, y, 0)
    losses <- ifelse(y < 0, -y, 0)
    
    G <- NULL
    L <- NULL
    
    for (n in 1:(nrow(y)-14)){
      
      G <- rbind.data.frame(G, mean(gains[n:(n+13),]))
      L <- rbind.data.frame(L, mean(losses[n:(n+13),]))
    }
    
    I <- 100 - (100 / (1 + G/L))
    
    dates <- rownames(y)[15:nrow(y)]
    
    rownames(I) <- dates
    colnames(I) <- colnames(y)
    
    I <- as.timeSeries(I)
    
    plot(
      I,
      ylab = "RSI",
      xlab = "Trading Days",
      main = sprintf("Relative Strength Index of %s", colnames(y)),
      las = 1
      ) 
    
    axis(side = 4, las = 2) # Right Y-Axis Values
    
    grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
    
    A <- I[nrow(I),]
    
    d <- c(d, if (A < 30) "Buy" else if (A > 70) "Sell" else "Hold")
    
    df <- c(df, A)
  }
  
  D <- cbind.data.frame(colnames(x), df, d)
  
  colnames(D) <- c("Ticker", "RSI", "Signal")
  
  D
}
RSI(c("AAPL", "NVDA"), s="2025-01-01")
