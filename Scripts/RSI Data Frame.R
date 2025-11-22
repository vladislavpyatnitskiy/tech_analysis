lapply(c("quantmod", "timeSeries"), require, character.only = T) # lib

RSI.data <- function(x, s=NULL, e=NULL, data=T){
  
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
  
  l <- diff(log(x))[-1,]
  
  df <- NULL
  d <- NULL
  
  for (m in 1:ncol(l)){ y <- l[,m]
    
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
    
    D <- cbind(x, I)
    
    D <- D[apply(D, 1, function(x) all(!is.na(x))),]
    
    colnames(D) <- c("Price", "RSI")
    
    if (is.null(df)){ df <- list(D) } else { df[[m]] <- D } } # Put into list
  
  names(df) <- colnames(p)
  
  df
}
RSI.data("NVDA", s="2025-01-01")
