lapply(c("timeSeries", "xts", "moexer"), require, character.only = T) # lib

bollinger.band.rus <- function(x, s=NULL, e=NULL, ts=20, data=T){
  
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
  
  for (i in 1:ncol(x)){ l <- NULL
  
    for (n in 1:(nrow(x) - ts_n)){ y <- x[,i]
    
      MEAN = mean(y[n:(n + ts_n),])
      SD = sd(y[n:(n + ts_n),])
      
      l = rbind.data.frame(l, cbind(MEAN, MEAN - SD, MEAN + SD)) }
      
    rownames(l) <- rownames(y)[ts:nrow(y)]
    
    p <- cbind(y, l)
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p)[2:4] <- c("MA20", "-SD", "+SD") 
    
    plot(
      p[,1],
      ylim = c(min(p), max(p)),
      lty = 1,
      type = "l",
      lwd = 2,
      las = 1,
      xlab = "Trading Days",
      ylab = NULL,
      main = sprintf(
        "%s Stock Price with Moving Averages and Bollinger Bands",
        colnames(p)[1])
    )
    
    axis(side = 4, las = 2) # Right Y-Axis Values
    
    grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
    
    abline(h = 0) # Add black horizontal line at break even point
    
    col = c(1, 4, 3, 2)
    
    for (m in 2:(ncol(p))){ lines(p[,m], col = col[m], lwd = 2) } # other lines
    
    par(mar = c(8, rep(4, 3))) # Define borders of the plot
    
    legend(
      x = "bottom",
      inset = c(0, -0.2),
      legend = colnames(p),
      col = col,
      lwd = 2,
      cex = .85,
      bty = "n",
      xpd = T,
      horiz = T
    )
    
    on.exit(par(par(no.readonly = T))) # Show legend with names
    }
}
bollinger.band.rus(c("PHOR", "AKRN", "TTLK"), s="2022-01-01")
