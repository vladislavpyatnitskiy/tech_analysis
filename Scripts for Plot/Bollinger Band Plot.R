lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

bollinger.band <- function(x, s=NULL, e=NULL, ts=20, data=T){
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  if (data){ for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) 
  
      message(
        sprintf(
          "%s is downloaded (%s / %s)", 
          A, which(x == A), length(x)
        )
      ) # Download message
    } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in column names
    
    x <- as.timeSeries(p) } # Make it time series and display
  
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
bollinger.band(c("GOOGL", "ZIM"), s="2022-01-01")
