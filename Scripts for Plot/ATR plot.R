lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

ATR <- function(x, s=NULL, e=NULL, ts = 14, data=T){
  
  L <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  if (data){ for (A in x){ p <- getData(A, s, e)
  
      p$`High-Low` <- p[,2] - p[,3]
      
      p$`High-Close` <- abs(p[,2] - p[,4])
      
      p$`Low-Close` <- abs(p[,3] - p[,4])
      
      g <- NULL
      
      for (n in 1:nrow(p)){ g <- rbind(g, max(p[n, 7:9])) }
      
      p <- as.timeSeries(p)
      g <- as.timeSeries(g)
      
      rownames(g) <- rownames(p) 
      
      L <- cbind(L, g) } # Join data
    
    L <- L[apply(L, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(L) <- x # Put the tickers in column names
    
    x <- as.timeSeries(L) } # Make it time series and display
  
  ts_n <- ts - 1
  
  df <- NULL
  
  for (i in 1:ncol(x)){ l <- NULL
    
    for (n in 1:(nrow(x) - ts_n)){ y <- x[,i]
      
      l = rbind.data.frame(l, MEAN = mean(y[n:(n + ts_n),])) }
      
    rownames(l) <- rownames(y)[ts:nrow(y)]
    
    p <- cbind(y, l)
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p)[2] <- c("MA14") 
    
    p <- as.timeSeries(p)
    
    if (is.null(df)){ df <- list(p) } else { df[[i]] <- p } }
    
  names(df) <- colnames(L)
  
  for (n in 1:length(df)){
  
    plot(
      df[[n]][,1],
      xlab = "Trading Days",
      ylab = "ATR",
      las = 1,
      main = sprintf("Average True Range for %s", colnames(L)[n])
      )
    
    axis(side = 4, las = 2) # Right Y-Axis Values
    
    grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
    
    lines(df[[n]][,2], col = "red", lwd = 3)
    
    par(mar = c(8, rep(4, 3))) # Define borders of the plot
    
    legend(
      x = "bottom",
      inset = c(0, -0.2),
      legend = c("True Range", "Average True Range"),
      col = c("black", "red"),
      lwd = 2,
      cex = .85,
      bty = "n",
      xpd = T,
      horiz = T
    )
    
    on.exit(par(par(no.readonly = T))) # Show legend with names
  }
}
ATR(c("GOOGL", "AAPL"), s="2022-01-01")
