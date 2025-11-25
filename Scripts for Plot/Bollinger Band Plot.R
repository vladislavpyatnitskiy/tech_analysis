library("timeSeries")

bollinger.band <- function(x, ts=20){
  
  ts_n <- ts - 1
  
  l <- NULL
  
  for (n in 1:(nrow(x) - ts_n)){ y <- x
    
    MEAN = mean(y[n:(n + ts_n),])
    SD = sd(y[n:(n + ts_n),])
    
    l = rbind.data.frame(l, cbind(MEAN, MEAN - SD, MEAN + SD)) }
  
  rownames(l) <- rownames(x)[ts:nrow(x)]
  
  p <- cbind(x, l)
  
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
  
  for (m in 2:(ncol(p))){ lines(p[,m], col = m, lwd = 2) } # Plot indices
  
  par(mar = c(8, rep(4, 3))) # Define borders of the plot
  
  legend(
    x = "bottom",
    inset = c(0, -0.2),
    legend = colnames(p),
    col = seq(ncol(p)),
    lwd = 2,
    cex = .85,
    bty = "n",
    xpd = T,
    horiz = T
  )
  
  on.exit(par(par(no.readonly = T))) # Show legend with names
}
bollinger.band(stock_data[,1])
