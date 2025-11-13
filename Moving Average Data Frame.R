library("timeSeries)

moving_average_df <- function(x, ts=50){ # Moving Average Data frame
  
  if (all(nrow(x) < ts)){ # Check whether it has sufficient number of rows
    
    return(message(
      "Choose another time interval for data frame or time series")) }
  
  DF <- NULL # Where to contain data frames
  
  for (i in 1:ncol(x)){ y <- x[,i]
  
    MA <- NULL # Where to put all moving averages
    
    for (m in 1:length(ts)){ l <- NULL # Get averages for all day periods
      
      for (n in 1:(nrow(y) - ts[m] + 1)){
        
        l = rbind.data.frame(l, mean(y[n:(n + ts[m] - 1),])) }
      
      l <- cbind.data.frame(rownames(y)[ts[m]:nrow(y)], l)
      
      colnames(l) <- c("Date", sprintf("%s MA%s", colnames(y)[1], ts[m]))
      
      if (is.null(MA)){ MA <- l } else { MA <- merge(MA, l, by="Date") } }
    
    dates <- MA[,1] # Assign variable for dates
    
    MA <- MA[,-1] # Reduce column with dates
    
    rownames(MA) <- dates # set up dates as row names
    
    MA <- as.timeSeries(MA) # Make it time series
    
    if (is.null(DF)){ DF <- list(MA) } else { DF[[i]] <- MA } # Put into list
  }
  
  DF # Display
}
moving_average_df(stock_data, ts=c(50, 200)) # Test
