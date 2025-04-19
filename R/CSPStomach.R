CSPStomach <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  
  LAGTS <- LagOC(TS, k=1)
  
  # Get the column names for Open and Close from the original TS
  open_col <- colnames(Op(TS))
  close_col <- colnames(Cl(TS))
  
  # Construct the lagged column names
  lag_open_col <- paste(open_col, "1", sep=".Lag.")
  lag_close_col <- paste(close_col, "1", sep=".Lag.")
  
  # Above the Stomach pattern:
  # Previous candle is bearish (red)
  # Current candle is bullish (green)
  # Current candle's open is above the midpoint of previous candle
  AboveTheStomach <- reclass(
    LAGTS[,lag_open_col] > LAGTS[,lag_close_col] &  # Previous candle is bearish
    Cl(TS) > Op(TS) &                               # Current candle is bullish
    Op(TS) >= ((LAGTS[,lag_open_col] + LAGTS[,lag_close_col])/2),  # Current open above previous midpoint
    TS
  )
  
  # Below the Stomach pattern:
  # Previous candle is bullish (green)
  # Current candle is bearish (red)
  # Current candle's open is below the midpoint of previous candle
  BelowTheStomach <- reclass(
    LAGTS[,lag_close_col] > LAGTS[,lag_open_col] &  # Previous candle is bullish
    Op(TS) > Cl(TS) &                               # Current candle is bearish
    Op(TS) <= ((LAGTS[,lag_open_col] + LAGTS[,lag_close_col])/2),  # Current open below previous midpoint
    TS
  )
  
  # Combine results and ensure it's an xts object
  result <- as.xts(cbind(AboveTheStomach, BelowTheStomach))
  colnames(result) <- c("AboveTheStomach", "BelowTheStomach")
  attr(result, "bars") <- 2
  return(result)
}
