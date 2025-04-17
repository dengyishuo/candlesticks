CSPNBlended <- function(TS, N) {

  if (!quantmod::is.OHLC(TS))
    stop("Price series must contain Open, High, Low and Close.")
  if (N < 1 || N != as.integer(N))
    stop("N must be an integer >=1")

  OpLagN <- quantmod::Lag(quantmod::Op(TS), k = N)

  HighRoll <- zoo::rollapply(quantmod::Hi(TS), width=N,
                             FUN=max, align="right", fill=NA)
  LowRoll <- zoo::rollapply(quantmod::Lo(TS), width=N,
                            FUN=min, align="right", fill=NA)

  result <- merge(
    Open  = OpLagN,
    High  = HighRoll,
    Low   = LowRoll,
    Close = quantmod::Cl(TS)
  )

  colnames(result) <- paste(N,
                            c("Blended.Open", "Blended.High", "Blended.Low", "Blended.Close"),
                            sep=".")

  return(na.omit(result))
}
