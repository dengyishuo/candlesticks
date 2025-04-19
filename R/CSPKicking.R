CSPKicking <- function(TS, ignoreShadows=TRUE, n=20, threshold=1, ATRFactor=1, maxshadowCL=.1) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  TSGAP <- CSPGap(TS, ignoreShadows=ignoreShadows)

  if (ignoreShadows == FALSE) {
    MB <- CSPMarubozu(TS, n=n, ATRFactor=ATRFactor,
                      maxuppershadowCL=maxshadowCL,
                      maxlowershadowCL=maxshadowCL)
    # 修正列索引（原代码错误地使用了第4列）
    WMB1 <- Lag(MB[, 1], k=1)  # 白色Marubozu
    BMB1 <- Lag(MB[, 2], k=1)  # 黑色Marubozu（原错误索引4改为2）

    BULLK <- reclass(
      TSGAP[,1] &  # Gap Up
        BMB1 & MB[,1],  # 前黑Marubozu，后白Marubozu
      TS)

    BEARK <- reclass(
      TSGAP[,2] &  # Gap Down
        WMB1 & MB[,2],  # 前白Marubozu，后黑Marubozu
      TS)

  } else if (ignoreShadows == TRUE) {
    LCB <- CSPLongCandleBody(TS, n=n, threshold=threshold)
    LWCB1 <- Lag(LCB[,1], k=1)  # 长白实体
    LBCB1 <- Lag(LCB[,2], k=1)  # 长黑实体

    BULLK <- reclass(
      TSGAP[,1] &  # Gap Up
        LBCB1 & LCB[,1],  # 前长黑实体，后长白实体
      TS)

    BEARK <- reclass(
      TSGAP[,2] &  # Gap Down
        LWCB1 & LCB[,2],  # 前长白实体，后长黑实体
      TS)
  } else {
    stop("ignoreShadows must be either TRUE or FALSE")
  }

  result <- cbind(BULLK, BEARK)
  colnames(result) <- c("Bull.Kicking", "Bear.Kicking")
  xtsAttributes(result) <- list(bars=2)
  return(result)
}
