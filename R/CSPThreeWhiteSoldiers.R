CSPThreeWhiteSoldiers <- function(TS, strict=TRUE, n=20, minbodysizeMedian=1) {
  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open,High,Low and Close.")
  }

  # 生成三根长白蜡烛的条件
  THREELWCB <- CSPNLongWhiteCandleBodies(TS, N=3, n=n, threshold=minbodysizeMedian)

  # 手动生成滞后序列并明确命名列
  lagged_oc <- do.call(merge, lapply(0:2, function(k) {
    merge(lag(quantmod::Op(TS), -k), lag(quantmod::Cl(TS), -k))
  }))
  colnames(lagged_oc) <- c("Op.L0", "Cl.L0", "Op.L1", "Cl.L1", "Op.L2", "Cl.L2")

  # 逻辑判断（直接使用列名访问）
  result <- reclass(
    THREELWCB[,1] &
      lagged_oc$Op.L0 > lagged_oc$Op.L1 &  # 第三根开盘 > 第二根
      lagged_oc$Op.L1 > lagged_oc$Op.L2 &  # 第二根开盘 > 第一根
      lagged_oc$Cl.L0 > lagged_oc$Cl.L1 &  # 第三根收盘 > 第二根
      lagged_oc$Cl.L1 > lagged_oc$Cl.L2,   # 第二根收盘 > 第一根
    TS
  )

  # 严格模式判断
  if (strict) {
    result <- result &
      lagged_oc$Op.L0 <= lagged_oc$Cl.L1 &  # 第三根开盘在第二根实体范围内
      lagged_oc$Op.L1 <= lagged_oc$Cl.L2    # 第二根开盘在第一根实体范围内
  }

  colnames(result) <- "ThreeWhiteSoldiers"
  xts::xtsAttributes(result) <- list(bars=3)
  return(result)
}

CSPThreeBlackCrows <- function(TS, strict=TRUE, n=20, minbodysizeMedian=1) {
  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open,High,Low and Close.")
  }

  # 生成三根长黑蜡烛的条件
  THREELBCB <- CSPNLongBlackCandleBodies(TS, N=3, n=n, threshold=minbodysizeMedian)

  # 使用相同的滞后序列生成方法
  lagged_oc <- do.call(merge, lapply(0:2, function(k) {
    merge(lag(quantmod::Op(TS), -k), lag(quantmod::Cl(TS), -k))
  }))
  colnames(lagged_oc) <- c("Op.L0", "Cl.L0", "Op.L1", "Cl.L1", "Op.L2", "Cl.L2")

  # 逻辑判断（反转比较方向）
  result <- reclass(
    THREELBCB[,1] &
      lagged_oc$Op.L0 < lagged_oc$Op.L1 &
      lagged_oc$Op.L1 < lagged_oc$Op.L2 &
      lagged_oc$Cl.L0 < lagged_oc$Cl.L1 &
      lagged_oc$Cl.L1 < lagged_oc$Cl.L2,
    TS
  )

  # 严格模式判断
  if (strict) {
    result <- result &
      lagged_oc$Op.L0 >= lagged_oc$Cl.L1 &
      lagged_oc$Op.L1 >= lagged_oc$Cl.L2
  }

  colnames(result) <- "ThreeBlackCrows"
  xts::xtsAttributes(result) <- list(bars=3)
  return(result)
}
