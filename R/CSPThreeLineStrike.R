CSPThreeLineStrike <- function(TS, n=25, minbodysizeMedian=0.5) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }

  # 手动生成滞后序列并明确命名列名
  lagged_oc <- do.call(merge, lapply(0:3, function(k) {
    merge(lag(quantmod::Op(TS), -k), lag(quantmod::Cl(TS), -k))
  }))
  colnames(lagged_oc) <- c("Op.L0", "Cl.L0", "Op.L1", "Cl.L1",
                           "Op.L2", "Cl.L2", "Op.L3", "Cl.L3")

  # 提取需要的列（使用列名代替Cl()/Op()自动匹配）
  Cl_LAGTS <- lagged_oc[, c("Cl.L0", "Cl.L1", "Cl.L2", "Cl.L3")]
  Op_LAGTS <- lagged_oc[, c("Op.L0", "Op.L1", "Op.L2", "Op.L3")]

  # 获取三连阳/阴信号（假设这些函数返回单列结果）
  THREELWCB <- CSPNLongWhiteCandleBodies(TS, N=3, n=n, threshold=minbodysizeMedian)
  LAGTHREELWCB <- stats::lag(THREELWCB, 1)  # 信号滞后1期
  THREELBCB <- CSPNLongBlackCandleBodies(TS, N=3, n=n, threshold=minbodysizeMedian)
  LAGTHREELBCB <- stats::lag(THREELBCB, 1)

  # 牛市三线打击形态判断（使用明确的列名引用）
  BullTLS <- reclass(
    LAGTHREELWCB[,1] &                          # 前三根为长白蜡烛（滞后1期）
      Cl_LAGTS$Cl.L2 > Cl_LAGTS$Cl.L3 &            # 第2根收盘 > 第1根收盘
      Cl_LAGTS$Cl.L1 > Cl_LAGTS$Cl.L2 &            # 第3根收盘 > 第2根收盘
      Op_LAGTS$Op.L0 >= Cl_LAGTS$Cl.L1 &           # 第4根开盘 >= 第3根收盘
      Cl_LAGTS$Cl.L0 <= Op_LAGTS$Op.L3,            # 第4根收盘 <= 第1根开盘
    TS
  )

  # 熊市三线打击形态判断
  BearTLS <- reclass(
    LAGTHREELBCB[,1] &                          # 前三根为长黑蜡烛（滞后1期）
      Cl_LAGTS$Cl.L2 < Cl_LAGTS$Cl.L3 &            # 第2根收盘 < 第1根收盘
      Cl_LAGTS$Cl.L1 < Cl_LAGTS$Cl.L2 &            # 第3根收盘 < 第2根收盘
      Op_LAGTS$Op.L0 <= Cl_LAGTS$Cl.L1 &           # 第4根开盘 <= 第3根收盘
      Cl_LAGTS$Cl.L0 >= Op_LAGTS$Op.L3,            # 第4根收盘 >= 第1根开盘
    TS
  )

  result <- cbind(BullTLS, BearTLS)
  colnames(result) <- c("Bull.ThreeLineStrike", "Bear.ThreeLineStrike")
  xts::xtsAttributes(result) <- list(bars=4)
  return(result)
}
