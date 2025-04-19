CSPNHigherClose <- function(TS, N) {
  if (!quantmod::has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  if (N < 1) {
    stop("N must be an integer >= 1")
  }

  # 显式提取Close列并生成滞后序列
  close_prices <- quantmod::Cl(TS)
  lagged_closes <- do.call(merge, lapply(0:N, function(k) lag(close_prices, -k)))
  colnames(lagged_closes) <- paste0("Close.L", 0:N)

  # 逐级比较连续N日的收盘价
  result <- lagged_closes[, 1] > lagged_closes[, 2]
  for (i in 2:N) {
    result <- result & (lagged_closes[, i] > lagged_closes[, i+1])
  }

  # 格式调整
  result <- reclass(result, TS)
  colnames(result) <- paste(N, "HigherClose", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)
  return(result)
}

CSPNLowerClose <- function(TS, N) {
  if (!quantmod::has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  if (N < 1) {
    stop("N must be an integer >= 1")
  }

  # 显式提取Close列并生成滞后序列
  close_prices <- quantmod::Cl(TS)
  lagged_closes <- do.call(merge, lapply(0:N, function(k) lag(close_prices, -k)))
  colnames(lagged_closes) <- paste0("Close.L", 0:N)

  # 逐级比较连续N日的收盘价
  result <- lagged_closes[, 1] < lagged_closes[, 2]
  for (i in 2:N) {
    result <- result & (lagged_closes[, i] < lagged_closes[, i+1])
  }

  # 格式调整
  result <- reclass(result, TS)
  colnames(result) <- paste(N, "LowerClose", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)
  return(result)
}
