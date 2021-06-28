#' Get statistical results of column
#'
#' @param col column of a dataframe
#'
#' @return
#' Arranged statistical results, frequency, percent.
#' @export
#' @examples
#' Stat_col(iris$Species)
Stat_col <- function(col = NULL) {
  Num <- stats::ftable(col)
  Prop <- prop.table(Num)
  Sum <- merge(as.data.frame(Num), as.data.frame(Prop), by = c("col")) # 合并频数/频率表
  Sum <- Sum[order(Sum[, 2], decreasing = T), ] # 从大到小排序
  rownames(Sum) <- seq_len(nrow(Sum))
  names(Sum) <- c("Group", "Num", "Prop")
  Sum$Prop <- scales::percent(Sum$Prop, accuracy = 0.01) # 百分比显示
  print(Sum)
}
