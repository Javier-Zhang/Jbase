#' Format pvalue
#'
#' @param p p value
#' @param accuracy cutoff for Scientific notation
#' @param flag Whether to keep the trailing 0
#'
#' @return
#' Formatted p value
#' @export
#'
#' @examples
#' format_pvalue(p = 0.05, accuracy = 0.01)
#' format_pvalue(p = 0.005, accuracy = 0.01)
format_pvalue <- function(p = NULL, accuracy = 0.01, flag = "#"){
  if(!(p <= 1)){
    stop(
      "p should be < 1",
      call. = FALSE
    )
  } else if (p < accuracy) {
    p = formatC(p, digits = 1, format="e", flag= flag)
  } else {
    p = formatC(p, digits = 2, format="fg", flag = flag) #fg时digist指定有效数字，f时指定小数点后位数
  }
  return(p)
}

