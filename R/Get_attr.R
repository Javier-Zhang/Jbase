#' Get attribution
#'
#' @param col column of a dataframe
#'
#' @return Arranged statistical results
#' @export
#' @import
#' @examples
#' Get_attr(iris$Species)
Get_attr = function(col = NULL, print = T){
  temp = table(col) %>% names()
  temps = paste(1:length(temp), temp, sep = ".")
  if (print == T) {
    cat(temps, sep = "\n")
  }
  invisible(temp)
}

