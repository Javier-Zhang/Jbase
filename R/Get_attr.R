#' Get attribution
#'
#' @param col column of a dataframe
#' @param print Whether or not to print
#'
#' @return
#' Arranged statistical results.
#' @export
#'
#' @examples
#' Get_attr(iris$Species)
Get_attr <- function(col = NULL, print = T) {
  temp <- table(col) %>% names()
  temps <- paste(seq_len(length(temp)), temp, sep = ".")
  if (print == T) {
    cat(temps, sep = "\n")
  }
  invisible(temp)
}
