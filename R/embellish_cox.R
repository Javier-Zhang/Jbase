#' Modify the results of COX analysis
#'
#' @param data Mul_cox results
#' @param Characteristics Characteristics
#'
#' @return
#' Cox results
#' @export
#'
#' @examples
#'
#'

embellish_cox <- function(data = NULL, Characteristics = NULL){
  data = add_column(data, `HR (95% CI).uni` = with(data, {if_else(is.na(HR.uni) | HR.uni %in% c("Reference"), HR.uni, paste0(HR.uni, " (", `95% CI.uni`, ")"))}), .before = "HR.uni")
  data = add_column(data, `HR (95% CI).mul` = with(data, {if_else(is.na(HR.mul) | HR.mul %in% c("Reference", "Not included"), HR.mul, paste0(HR.mul, " (", `95% CI.mul`, ")"))}), .before = "HR.mul")
  data = add_column(data, `Cases/Events` =with(data, {paste0(No., "/", Event)}), .before = "No.")
  data  = data[, c(1:3,6,9,10,13)]
  colnames(data)[4:7] = rep(c("HR (95% CI)","p-value"), times = 2)
  rownames(data) = 1:nrow(data)
  if (!is.null(Characteristics)) {
    data$Variables[data$Variables != ""] = Characteristics
  }
  return(data)
}
