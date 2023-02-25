#' Uni Cox Analysis
#'
#' @param para Variable for analysis
#' @param data Clinial Matrix
#' @param OS Column names representing survival status
#' @param OS_time Column names representing survival time
#' @param censored OS time cutoff; 60 is recommended
#' @param return_raw test
#'
#' @return
#' Cox univariate and multivariate results
#' @export
#'
#' @examples
#' Uni_cox <- function(variables = NULL, data = NULL, p_cut = 0.25, censored = NULL, OS = "OS", OS_time = "OS_time", included_uni = F)

# 批量单因素Function
Uni_cox <- function(para = NULL, data = NULL, OS = "OS", OS_time = "OS_time", censored = 60, return_raw = F){
  # 截断值处理
  if(is.numeric(censored)){
    data[, OS] = ifelse(data[, OS_time] > censored, 0, data[, OS])
    data[, OS_time] = ifelse(data[, OS_time] > censored, censored, data[, OS_time])
  }

  my_surv = Surv(data[, OS_time], data[, OS] == 1)
  FML <- as.formula(paste0 ("my_surv~", para))
  cox <- coxph(FML, data = data)
  cox1 <- summary(cox)
  #print(cox1)

  n = length(cox$coefficients)
  HR <- round(cox1$coefficients[,2],2)
  PValue <- sapply(cox1$coefficients[,5], function(x) ifelse(x < 0.001, "<0.001", formatC(x, digits = 3, format="f", flag = "#")), simplify = "array")
  CI5 <- round(cox1$conf.int[,3],2)
  CI95 <- round(cox1$conf.int[,4],2)
  #CI <- paste0(round(cox1$conf.int[,3:4],2),collapse = '-')
  Uni_cox_raw<- data.frame('Variables' = para,
                           'HR' = sprintf("%.2f", HR),
                           #'CI5' = CI5,
                           #'CI95' = CI95,
                           '95% CI' = paste0(sprintf("%.2f", CI5),'-',sprintf("%.2f", CI95)),
                           'p' = PValue,
                           check.names = F)
  Uni_cox <- data.frame('Variables' = row.names(cox1[["coefficients"]]),
                        #'Characteristics' = x,
                        'HR' = sprintf("%.2f", HR),
                        #'CI5' = CI5,
                        #'CI95' = CI95,
                        '95% CI' = paste0(sprintf("%.2f", CI5),'-',sprintf("%.2f", CI95)),
                        'p' = PValue,
                        check.names = F)

  ifelse(return_raw == T, return(Uni_cox_raw), return(Uni_cox))
}
