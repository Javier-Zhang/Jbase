#' Cox Analysis
#'
#' @param para Variable for analysis
#' @param data Clinial Matrix
#' @param OS Column names representing survival status
#' @param OS_time Column names representing survival time
#' @param censored OS time cutoff; 60 is recommended
#'
#' @return
#' Cox univariate and multivariate results
#' @export
#'
#' @examples
#' Mul_cox <- function(variables = NULL, data = NULL, p_cut = 0.25, censored = NULL, OS = "OS", OS_time = "OS_time", included_uni = F)

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


# 多因素分析（必须加载Uni_cox）
Mul_cox <- function(variables = NULL, data = NULL, p_cut = 0.25, censored = NULL, OS = "OS", OS_time = "OS_time", included_uni = T) {
  # 截断值处理
  if(is.numeric(censored)){
    data[, OS] = ifelse(data[, OS_time] > censored, 0, data[, OS])
    data[, OS_time] = ifelse(data[, OS_time] > censored, censored, data[, OS_time])
  }

  Uni_res <- lapply(variables, function(x) {Uni_cox(para = x, OS = OS, OS_time = OS_time, data = data, return_raw = T)})
  Uni_res <- ldply(Uni_res, function(x) data.frame(x, check.names = F))
  mul_var = Uni_res %>% .[.[, "p"] < p_cut, "Variables"] #筛选符合条件的变量

  my_surv = Surv(data[, OS_time], data[, OS] == 1)
  FML <- as.formula(paste0 ("my_surv~",
                            paste0(mul_var, collapse = "+")))
  cox <- coxph(FML, data = data)
  cox1 <- summary(cox)

  #多重共线性检验
  #print(car::vif(cox, data = temp_OS[, c(mul_var, OS, OS_time)]))

  HR <- round(cox1$coefficients[,2],2)
  PValue <- sapply(cox1$coefficients[,5], function(x) ifelse(x < 0.001, "<0.001", formatC(x, digits = 3, format="f", flag = "#")), simplify = "array")
  CI5 <- round(cox1$conf.int[,3],2)
  CI95 <- round(cox1$conf.int[,4],2)

  Mul_res <- data.frame('Variables' = row.names(cox1[["coefficients"]]),
                        #'Characteristics' = x,
                        'HR' = sprintf("%.2f", HR),
                        #'CI5' = CI5,
                        #'CI95' = CI95,
                        '95% CI' = paste0(sprintf("%.2f", CI5),'-',sprintf("%.2f", CI95)),
                        'p' = PValue,
                        check.names = F)

  if (included_uni == T) {
    Uni_res <- lapply(variables, function(x) {Uni_cox(para = x, OS = OS, OS_time = OS_time, data = data, return_raw = F)})
    Uni_res <- ldply(Uni_res, function(x) data.frame(x, check.names = F))
    Mul_res = left_join(Uni_res, Mul_res, by = c("Variables" = "Variables"), suffix = c(".uni", ".mul"))
  }


  #生成全变量多因素分析，缔造格式化表格，From-ggforest
  my_surv = Surv(data[, OS_time], data[, OS] == 1)
  FML <- as.formula(paste0 ("my_surv~",
                            paste0(variables, collapse = "+")))
  cox <- coxph(FML, data = data)

  #多重共线性检验 https://stackoverflow.com/questions/23518075/testing-multicollinearity-in-cox-proportional-hazards-using-r
  print(rms::vif(cox))

  # get data and variables/terms from cox model
  terms <- attr(cox$terms, "dataClasses")[-1]

  # 校正Event和case的数量
  sub_data = na.omit(data[,c(variables, OS, OS_time)])
  print(paste0("n= ",cox$n,", number of events= ",  cox$nevent))
  # extract statistics for every variable
  allTerms <- lapply(seq_along(terms), function(i){
    var <- names(terms)[i]
    if (terms[i] %in% c("factor", "character")) {
      adf <- as.data.frame(table(sub_data[, var]))
      adfe <- table(sub_data[, var], sub_data[, OS])[,2]
      cbind(var = var, adf, Event = adfe, pos = 1:nrow(adf))
    }
    else if (terms[i] == "numeric") {
      #  data.frame(var = var, Var1 = "", Freq = nrow(data),
      #             pos = 1)
      #修改这部分代码，原Freq计算不准确, 只计算含有该数据的病人数。
      data.frame(var = var, Var1 = "", Freq = sum(!is.na(sub_data[, var])),
                 Event = sum(sub_data[, OS][!is.na(sub_data[, var])]),
                 pos = 1)
    }
    else {
      vars = grep(paste0("^", var, "*."), coef$term, value=TRUE)
      data.frame(var = vars, Var1 = "", Freq = nrow(sub_data),
                 pos = seq_along(vars))
    }
  })
  allTermsDF <- do.call(rbind, allTerms)
  colnames(allTermsDF) <- c("var", "level", "N", "Event","pos")
  inds <- apply(allTermsDF[,1:2], 1, paste0, collapse = "")

  #合并
  rownames(Mul_res) <- gsub(Mul_res$Variables, pattern = "`", replacement = "")
  Mul_res <- cbind(allTermsDF, Mul_res[inds,])[,c("var", "level", "N", "Event", colnames(Mul_res)[-1])]
  colnames(Mul_res)[1:4] = c("Variables", "Category", "No.", "Event")

  #以下顺序一定不能乱
  NC_var = setdiff(Mul_res$Variables, mul_var)
  Mul_res$Variables[duplicated(Mul_res$Variables)] = ""
  Mul_res$HR.mul[is.na(Mul_res$HR.uni)] = "Reference"
  Mul_res$HR.uni[is.na(Mul_res$HR.uni)] = "Reference"
  Mul_res[which(Mul_res$Variables %in% NC_var),"HR.mul"] = "Not included"
  return(Mul_res)
}
