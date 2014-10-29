# 返回var_list中 加入model adjust R-squared 最大的 变量名，返回变量名
#  var_list: 可选变量列表
#  f: 已有变量公式: resp_var~ 或 resp_var~ x1 + x2...
#  data
sel_var <-function(f, var_list, data) {
  if (grepl("~$", f)) {  # resp_var~ 
    init_adj_r_squared <- 0
  }  else {  # resp_var~ x1 + x2...
    fit<-do.call("lm", list(as.formula(f), data))
    init_adj_r_squared<-summary(fit)$adj.r.squared
  }
  
  max_adj_r_squared <-  init_adj_r_squared
  idx_max <- NA
  for (v in var_list ) {
    formu<-as.formula(paste(f,"+",v)) 
    fit<-do.call("lm", list(formu,data))
    
    if (summary(fit)$adj.r.squared > max_adj_r_squared) {
      max_adj_r_squared <- summary(fit)$adj.r.squared
      idx_max <- v
    }
    
    cat(v,"--- adjust R squared =", round(summary(fit)$adj.r.squared,5),"\n")
  }
  
  cat("*********************************************************************************\n")
  cat("max adjust R squared =", round(max_adj_r_squared,5), ", var: ", idx_max,"\n")
  cat("\n")
  
  idx_max
}
  
# forward selection - adjusted R2
# resp_var: 输出变量的名字
# must_list：必选变量列表，格式 c(x1,x2,...)
# option_list: 可选变量列表, 格式 c(x1,x2,...)
model_sel<-function(resp_var, option_list, must_list=NULL, data) {
  fs<-paste(resp_var,"~")
  plus_flag<-FALSE
  
  if (length(must_list)>0) {
    for (v in must_list) {
      fs<-paste(fs,v,"+")
    }  
    fs<-strtrim(fs,nchar(fs)-1) # 去掉最后的“+”
    plus_flag<-TRUE
  }  
  
  var_list<-setdiff(option_list, must_list)
  stepno<-0
  repeat  {
    cat("\nStep ", stepno,": ", fs, "\n")
    selected<- sel_var(fs, var_list, data)
    if (!is.na(selected)) {
       if (plus_flag) {
         fs <- paste(fs, "+", selected)
       } else {
         fs <- paste(fs, selected)
         plus_flag=TRUE
       }
     var_list<- setdiff(var_list, selected)
     stepno<-stepno+1
    } else {
      break
    }
  }
  
 cat("**************************** Result fit model **************************************\n")
 cat(fs,"\n")
}

