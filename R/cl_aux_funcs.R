# 1. 判断是否为最新的语料-------
#' 判断是否为最新的语料
#'
#' @param FBrand 品牌
#' @param FDate 日期
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' is_new_cl_data();
is_new_cl_data <- function(FBrand,FDate){
  conn_nsim <- conn_nsim();
  sql <- paste("select FLog from im_raw_record where FBrand = '",
               FBrand,
               "' and FDate = '",
               FDate,
               "' ;",sep = "");
  data <-sql_select(conn_nsim,sql);
  if(nrow(data) == 0)
  {
    res <-TRUE
  }else{
    res <-FALSE
  }
  return(res);
}


# 取得原始语料的最大行--------
#' 取得原始语料的最大行
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' max_cl_id()
max_cl_id <- function(){

  res <- nsim_max_id('im_raw_record','FId');

  return(res);
}





#  处理语料数据保存-------
#' 处理语料数据保存问题
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cl_write_data();
cl_write_data <- function(data){
  ncount <- nrow(data);
  conn_nsim <- tsda::conn_nsim();
  data$FId <- max_cl_id()+1:ncount;
  data$FNeedRBind<-rep(0,ncount);
  data$FRbindId <- rep(0,ncount);
  data <-data[,c('FId','FUser','FDatetime','FLog','FDate','FTime','FBrand','FNeedRBind','FRbindId')]
  tsda::db_writeTable(conn=conn_nsim,table_name = 'im_raw_record',r_object = data,append = T)


}


