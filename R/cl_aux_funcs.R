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
