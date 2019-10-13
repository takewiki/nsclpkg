#' 判断是否为最新的QA对信息
#'
#' @param FBrand  品牌
#' @param FDate 日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' is_new_qalist();
is_new_qalist <- function(FBrand,FDate){
  conn_nsim <- tsda::conn_nsim()
  sql <- paste("select 1 from qalist_raw  where FBrand = '",
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
