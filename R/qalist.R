# 判断是否为最新的QA对信息------
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

# 获取QAList的最大码------
#' 获取QAList的最大码
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' max_qa_id
max_qa_id <- function(){
  #处理方式非常漂亮！ 2019-10-13
  res <- nsim_max_id('qalist_raw','FSessionId')
  return(res);
}

# 保存QA数据------
#' 保存QA数据
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' qa_write_data();
qa_write_data <- function(data){
  ncount <- nrow(data);
  conn_nsim <- tsda::conn_nsim()
  data$FSessionId <- max_qa_id()+1:ncount;
  data <-data[,c("FBrand","FDate","FGroupId","FSessionId","FQuestionText","FAnswerText")]
  tsda::db_writeTable(conn=conn_nsim,table_name = 'qalist_raw',r_object = data,append = T)

}
