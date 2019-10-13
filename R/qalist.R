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




#' 处理QA配对信息
#'
#' @param brand 品牌
#' @param startDate 开始日期
#' @param endDate 结束日期
#'
#' @return 返回值
#' @import tsdo
#' @import tsda
#' @export
#'
#' @examples
#' qalist_gen();
qalist_gen <- function(brand,startDate,endDate){
  sql_qalist <- paste("select FUser,FLog,FDate,FTime from im_raw_record
where FBrand ='",brand,"'
and FDate between '",startDate,"' and '",endDate,"'",sep="")
  conn_nsim <- conn_nsim();

  res <- sql_select(conn_nsim,sql_qalist);
  brand_sql <- paste("select FName from brand
where FNumber ='",brand,"'",sep="")
  brand_name <- sql_select(conn_nsim,brand_sql);
  brand_name <- brand_name$FName;
  #处理QA生成对功能
  names(res) <- c('aut_id','logContent','dlg_date','dlg_hms');
  #进一步处理
  res$isA <- str_contain(res$aut_id,brand_name)
  #增加对数据完整性的判断
  del_row <-del_aq(res$isA,len=10)+1;
  all_row <-nrow(res);
  res <- res[del_row:all_row, ];
  #判断会话的分组
  res$gp_id <-res$aut_id;
  res$gp_id[res$isA ==TRUE] <- "";
  res$gp_id <- str_copyPrevRow(res$gp_id);
  res$session_id <- getSessionId(res$isA);
  #按列筛选数据
  res <- res %>% df_selectCol(c('dlg_date','gp_id','session_id','isA','logContent'))
  res$action_id <- res$session_id *2;
  res$action_id[res$isA == FALSE] <- res$action_id[res$isA == FALSE] -1;
  # 数据进行分组处理
  sep <- " "
  g <-res$action_id;
  res <- split(res,g);
  # 针对多行数据进行合并；
  res <- lapply(res, log_combine_gp,sep=sep);
  #将结合调整回一个数据框；
  res <- do.call("rbind",res);

  #将数据进一步进行处理
  #删除actionId
  res <- res[,c( "dlg_date","gp_id","session_id","isA","logContent")]
  res_q <- res[res$isA == FALSE,];
  res_q$question <- res_q$logContent;
  res_a <- res[res$isA == TRUE,];
  res_a$answer <- res_a$logContent;
  res <- df_innerJoin_bySameColNames(res_q,res_a,"session_id");
  res_name_sel <-c("dlg_date.x","gp_id.x","session_id","question","answer")
  res<-res[,res_name_sel];
  #names to display---------
  names(res) <-c("FDate","FGroupId","FSessionId","FQuestionText","FAnswerText")
  #修正FSessionID错误
  ncount <- nrow(res);
  res$FSessionId <-1:ncount;
  res$FBrand <-rep(brand,ncount);
  res <- res[,c("FBrand","FDate","FGroupId","FSessionId","FQuestionText","FAnswerText")]
  res <- res[len(res$FGroupId) <=40 ,]


  data <- res;

  data_date <- unique(data[,c('FBrand','FDate')]);

  ncount <- nrow(data_date);
  msg <- list_init(ncount);

  for (i in 1:ncount){
    var_brand1 <-data_date$FBrand[i];
    var_date1 <-data_date$FDate[i];
    if(is_new_qalist(var_brand1,var_date1)){
      data1 <- data[data$FBrand == var_brand1 & data$FDate == var_date1,]
      qa_write_data(data1);
      msg[[i]] <-paste(var_brand1,"于",var_date1,"QA配对上传了",nrow(data1),"行记录",sep="")
    }else{
      msg[[i]] <-paste(var_brand1,"于",var_date1,"QA配对上传了","0行记录",sep="")
    }

  }
  msg <- list_as_vect(msg)
  data <-list(res,msg)
  return(data)


}








