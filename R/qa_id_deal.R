#' 按品牌处理问题答案及对话的ID化问题
#'
#' @param brand 品牌
#'
#' @return 返回消息内容
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' qalist_idize();
qalist_idize <- function(brand){

  #问题ID-------

  data_ques <- nsim_qalist_FQuestionText2(brand,unique = TRUE);
  #data_ques <- unique(data_ques)

  ques_count <- length(data_ques);
  data_ques <- data.frame(FQuestion=data_ques,
                          FBrand=rep(brand,ques_count),
                          stringsAsFactors = F);
  #将数据保存到数据库
  nsim_save(data_ques,"item_question",id_var = "FQuestionId");


  #答案ID处理------
  data_answer <- nsim_qalist_FAnswerText2(brand,unique = TRUE);
  #data_answer <- unique(data_answer);

  answer_count <- length(data_answer);
  data_answer <- data.frame(FAnswer=data_answer,
                            FBrand=rep(brand,answer_count),
                            stringsAsFactors = F);
  #将数据保存到数据库
  nsim_save(data_answer,"item_answer",id_var = "FAnswerId");

  #还原到问题列表的ID----
  data_vw_qa_id <- nsim_vw_qalist_id();
  count_vw_qa_id <- nrow(data_vw_qa_id );
  #fix the bug
  #字段名称区分大小写FQestionId与FQuestionID是不同的字段
  nsim_save(data_vw_qa_id,'qalist_ID','FQAId');



  #弹出消息进行提醒
  msg_q <-paste("处理了",nrow(data_ques),"条问题ID化记录",sep="");
  msg_a <-paste("处理了",answer_count,"条答案ID化记录",sep="");
  msg_qa_id <-paste("处理了",count_vw_qa_id,"条QA-ID化记录",sep="");
  msg <- c(msg_q,msg_a,msg_qa_id);
  msg <-paste(msg,collapse = "\n");
  return(msg)
}

