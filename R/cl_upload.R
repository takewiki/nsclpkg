# 读取原始的语料信息----------
#' 读取原始的语料信息
#'
#' @param files 文件
#' @param brand 品牌
#'
#' @return 返回值
#' @import nrcsrobot
#' @export
#'
#' @examples
#' cl_raw_read();
cl_raw_read <- function(files,brand){
  data <- read_kflogs(files);
  ncount <-nrow(data);
  data$FBrand <- rep(brand,ncount)
  names(data) <-c('FUser','FDatetime','FLog','FDate','FTime','FBrand')
  #剔除日期的异常数据
  data <- data[len(data$FDatetime)<=19,]
  data <- data[left(data$FDatetime,2) =='20' |left(data$FDatetime,2) =='19', ]
  #删除过长的用户信息
  data <- data[len(data$FUser) <=100,]
  return(data)

}

# 上传原始语料进入服务器-------
#' 上传原始语料进入服务器
#'
#' @param data 数据
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' cl_raw_upload_with_msg();
cl_raw_upload_with_msg <- function(data){

  data_date <- unique(data[,c('FBrand','FDate')]);

  ncount <- nrow(data_date);
  msg <- list_init(ncount);

  for (i in 1:ncount){
    var_brand1 <-data_date$FBrand[i];
    var_date1 <-data_date$FDate[i];
    if(is_new_cl_data(var_brand1,var_date1)){
      data1 <- data[data$FBrand == var_brand1 & data$FDate == var_date1,]
      cl_write_data(data1);
      msg[[i]] <-paste(var_brand1,"于",var_date1,"原始语料上传了",nrow(data1),"行记录",sep="")
    }else{
      msg[[i]] <-paste(var_brand1,"于",var_date1,"原始语料上传了","0行记录",sep="")
    }

  }
  msg <- list_as_vect(msg)
  return(msg)
}



#' 根据品牌及开始日期结束日期查询相应的语料信息
#'
#' @param brand 品牌
#' @param startDate 开始日期
#' @param endDate 结束日期
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' cl_raw_query();
cl_raw_query <-function(brand,startDate,endDate){

   sql_query <- paste(
      "select b.FName as '品牌名称' ,a.FDate as '业务日期',a.FCount as '语料行数'  from  vw_brand_rawMaterial_count a
         inner join brand b
         on a.FBrand = b.FNumber
         where FBrand ='",brand,
      "' and FDate between '",startDate,
      "' and '",endDate,"'
         order by FBrand desc ,FDate desc",sep=""
    )
    conn_nsim <- tsda::conn_nsim();
    data <- sql_select(conn_nsim,sql_query);
    return(data)

}


