rm(list=ls())
library(rvest)
library(tidyverse)
library(stringr)
library(readr)
setwd("F:/")##需要手动在F盘新建一个data文件夹
# 首先，构建仅爬取一页的Function:
crawl_one_page <- function(key="data analyst", i = 1) {
  # key 为爬取岗位关键词， i 为页码
  base_url <- "https://www.liepin.com/zhaopin/?&fromSearchBtn=2"
  #限定职位类型的一些参数：&jobKind=1，&compKind=010等
  key <- iconv(key, to = "UTF-8") # 保证关键词文本为UTF-8编码
  # 构建URL并加码。注意：别忘了curPage是从0开始的。
  liepin_url <- str_c(base_url, "&key=", key, "&curPage=", (i-1)) %>% 
    URLencode()
  # 由于网站信息可能随时更新，所以下载网页方便验证数据准确性。
  file_name <- str_c("data/", key, i, ".html") #构建文件路径，事先建一个data文件夹
  download_html(liepin_url, file_name)
  ##转码
  dir<-"F:/data"
  setwd(dir)
  fl_1<-dir() #将目录列表赋值给变量转码
  fl_2<-iconv(fl_1,"utf-8")
  file.rename(fl_1, fl_2)
  setwd("F:/")
  #下面就是真正从网页上抓取数据了，建议使用SelectorGadget定位信息节点，非常方便。
  lp_html <- read_html(file_name)
  position <- lp_html %>%     #岗位名称
    html_nodes(".job-info h3 a") %>%
    html_text() %>%
    str_trim() # 删除多余的空白
  company <- lp_html %>%      #公司名称
    html_nodes(".company-name a") %>%
    html_text()
  req_info <- lp_html %>%     #岗位要求，例如：12-18万_北京_大专及以上_5年以上
    html_nodes(".condition") %>%
    html_attr("title") # get title attribute
  #.job-description
  link<- lp_html %>% 
    html_nodes('.job-info,h3 a')%>%
    html_attrs()
  link1<-c(1:length(link))  #初始化一个和link长度相等的link1
  for(i in 1:length(link))
    link1[i]<-link[[i]][1]
  tab2<-data.frame(unlist(link1))
  del <- seq(1, nrow(tab2), by = 2)
  #tab2<-tab2[-del,]
  #tab2<-data.frame(tab2)
  tab2<-data.frame(tab=link1[-del])
  #职位要求
  req_tb<- str_split(req_info, "_", simplify = TRUE) %>% 
    as_tibble()
  colnames(req_tb) <- c("salary", "location", "education", "experience")
  pos_tb <- tibble(position, company) %>% 
    bind_cols(req_tb)
  tab<-data.frame(pos_tb,tab2)
  return(tab)
}
# 之后，构建爬取n个网页的function：
crawl_n_page <- function(key = "data analyst", n = 1){
  pos_lt <- vector("list", n) #事先分配存储空间，有利于提高效率——Hadley
  for (i in seq_len(n)) {
    pos_lt[[i]] <- crawl_one_page(key, i) 
    cat("Crawling page:", i, "\tProgress:", (i/n)*100, "%\n") # 显示进度
    Sys.sleep(2)#两秒运行一次，防封IP
  }
  pos_tb <- bind_rows(pos_lt) #将list转换为tibble
  return(pos_tb)
}
#爬取10页数据分析岗位招聘信息,更改搜索关键词仅需在这里输入
position <- "数据分析"
#首先尝试爬取1页是否成功： 
pos_tb1 <- crawl_one_page(position, 2)
#然后爬取100页内容
pos_tb <- crawl_n_page(position, 40)
##加网址前缀
pos_tb$tab<-sub("/a/", "https://www.liepin.com/a/", pos_tb$tab)
#剔除重复网址（样本）
pos_tb<-pos_tb[!duplicated(pos_tb$tab),]
##剔除无效网址（样本）
pos_tb<-pos_tb[-c(grep("/rpojob/",pos_tb$tab)),]
colnames(pos_tb)<-c("position","company","salary","location","education","experience","link")

##读取数据基本信息及网址
mydata<-pos_tb
colnames(mydata)<-c("position","company","salary","location","education","experience","link")
colnames(mydata)
rownames(mydata)<-c(1:nrow(mydata))
workneed<-data.frame()
industry<-data.frame()
size<-data.frame()

##定义爬取工作要求的函数
fun01<-function(i){
  workneed[i,1]<-page%>%
    html_nodes('.job-description')%>%
    html_text(trim =TRUE)
}
fun02<-function(i){
  size[i,1] <- page %>%      
    html_nodes(".new-compintro li:nth-child(2)") %>%
    html_text()
}
fun03<-function(i){
  industry[i,1] <- page %>%      
    html_nodes(".new-compintro a ") %>%
    html_text()
}
##爬取所属行业/公司规模/岗位描述
for(i in 1:nrow(mydata)){
  url <- as.character(mydata$link[i])
  page=read_html(url)
  x.a <- try(fun01(i), silent=TRUE)
  if ('try-error' %in% class(x.a)) next
  else workneed[i,1]<-page%>%
    html_nodes('.job-description')%>%
    html_text(trim =TRUE)
  x.b <- try(fun02(i), silent=TRUE)
  if ('try-error' %in% class(x.c)) next
  else size[i,1] <- page %>%      
    html_nodes(".new-compintro li:nth-child(2)") %>%
    html_text() 
  x.c <- try(fun03(i), silent=TRUE)
   if ('try-error' %in% class(x.b))
    industry[i,1] <- page %>%      
    html_nodes(".new-compintro li:nth-child(1)") %>%
    html_text()
  else industry[i,1] <- page %>%      
    html_nodes(".new-compintro a") %>%
    html_text()
  cat("Crawling page:", i, "\tProgress:", (i/nrow(mydata))*100, "%\n") # 显示进度
}
##合并爬取信息
size$V1<-sub("公司规模：", " ", size$V1)
data<-cbind(mydata,workneed,industry,size)
colnames(data)<-c("position","company","salary","location","education","experience","link",
                  "V1","industry","size")
