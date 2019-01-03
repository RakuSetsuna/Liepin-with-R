rm(list=ls())
##下载并加载必要的包并做必要设置：
install.packages("rvest")
install.packages("tidyverse ")
install.packages("stringr ")
install.packages("readr ")
library(rvest)
library(tidyverse)
library(stringr)
library(readr)
setwd("F:/") ##设置工作目录
dir.create("data") #创建一个文件data
##第一层爬虫：
# 首先，构建仅爬取一页的Function:
crawl_one_page <- function(key="data analyst", i = 1) {
  # key 为爬取岗位关键词， i 为页码
  base_url <- https://www.liepin.com/zhaopin/?&&fromSearchBtn=2
#限定职位类型的一些参数：&jobKind=1，&compKind=010等
  key <- iconv(key, to = "UTF-8") # 保证关键词文本为UTF-8编码
  # 构建URL并加码。注意：curPage是从0开始的。
  liepin_url <- str_c(base_url, "&key=", key, "&curPage=", (i-1)) %>% 
    URLencode()
  # 由于网站信息可能随时更新，所以下载网页方便验证数据准确性。
  file_name <- str_c("data/", key, i, ".html") #构建文件路径
  download_html(liepin_url, file_name)
  ##防止中文乱码，转码
  dir<-"F:/data"##这里的路径改为工作目录F盘的data文件夹
  setwd(dir)
  fl_1<-dir() #将目录列表赋值给变量转码
  fl_2<-iconv(fl_1,"utf-8")
  file.rename(fl_1, fl_2)
  setwd("F:/") ##转回原工作目录
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
  pos_lt <- vector("list", n) #事先分配存储空间，有利于提高效率
  for (i in seq_len(n)) {
    pos_lt[[i]] <- crawl_one_page(key, i) 
    cat("Crawling page:", i, "\tProgress:", (i/n)*100, "%\n") # 显示进度
    Sys.sleep(1)##爬取间隔1秒1次
  }
  pos_tb <- bind_rows(pos_lt) #将list转换为tibble
  return(pos_tb)
}

##开始爬取第一层信息：
#爬取10页数据分析岗位招聘信息
position <- "数据分析"##在此处可修改所需爬取岗位的关键词
#首先尝试爬取1页是否成功： 
pos_tb1 <- crawl_one_page(position, 2)
#然后爬取100页内容
pos_tb <- crawl_n_page(position, 100)
##加网址前缀
pos_tb$tab<-sub("/a/", "https://www.liepin.com/a/", pos_tb$tab)
#剔除重复网址（样本）
pos_tb<-pos_tb[!duplicated(pos_tb$tab),]
##剔除无效网址（样本）
pos_tb<-pos_tb[-c(grep("/rpojob/",pos_tb$tab)),]
colnames(pos_tb)<-c("position","company","salary","location","education","experience","link")


##第二层爬虫：
##读取数据基本信息及网址
mydata<-pos_tb4
colnames(mydata)<-c("position","company","salary","location","education","experience","link")
colnames(mydata)
rownames(mydata)<-c(1:nrow(mydata))
##建三个数据框用于储存将要爬取的三类信息
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
##开始爬取第二层信息：所属行业/公司规模/岗位描述
for(i in 1:nrow(mydata)){
  url <- as.character(mydata$link[i])
  page=read_html(url)
  x.a <- try(fun01(i), silent=TRUE)
  if ('try-error' %in% class(x.a)) next
  else workneed[i,1]<-page%>%
    html_nodes('.job-description')%>%
    html_text(trim =TRUE)
  x.b <- try(fun02(i), silent=TRUE)
  if ('try-error' %in% class(x.b)) next
  else size[i,1] <- page %>%      
    html_nodes(".new-compintro li:nth-child(2)") %>%
    html_text() 
  x.c <- try(fun03(i), silent=TRUE)
  if ('try-error' %in% class(x.c))
    industry[i,1] <- page %>%      
    html_nodes(".new-compintro li:nth-child(1)") %>%
    html_text()
  else industry[i,1] <- page %>%      
    html_nodes(".new-compintro a") %>%
    html_text()
  cat("Crawling page:", i, "\tProgress:", (i/nrow(mydata))*100, "%\n") # 显示进度
}
##合并二次爬取的信息
size$V1<-sub("公司规模：", " ", size$V1)
data<-cbind(mydata,workneed,industry,size)
colnames(data)<-c("position","company","salary","location","education","experience","link",
                  "V1","industry","size")


##重新爬取新的内容时需要运行以下代码：
unlink("data", recursive=TRUE)#删除原来的data文件夹以及其中下载下来的网页
dir.create("data") #创建一个新的文件夹data

