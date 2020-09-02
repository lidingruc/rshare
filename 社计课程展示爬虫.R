library(tidyverse)
library(rvest)
library(stringr)
html <- read_html('http://www.mca.gov.cn/article/sj/xzqh/2020/2020/202003301019.html')
html %>% 
  html_table() -> table
table %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  select(X2, X3) %>% 
  slice(-1, -2, -3) %>% 
  purrr::set_names("行政区划代码", "单位名称") %>% 
  dplyr::filter(str_length(行政区划代码) == 6) -> mytable
# 市
mytable %>% 
  dplyr::filter(str_sub(行政区划代码, 5, 6)  == "00")
# 省
mytable %>% 
  dplyr::filter(str_sub(行政区划代码, 3, 6)  == "0000")
# 区县
mytable %>% 
  dplyr::filter(str_sub(行政区划代码, 5, 6)  != "00")


#######抓广州全城20页 
#爬天河区使用下面的网址即可
#http://gz.lianjia.com/zufang/tianhe/
require(RCurl)         ##载入包
require(xml2)
rm(list = ls())
GZsource <- data.frame()
system.time(for (i in 1:20) {
  if(i==1){webside ="https://gz.lianjia.com/zufang/"}
  else{webside = paste("https://gz.lianjia.com/zufang/pg", i, "/", sep = "")}
  url = getURL(webside, .encoding = "utf-8")
  url_parse = read_html(url, encoding = "utf-8")
  # 标题、链接//
  node = xml_find_all(url_parse,"//div[@class='content__list--item']//a[@class='content__list--item--aside']")
  title_name = sapply(node, function(X) xml_attr(X, "title"))
  Encoding(title_name) = "UTF-8"
  link = sapply(node, function(X) xml_attr(X, "href"))
  #区域、地段、小区、户型、面积、楼层
  node = xml_find_all(url_parse, "//p[@class='content__list--item--des']")
  inf1 = sapply(node, xml_text)
  inf1a = strsplit(inf1,"/\n")
  inf1b = sapply(inf1a, "[",1)
  inf1b = strsplit(inf1b,"-")
  xiaoqu=sapply(inf1b, "[",3)
  diduan=sapply(inf1b, "[",3)
  area=sapply(inf1b, "[",3)
  house_type = sapply(inf1a, "[",3)
  sizeraw=sapply(inf1a, "[",2)
  size = as.numeric(gsub("[^0-9]*$", "",sizeraw))
  level = sapply(inf1a, "[",4)                 
  # 价格
  node = xml_find_all(url_parse, "//span[@class='content__list--item-price']")
  price = as.numeric(gsub("[^0-9]", "", sapply(node, xml_text)))
  GZsource = rbind(GZsource, data.frame(title_name, link, xiaoqu, house_type, size,level, area, diduan,price, stringsAsFactors = FALSE))
  Sys.sleep(1)
}) 


devtools::install_github('awhstin/awtools')
