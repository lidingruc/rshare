##
# 利用百度慧眼数据计算 2020年新冠肺炎疫情发生后各个地方生产生活恢复正常的进度
###
#李丁  中国人民大学社会与人口学院
#2020年2月13日下午开始
#2020年2月15日修改
################################
# 1.加载包
################################
library(RCurl) # 爬虫
library(xml2) # 爬虫
library(jsonlite) #json处理
library("ggpubr") # 图片组合
library(tidyverse) # 数据整理和呈现
library(GGally) # 相关矩阵
library(sf)  # 地图数 
library(worldtilegrid) ## 地图主题
library(hrbrthemes)  ## 地图主题

library(lubridate) # 日期函数
enfont="Arial"
################################
#2.公共数据和变量
################################
#城市代码
citycode <- read.csv("/Users/liding/E/Bdata/rtemp/citycode.csv")
#除夕对齐（现在可以不用了，已经改为函数自动计算）
dataday <- read.csv("/Users/liding/E/Bdata/rtemp/dataday.csv")
#城市分类
citykind<- read.csv("/Users/liding/E/Bdata/rtemp/outplc.csv")
#读入疫情数据
codata <- read.csv("/Users/liding/E/Bdata/rtemp/cityconvirvusdata20200216.csv",header=TRUE)
# 地级市特征数据
hedudata <- read.csv("/Users/liding/E/Bdata/rtemp/2003－2017年城市数据库.csv",header=TRUE) %>% filter(year==2017) %>% select(hedu,adcode)

pst<- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}


################################
#3. 抓取数据市内出行强度
################################
all <- data.frame()

for( i in 1:dim(citycode)[1]) {
  ourl <-pst("http://huiyan.baidu.com/migration/internalflowhistory.jsonp?dt=city&id=",as.character(citycode$code[i]),"&date=20200507")
  response <- getURL(ourl)
  txt <- substr(response,49,(nchar(response)-3))
  dat <- jsonlite::fromJSON(txt)
  data <- data.frame(unlist(dat))
  names(data) <- "value"
  data$date <- rownames(data)
  data$citycode=citycode$code[i]
  data$cityname=citycode$city[i]
  all <- rbind(all,data)
  Sys.sleep(3)
}

#all <- all[-(all$citycode==130700 & all$cityname=="张家界"),]
#all2 <- all2[-(all2$citycode==659008),]
#all2 <- all2[-(all2$citycode==152900),]
write.csv(all,file="/Users/liding/E/Bdata/rtemp/internalflowcity507.csv",row.names=FALSE)


########################################
#4. 爬取返城数据
########################################

pst<- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}

# join(myData, countryData, by='nation', type='left', match='all')
allmv <- data.frame()

for( i in 1:dim(citycode)[1]) {
  inurl <-pst("http://huiyan.baidu.com/migration/historycurve.jsonp?dt=province&id=",as.character(citycode$code[i]),"&type=move_in")
  response <- getURL(inurl)
  txt <- substr(response,49,(nchar(response)-3))
  dat <- jsonlite::fromJSON(txt)
  data <- data.frame(unlist(dat))
  names(data) <- "value"
  data$date <- rownames(data)
  data$adcode=citycode$code[i]
  data$cityname=citycode$city[i]
  data$inout="inval"
  allmv <- rbind(allmv,data)
  Sys.sleep(3)
  
  outurl <-pst("http://huiyan.baidu.com/migration/historycurve.jsonp?dt=province&id=",as.character(citycode$code[i]),"&type=move_out")
  
  response <- getURL(outurl)
  txt <- substr(response,49,(nchar(response)-3))
  dat <- jsonlite::fromJSON(txt)
  data <- data.frame(unlist(dat))
  names(data) <- "value"
  data$date <- rownames(data)
  data$adcode=citycode$code[i]
  data$cityname=citycode$city[i]
  data$inout="outval"
  allmv <- rbind(allmv,data)
  Sys.sleep(3)
}

#all <- all[-(all$citycode==130700 & all$cityname=="张家界"),]
#all2 <- all2[-(all2$citycode==659008),]
#all2 <- all2[-(all2$citycode==152900),]
write.csv(allmv,file="/Users/liding/E/Bdata/rtemp/historycurve427.csv",row.names=FALSE)


###
#http://huiyan.baidu.com/migration/provincerank.jsonp?dt=city&id=340100&type=move_in&date=20200411

#http://huiyan.baidu.com/migration/cityrank.jsonp?dt=province&id=110000&type=move_in&date=20200411

#http://huiyan.baidu.com/migration

allmv <- data.frame()

for( i in 1:dim(citycode)[1]) {
  inurl <-pst("http://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=",as.character(citycode$code[i]),"&type=move_in&date="&date[j])
  
  inur2 <-pst("http://huiyan.baidu.com/migration/provincerank.jsonp?dt=city&id=",as.character(citycode$code[i]),"&type=move_in&data="&date[j])
  
  outurl <-pst("http://huiyan.baidu.com/migration/cityrank.jsonp?dt=city&id=",as.character(citycode$code[i]),"&type=move_out&data="&date[j])
  
  inurl <-pst("http://huiyan.baidu.com/migration/provincerank.jsonp?dt=city&id=",as.character(citycode$code[i]),"&type=move_out&data="&date[j])

}


quanqiu <- xml2::read_html("/Users/liding/Downloads/疫情数据/quanqiu.html")


########################################
##5. 读入何凌锋第一次爬取的数据
########################################
## 流入流出
# path = "/Users/liding/E/Bdata/rtemp/baiduhuiyan/1_move_in/"
# fileName = dir(path)
# 
# mvinall <- data.frame()
# for(k in 1:length(fileName)){
#   mvin = read.csv(file = paste(path,fileName[k],sep = ""),
#                   header = T,stringsAsFactors = F)
#   mvin$citycode <- substr(fileName[k],1,6)
#   mvinall <-  rbind(mvinall,mvin)
# }
# names(mvinall)[2] <- "inval"
# mvinall$citycode <- as.numeric(mvinall$citycode)
# 
# path = "/Users/liding/E/Bdata/rtemp/baiduhuiyan/1_move_out/"
# fileName = dir(path)
# moall <- data.frame()
# for(k in 1:length(fileName)){
#   mo = read.csv(file = paste(path,fileName[k],sep = ""),
#                 header = T,stringsAsFactors = F)
#   mo$citycode <- substr(fileName[k],1,6)
#   moall <-  rbind(moall,mo)
# }
# names(moall)[2] <- "outval"
# moall$citycode <- as.numeric(moall$citycode)
# 
# move <- merge(x=mvinall, y=moall, by.x=c("citycode","date"), by.y=c("citycode","date"), all.x=T, all.y=T)
# 
# #move$citycode <- as.numeric(move$citycode)
# names(move)[1] <- "adcode"
# names(citycode)[2] <- "adcode"
# move <- merge(x=move, y=citycode)
# #rm(list=c("mvinall", "moall", "mvin",  "mo"))
#何凌锋部分结束
##########


########################################
#市内出行强度数据清理 和初步描述
########################################
all <- read.csv("/Users/liding/E/Bdata/rtemp/internalflowcity426.csv")%>% 
  arrange(citycode,date) %>% 
  left_join(citykind,by=c("citycode"="adcode"))  %>% 
  mutate(year=if_else(date<20200101,"2019","2020"),
         ymddate=ymd(date),
         num=if_else(year=="2019",as.numeric((ymddate-ymd(20190204)),"days"),as.numeric((ymddate-ymd(20200124)),"days")),
         afcx=if_else(num>0,"afy","bfy"))
table(all$num)
 
# 原始数据描述，湖北主要城市（411500,430600,）,year=="2020"
all %>% filter(afcx =="afy") %>% 
  dplyr::filter(citycode %in% c(420100,420600,420900,421000,421100,421200,422800,429005)) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=num,y=value,color=cityname,linetype=cityname,shape=cityname,group=cityname))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=cityname),size=2)+
  #scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  facet_wrap(~year)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti"),
        legend.title=element_blank()) +
  labs(title = "湖北主要市的市内出行强度",x="春节后日数",y="强度") 


# 原始数据描述，主要城市
all %>% filter(afcx =="afy") %>% 
  dplyr::filter(citycode %in%c(440300,441900,440100,440600)) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=num,y=value,color=cityname,linetype=cityname,shape=cityname,group=cityname))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=cityname),size=2)+
  scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  facet_wrap(~year)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti"))+
  labs(title = "Internal flow of GD cities") 


all %>% filter(afcx =="afy") %>% 
  filter(citycode %in%c(110000,310000,120000,440100)) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=num,y=value,color=cityname,linetype=cityname,shape=cityname,group=cityname))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=cityname),size=2)+
  scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  facet_wrap(~year)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti"))+
  labs(title = "Internal flow of Big cities") 


all %>% filter(afcx =="afy") %>% 
  filter(citycode %in%c(110000,310000,440300,420100,650100,331000,230100)) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=num,y=value,color=cityname,shape=cityname,group=cityname))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=cityname),size=2)+
  scale_shape_manual(values=LETTERS[1:7])+
  #scale_color_brewer(palette = "Set1") +
  facet_wrap(~year)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti"))+
  labs(title = "Internal flow of Big cities") 



########################################
#返城数据初步处理与满血进度描述
########################################
move <- read.csv("/Users/liding/E/Bdata/rtemp/historycurve427.csv")%>%  spread(key=inout,value=value)

names(move)[3:5] <- c("city","inval","outval")

move <- move %>%
  mutate(year=if_else(date<20200101,"2019","2020"),
         ymddate=ymd(date),
         num=if_else(year=="2019",as.numeric((ymddate-ymd(20190204)),"days"),as.numeric((ymddate-ymd(20200124)),"days")),
         net=inval-outval,
         afcx=if_else(num>0,"afy","bfy"))%>% 
  left_join(citykind,by="adcode") 



tmove <- move %>% group_by(adcode,year,afcx) %>% 
  summarise(tnet=sum(net)) %>% 
  unite(abyear,afcx,year) %>% 
  spread(key=abyear,value=tnet) #%>% head()


# 需求量
move %>% left_join(tmove,by="adcode") %>% 
  left_join(hedudata,by='adcode') %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020) %>% 
  filter(afcx=="afy",year=="2020",date==20200321) %>% 
  ungroup() %>% 
  arrange(bfy_2020) %>%
  select(city,bfy_2020,rcidx,hedu) %>% head(50) %>% 
  mutate(need=round( -bfy_2020*(1-rcidx)*8 - 0.6*hedu/10000,digits=1),
         rcidx=round(rcidx*100,1),
         bfy_2020=round(- bfy_2020*8,1),
         hedu=round(hedu/10000,1))%>% 
  arrange(desc(need)) %>% DT::datatable()

## 满血进度 1
## 一线城市 c(110000,310000,440100,440300)
## 温台地区 c(331000,330300,330600,350500)
## 浙江城市 c(330100,330200,330300,330700)
## 广东地区 c(440300,441900,440100,440600)
## 中部城市 c(510100,430100,360100,420100)


move %>% left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter(adcode %in%c(110000,310000,440100,440300,420100,430100,610100,331000,650100,420200)) %>%
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  scale_shape_manual(values = c(2,3,5,1,4,6,7,8,9,10)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16),
        legend.title=element_blank())+
  labs(title = "几个典型城市满血进度",x="日期",y="返城率") 

#110000,310000,440300,420100,650100,331000,230100

## 满血进度 2
move %>% left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter(adcode %in%c(330100,331000,330300,330600,350500,320500,350100)) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  scale_linetype_manual(values = c(2,3,5,1,4,6,7)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16),
        legend.title=element_blank())+
  labs(title = "几个民企大市的满血进度",x="日期",y="返城率") 


## 湖北各市进度
move %>% left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020,
         prov=floor(adcode/10000)) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  # filter(adcode %in%c(110000,420100,420200,420300,420400,420500,420600,420700,420800)) %>% 
  filter((prov==42 | prov==11),adcode != 429021) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  #scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))+
  labs(title = "湖北各市的返城率(与北京对比)",x="日期",y="返城率") 

## 湖北各市进度
move %>%  left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020,
         prov=floor(adcode/10000)) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter(adcode %in%c(411500,430600,420100,420600,420900,421000,421100,421200,422800,429005)) %>% #DT::datatable()
  #filter((prov==42 | prov==11),adcode != 429021) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  guides(linetype=guide_legend(title=""),
         shape=guide_legend(title=""),
         color=guide_legend(title=""))+
  scale_shape_manual(values = LETTERS[1:10] +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))+
  labs(title = "湖北主要地区外出-返城率:与岳阳及信阳对比",x="日期",y="返城率") 

## 湖南和江西的主要城市
move %>%  left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020,
         prov=floor(adcode/10000)) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter(adcode %in%c(430100,360100,360700,360800,360900,361000,361100,360400,430400,430500,430600,430700,430900,431000,431100,431200,431300)) %>% #DT::datatable()
  #filter((prov==42 | prov==11),adcode != 429021) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  #scale_linetype_manual(values=LETTERS[1:17])+
  scale_shape_manual(values=LETTERS[1:17])+
  facet_wrap(~prov)+
  guides(linetype=guide_legend(title=""),
         shape=guide_legend(title=""),
         color=guide_legend(title=""))+
  #scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))+
  labs(title = "湖南江西主要地区外出-返城率",x="日期",y="返城率") 


# 湖南与广西45，贵州52
move %>%  left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020,
         prov=floor(adcode/10000)) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter((prov==43 | prov==52)) %>% 
  mutate(provname=if_else(prov==43,"湖南","贵州")) %>% 
  filter(bfy_2020 > 10 | bfy_2020< -10) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  #scale_linetype_manual(values=LETTERS[1:17])+
  scale_shape_manual(values=LETTERS[1:19])+
  facet_wrap(~provname)+
  guides(linetype=guide_legend(title=""),
         shape=guide_legend(title=""),
         color=guide_legend(title=""))+
  #scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))+
  labs(title = "湖南贵州主要地区外出-返城率",x="日期",y="返城率") 


# 湖南与河南41 安徽34
move %>%  left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020,
         prov=floor(adcode/10000)) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter((prov==43 | prov==34)) %>% 
  filter(bfy_2020 > 15 | bfy_2020< -10) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  #scale_linetype_manual(values=LETTERS[1:17])+
  scale_shape_manual(values=LETTERS[1:19])+
  facet_wrap(~prov)+
  guides(linetype=guide_legend(title=""),
         shape=guide_legend(title=""),
         color=guide_legend(title=""))+
  #scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))+
  labs(title = "湖南安徽主要地区外出-返城率",x="日期",y="返城率") 

# 浙江、江苏
move %>%  left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020,
         prov=floor(adcode/10000)) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter((prov==32 | prov==33)) %>% 
  mutate(provname=if_else(prov==32,"江苏","浙江")) %>% 
  filter(bfy_2020 > 15 | bfy_2020< -10) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  #scale_linetype_manual(values=LETTERS[1:17])+
  scale_shape_manual(values=LETTERS[1:19])+
  facet_wrap(~provname)+
  guides(linetype=guide_legend(title=""),
         shape=guide_legend(title=""),
         color=guide_legend(title=""))+
  #scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))+
  labs(title = "江苏浙江主要地区外出-返城率",x="日期",y="返城率") 

# 浙江、江苏
move %>%  left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020,
         prov=floor(adcode/10000)) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter((prov==44 | prov==35)) %>% 
  mutate(provname=if_else(prov==44,"广东","福建")) %>% 
  filter(bfy_2020 > 15 | bfy_2020< -10,adcode!=441800) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  #scale_linetype_manual(values=LETTERS[1:17])+
  scale_shape_manual(values=LETTERS[1:19])+
  facet_wrap(~provname)+
  guides(linetype=guide_legend(title=""),
         shape=guide_legend(title=""),
         color=guide_legend(title=""))+
  #scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))+
  labs(title = "广东福建主要地区外出-返城率",x="日期",y="返城率") 



########################################
#市内流动强度指标汇总
########################################
### 指标处理
#dataday <- dataday %>% mutate(wk=week(ymddate),wkday=wday(ymddate))

#lastd20 <- all %>% group_by(year) %>% summarise(td=max(num)) %>% .[[2]]%>% .[[2]]
#lastd19 <- all%>% group_by(year) %>% summarise(td=max(num)) %>% .[[2]]%>% .[[1]]
lastd20 = 50
lastd19 =57 
data1 <- all %>% 
  mutate(b=cut(num,breaks=c(-24,-3,0,lastd20,lastd19), right = TRUE, labels=FALSE),
         a7=cut(num,breaks=c(-24,7,lastd20,lastd19), right = TRUE, labels=FALSE),
         b7=cut(num,breaks=c(-24,0,7,lastd19), right = TRUE, labels=FALSE),
         a15= cut(num,breaks=c(-24,15,lastd20,lastd19), right = TRUE, labels=FALSE) ,
         w4=cut(num,breaks=c(-24,0,7,9,14,16,21,23,28,30,35,37,42,44,49,51,56,lastd19), right = TRUE, labels=FALSE),
         lwk=if_else((date>20190324 & date<=20190328),1,if_else((date>=20200316 & date<=20200320),2,0))) 

# 节前、节后
sdata1 <- data1 %>% 
  filter(b!=2,b!=4) %>% 
  group_by(citycode,cityname,year,b) %>% 
  summarize(aint=mean(value)) %>%
  unite(var,year,b,remove=TRUE) %>% 
  spread(key=var,value=aint)
names(sdata1)[3:6] <- c("byr19","ayr19","byr20","ayr20")

#初七之后
sdata2 <- data1 %>% 
  filter(a7!=1,a7!=3) %>% 
  group_by(citycode,cityname,year,a7) %>% 
  summarize(aint=mean(value)) %>% 
  unite(var,year,a7,remove=TRUE) %>%
  spread(key=var,value=aint) 

names(sdata2)[3:4] <- c("a719","a720")

#正月十五之后
sdata3 <- data1 %>%
  group_by(citycode,cityname,year,a15) %>% 
  summarize(aint=mean(value)) %>% 
  filter(a15!=1,a15!=3) %>% 
  unite(var,year,a15,remove=TRUE) %>%
  spread(key=var,value=aint)

names(sdata3)[3:4] <- c("a1519","a1520")

sdata4 <- data1 %>% 
  group_by(citycode,cityname,year,b7) %>% 
  summarize(aint=mean(value)) %>% 
  filter(b7!=1,b7!=3,b7!=5) %>% 
  unite(var,year,b7,remove=TRUE) %>%
  spread(key=var,value=aint) #%>% head()

names(sdata4)[3:4] <- c("b719","b720")

sdata5 <- data1 %>% 
  group_by(citycode,cityname,year,w4) %>% 
  summarize(aint=mean(value)) %>% 
  filter(w4!=1,w4!=3,w4!=5,w4!=7,w4!=9,w4!=11,w4!=13,w4!=15,w4!=17) %>% 
  unite(var,year,w4,remove=TRUE) %>%
  spread(key=var,value=aint)  # %>% names()

names(sdata5)[3:18] <- c("w519","w619","w719","w819","w119","w219","w319","w419","w520","w620","w720","w820","w120","w220","w320","w420")

# 最新日期  廿七日 二十日
# 最新日期  廿八日 廿一日
# 最近一周相对于2019年最后四天记录（阴历2月19日到2月22日）
sdata6 <- data1 %>% 
  select(citycode,cityname,year,lwk,value) %>% 
  filter(lwk!=0) %>%
  group_by(citycode,cityname,year,lwk) %>% 
  summarise(value=mean(value)) %>% 
  unite(var,year,lwk,remove=TRUE) %>%
  spread(key=var,value=value) %>% 
  ungroup() %>% 
  mutate(incr=.[[4]]-.[[3]]) #%>% head()

names(sdata6)[3:4] <- c("lwk19","lwk20")

sdata <- list(sdata1,sdata2,sdata3,sdata4,sdata5,sdata6) %>% 
  reduce(full_join, by = c("citycode","cityname") )

# 定义指标
sdata$brate=sdata$byr20/sdata$byr19
sdata$rate19=sdata$ayr20/sdata$ayr19
sdata$b7rate=sdata$b720/sdata$b719
sdata$a7rate=sdata$a720/sdata$a719
sdata$a15rate=sdata$a1520/sdata$a1519
sdata$w3rc19=sdata$w320/sdata$w319
sdata$w4rc19=sdata$w420/sdata$w419
sdata$w32rate=sdata$incr/sdata$lwk19
sdata$a21rate=sdata$lwk19/sdata$b720
sdata$a28rate=sdata$lwk20/sdata$b720
sdata$w2r=sdata$w220/sdata$b720
sdata$w3r=sdata$w320/sdata$b720
sdata$w4r=sdata$w420/sdata$b720
sdata$w5r=sdata$w520/sdata$b720
sdata$w6r=sdata$w620/sdata$b720
sdata$w7r=sdata$w720/sdata$b720
sdata$w8r=sdata$w820/sdata$b720
sdata$w3r_19=sdata$w319/sdata$b719

# 修正基准出行强度
sdata$rate19_2=sdata$ayr20/sdata$ayr19/sdata$brate
sdata$b7rate_2=sdata$b720/sdata$b719/sdata$brate
sdata$a7rate_2=sdata$a720/sdata$a719/sdata$brate
sdata$a15rate_2=sdata$a1520/sdata$a1519/sdata$brate
sdata$w3rc19_2=sdata$w320/sdata$w319/sdata$brate

write.csv(sdata,file="/Users/liding/E/Bdata/rtemp/internalrecoverindex321.csv",row.names = FALSE)


########################################
#返城进度指标汇总
########################################
wkmig <- move %>% mutate(wk=cut(date,breaks=c(20190111,20200126,20200202,20200209,20200216,20200223,20200301,20200308,20200315,20200322,20200329,20200405), right = TRUE, labels=FALSE)) %>% 
  filter(wk !=1) %>% 
  group_by(adcode,city,wk) %>%
  summarise(net=sum(net),
            inval=sum(inval),
            outval=sum(outval)) %>% 
  gather(key=var,value=value,net,inval ,outval) %>% #head()
  unite(wkmig,var,wk,remove=TRUE) %>% #head()
  spread(key=wkmig,value=value) 
names(wkmig)

write.csv(wkmig,file="/Users/liding/E/Bdata/rtemp/wkmig-411.csv",row.names=FALSE)


##################################
# 9.2 年后返城的总情况（累计）情况
##################################
## 2019年和2020年的返城比  净流入
table(move$num)
bk1920 <- move %>% filter(num<79) %>% 
  group_by(adcode,city,year,afcx) %>%
  summarise(net=sum(net),
            liuru=sum(inval),
            liuchu=sum(outval))%>% # head(20) 
  gather(key=var,value=value,liuru,liuchu,net) %>%
  unite(yliuru, var, afcx, year, remove = TRUE) %>% # head() 
  spread(key=yliuru,value=value) %>%  # head()
  mutate(plc=if_else((net_bfy_2019< -5 & net_afy_2019 > 5),"inplc",if_else((net_bfy_2019>5 & net_afy_2019 < -5),"outplc","elsplc"))) %>% 
  mutate(
    bkrc19ain=liuru_afy_2020/liuru_afy_2019/0.7,
    otrc19aout=liuchu_afy_2020/liuchu_afy_2019/0.7,
    bkrc20bout=liuru_afy_2020/liuchu_bfy_2020/0.7,
    otrc20bin=liuchu_afy_2020/liuru_bfy_2020/0.7,
    bkrc19anet=net_afy_2020/net_afy_2019,
    bkrc19bnet=-net_afy_2020/net_bfy_2019,
    bkrc20bnet=-net_afy_2020/net_bfy_2020,
    bc19net=net_bfy_2020/net_bfy_2019,
    bc19in=liuru_bfy_2020/liuru_bfy_2019,
    bc19out=liuchu_bfy_2020/liuchu_bfy_2019,
    bk20= if_else(bkrc20bnet >1,1,(if_else(bkrc20bnet< -1,-1,bkrc20bnet))))

write.csv(bk1920,file="/Users/liding/E/Bdata/rtemp/bk1920-4111.csv",row.names=FALSE)


################################
#整合数据
################################
bk1920 <- read.csv("/Users/liding/E/Bdata/rtemp/bk1920-321.csv")
wkmig<- read.csv("/Users/liding/E/Bdata/rtemp/wkmig-321.csv")
city_distribution <- read_csv('/Users/liding/E/Bdata/rtemp/internalrecoverindex321.csv') %>%dplyr::rename(adcode=citycode)

# 基于程振兴整合性地图的改进图
# 南海诸岛一次成图
citymap<- sf::st_read("/Users/liding/E/Bdata/RSource/map/china_city_full_map2.json")%>% #st_set_geometry(NULL) %>% write.csv("/Users/liding/E/Bdata/RSource/map/china_city_full_map2.csv")
  left_join(codata,by="adcode") %>% 
  left_join(city_distribution,by="adcode") %>% 
  left_join(bk1920,by="adcode") %>% 
  left_join(wkmig,by="adcode")

#省边界
cn_boundary<- sf::st_read("/Users/liding/E/Bdata/RSource/map/china_prov_full_nine.json")


ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill =if_else(lwk19>6,6,lwk19)),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "Arial") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank())

## 竖版地图
library(sf)
citymap <- sf::st_read("/Users/liding/E/Bdata/RSource/map/china_city.json")%>%
  left_join(codata,by="adcode") %>% 
  left_join(city_distribution,by="adcode") %>% 
  left_join(bk1920,by="adcode") %>% 
  left_join(wkmig,by="adcode")
#citymap <- cbind(citymap, st_coordinates(st_centroid(citymap)))
# 设定投影方式
st_crs(citymap)
citymap<- st_transform(citymap, "+init=epsg:4508")
# 国界线
cn_boundary <- sf::st_read("/Users/liding/E/Bdata/RSource/map/sheng_full/100000_full.json")
st_crs(cn_boundary)
cn_boundary<- st_transform(cn_boundary, "+init=epsg:4508")
#cn_boundary <- sf::st_read("/Users/liding/E/Bdata/RSource/map/chinaprov_full_long.json")


# citymap2 <- citymap %>% 
#   left_join(bk1920,by="adcode") %>% 
#   left_join(wkmig,by="adcode")


################################
#7. 市内出行强度分析
################################

## 描述分析，指标之间的相关性
scatmat(city_distribution , columns = (17:ncol(city_distribution)), color = NULL, alpha = 1)+theme_bw()

scatmat(city_distribution , columns = c("rate19","a7rate","a15rate","a24rate", "rate19_2","a7rate_2","a15rate_2","a24rate_2"), color = NULL, alpha = 1)+theme_bw()

#相对于上周的增量（相对于正月初七的增量）
scatmat(city_distribution , columns = c("w21rate","w20rate"), color = NULL, alpha = 1)+theme_bw()

# 正月28全国城市出行强度,因为出行强度本来就是出行人数相对于本地常住人口的比值
# 因此其本来就是一个相对指标，在全国不同城市之间是可以进行比较的

# 2019年记录最后一周的出行强度
ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill =if_else(lwk19>6,6,lwk19)),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "Arial") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(-2600000, 2400000), ylim = c(2000000, 6000000))


ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill =if_else(lwk20-lwk19 >0,0,if_else(lwk20-lwk19 < -3, -3, lwk20-lwk19))),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "Arial") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(-2600000, 2400000), ylim = c(2000000, 6000000))


#+ 
#  coord_sf(xlim = c(75, 133), ylim = c(18, 53.5),
#           crs = "+proj=longlat", expand = TRUE)

# 2月28日相对于2月21日的增量
w32 <- ggplot() +
  geom_sf(data = citymap, aes(fill = if_else(w32rate > 0.75,0.75,w32rate)),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "internalflow incr this to lastwk") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
w32


## 新疆特殊情况
p1 <-ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill =if_else(w4r > 1,1,w4r)),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "STKaiti") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "2月23日-27日/正月七天出行强度的比值") +
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(-2600000, 2400000), ylim = c(2000000, 6000000))

p2 <- ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill =if_else(w4r > 1,1,w4r)),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_modern_rc(base_family = enfont) +
  worldtilegrid::theme_enhance_wtg()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        plot.background = element_blank()) + 
  coord_sf(xlim = c(-80000, 1500000), ylim = c(200000, 2500000))

# 2月23日到2月27日出行强度相对于正月初七前的比值
summary(citymap$w4r)
citymap<- st_transform(citymap, "+init=epsg:4508")

p1 <- ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill =if_else(w5r< 1,1,if_else(w5r > 2.5,2.5,w5r) ) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "STKaiti") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "3月2日-6日/正月七天出行强度的比值") +
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(-2600000, 2400000), ylim = c(2000000, 6000000))
# 注意单位是米，设定 赤道和东经111度为原点，左边2800公里，右边2200公里，上方2000-6000公里

p2 <- ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill = if_else(w5r < 1,1,if_else(w5r > 2.5,2.5,w5r) ) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_modern_rc(base_family = enfont) +
  worldtilegrid::theme_enhance_wtg()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        plot.background = element_blank()) + 
  coord_sf(xlim = c(-80000, 1500000), ylim = c(200000, 2500000))

#合并两个图
library(cowplot)
ggdraw() + 
  draw_plot(p1) + 
  draw_plot(p2, x = 0.78, y = -0.32, width = 0.25)


# 2月月28日相对于正月初七之前的出行密度
#拼接南海诸岛版本：  最新的复工情况
# a24rate a24rate_2  #if_else(a24rate>1,1,a24rate)
summary(citymap$a28rate)
summary(citymap$a21rate)
# w10rate # if_else(w10rate>1.5,1.5,w10rate) 
# w21rate # if_else(w21rate<0,0,w21rate) 
# summary(citymap$w20rate)

p1 <- ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill =if_else(a28rate< 1,1,if_else(a28rate > 2,2,a28rate) ) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "Arial") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(75, 133), ylim = c(18, 53.5),
           crs = "+proj=longlat", expand = TRUE)

#使用下面的方式可以将九段线部分截取出来：
p2 <- ggplot() + 
  geom_sf(data = citymap, aes(geometry = geometry,
                              fill = if_else(a28rate < 1,1,if_else(a28rate > 2,2,a28rate) ) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_modern_rc(base_family = enfont) +
  worldtilegrid::theme_enhance_wtg()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        plot.background = element_blank()) + 
  coord_sf(xlim = c(107, 122), ylim = c(4, 24),
           crs = "+proj=longlat")

#合并两个图
library(cowplot)
ggdraw() + 
  draw_plot(p1) + 
  draw_plot(p2, x = 0.78, y = -0.29, width = 0.25)

####
#最近两周市内出行强度,回归系数相对于上周的百分比，100*b
####

gintwk12 <- wkmig%>%
  left_join(city_distribution,by="adcode") %>% 
  #filter(bfy < -3) %>% 
  mutate(clabel=if_else((net_2 < 1 & net_3<1),"",as.character(city))) %>% 
  mutate(clabel2=if_else(((a21rate<1.2  & a28rate<1.2) | clabel !=""),"",as.character(city))) %>% 
  ggplot(aes(x=a21rate,y= a28rate))+
  geom_point()+
  geom_smooth(method="lm")+
  stat_smooth_func(geom="text",method="lm",hjust=0.10,vjust=19.3,parse=TRUE) +
  geom_text(aes(label=clabel2,y=a28rate+0.05),family="STSong", color="red",size=5)+
  geom_text(aes(label=clabel,y=a28rate+0.05),family="STSong",color="grey30" ,size=5)+
  geom_segment(aes(x = 0, y = 0, xend = 2.5, yend = 2.5)) +
  theme_bw()+
  xlab("上周五市内流动强度")+
  ylab("本周五市内流动强度")+
  #labs(title = "2019 vs 2020")+
  theme(text=element_text(family="STKaiti",size=18))

gintwk12

# 取值比较小的点
gintwk12b <- wkmig%>%
  left_join(city_distribution,by="adcode") %>% 
  #filter(net_1 < 1 & net_2<1) %>% 
  mutate(clabel=if_else((a28rate< 1.0 & a21rate<1.0),"",as.character(city))) %>% 
  ggplot(aes(x=a21rate,y= a28rate))+
  geom_point()+
  geom_smooth(method="lm")+
  stat_smooth_func(geom="text",method="lm",hjust=0.10,vjust=19.3,parse=TRUE) +
  geom_text(aes(label=clabel,y=a28rate+0.05),family="STSong",color="grey30" ,size=5)+
  geom_segment(aes(x = -0, y = 0, xend = 2, yend = 2)) +
  theme_bw()+
  xlab("上周五市内流动强度")+
  ylab("本周五市内流动强度")+
  #labs(title = "2019 vs 2020")+
  theme(text=element_text(family="STKaiti",size=18))
gintwk12b 



################################
# 首先呈现疫情地图情况
nCov17 <- ggplot() +
  geom_sf(data = citymap, aes(fill = log(anum)),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "log n of Cov2019") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

## 最新一周的出行强度

ggplot() +
  geom_sf(data = citymap, aes(fill = if_else(w8r>5,5,w8r)),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "internalflow 2020 \n week 8") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

#最新一个工作日的
ggplot() +
  geom_sf(data = citymap, aes(fill = if_else(a28rate>5,5,a28rate)),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "internalflow 2020 \n the last workday") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# 第三周出行强度 相对于2019年,w4rc19
a24 <- ggplot() +
  geom_sf(data = citymap, aes(fill = if_else(w4rc19>1.5,1.5,w4rc19)),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "internalflow 2020 \n day 24-27") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

## 湖南和贵州的情况,人口流入地的出行强度增加，人口流出地出行强度下降。
hngz <- citymap %>% 
  dplyr::filter(prov %in% c(52,43)) %>%
  ggplot() +
  geom_sf(aes(fill = w6r),
          color = "white", size = 0.01) +
  geom_point(aes(x = X, y = Y), shape = 1, size = 1,
             color = "black") + 
  geom_text(aes(x = X, y = Y, label = cityname), 
            family = "STKaiti", 
            size = 3, color = "black") +
 # geom_sf(data = (cn_boundary %>% filter(name %in%c("贵州省","湖南省"))),fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "STKaiti"))+
labs(title = "廿四日市内出行强度恢复情况") +coord_sf(crs = "+proj=longlat")
  
## 上海、江苏、浙江的情况
csj <- citymap %>% 
  dplyr::filter(prov %in% c(31,32,33)) %>%
  ggplot() +
  geom_sf(aes(fill = w8r),
          color = "white", size = 0.01) +
  #geom_point(aes(x = X, y = Y), shape = 1, size = 1, color = "black") + 
  geom_text(aes(x = X, y = Y, label = if_else(cityname=="上海","",cityname)), 
            family = "STKaiti", 
            size = 3, color = "black") +
  # geom_sf(data = (cn_boundary %>% filter(name %in%c("贵州省","湖南省"))),fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "STKaiti"))+
  labs(title = "长三角") +coord_sf(crs = "+proj=longlat")
# +coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

## 京津冀
jjj <- citymap %>% 
  dplyr::filter(prov %in% c(11,12,13)) %>%
  ggplot() +
  geom_sf(aes(fill = w6r),
          color = "white", size = 0.01) +
  #geom_point(aes(x = X, y = Y), shape = 1, size = 1, color = "black") + 
  geom_text(aes(x = X, y = Y, 
                label = if_else(cityname=="北京","",
                                if_else(cityname=="天津","",cityname))), 
            family = "STKaiti", 
            size = 3, color = "black") +
  # geom_sf(data = (cn_boundary %>% filter(name %in%c("贵州省","湖南省"))),fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "STKaiti"))+
  labs(title = "京津冀") +coord_sf(crs = "+proj=longlat")
  
###
jjjcsj <- ggarrange(csj,jjj ,nrow = 1, ncol = 2,
                common.legend=TRUE,
                legend="right")
ggexport(jjjcsj,filename="/Users/liding/E/Bdata/rtemp/jjjcsj-internalmb-20200217.png",width=2508,height=1508,res=250)

## 广东 44
fj <- citymap %>% 
  dplyr::filter(prov ==35) %>%
  ggplot() +
  geom_sf(aes(fill = w20rate),
          color = "white", size = 0.01) +
  #geom_point(aes(x = X, y = Y), shape = 1, size = 1, color = "black") + 
  geom_text(aes(x = X, y = Y, 
                label = cityname), 
            family = "STKaiti", 
            size = 3, color = "black") +
  # geom_sf(data = (cn_boundary %>% filter(name %in%c("贵州省","湖南省"))),fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "STKaiti"))+
  labs(title = "福建") 

 ggarrange(csj,jjj ,gd,nrow = 1, ncol = 3,
                    common.legend=TRUE,
                    legend="right")

library(patchwork)  # great package!
fj|jjj

gd <- citymap %>% 
   dplyr::filter(prov ==44) %>%
   ggplot() +
   geom_sf(aes(fill = w20rate),
           color = "white", size = 0.01) +
   #geom_point(aes(x = X, y = Y), shape = 1, size = 1, color = "black") + 
   geom_text(aes(x = X, y = Y, 
                 label = cityname), 
             family = "STKaiti", 
             size = 3, color = "black") +
   # geom_sf(data = (cn_boundary %>% filter(name %in%c("贵州省","湖南省"))),fill=NA,color="black", size = 0.1) +
   scale_fill_viridis_c(begin = 0, end = 1,name = "") +
   theme_bw()+
   theme(plot.title = element_text(hjust=0.5),
         axis.ticks = element_blank(),
         axis.text  = element_blank(),
         axis.title = element_blank(),
         text = element_text(family = "STKaiti"))+
   labs(title = "广东") 
 
 ggarrange(csj,jjj ,gd,nrow = 1, ncol = 3,
           common.legend=TRUE,
           legend="right")
 
 
 sd <- citymap %>% 
   dplyr::filter(prov ==37) %>%
   ggplot() +
   geom_sf(aes(fill = w20rate),
           color = "white", size = 0.01) +
   #geom_point(aes(x = X, y = Y), shape = 1, size = 1, color = "black") + 
   geom_text(aes(x = X, y = Y, 
                 label = cityname), 
             family = "STKaiti", 
             size = 3, color = "black") +
   # geom_sf(data = (cn_boundary %>% filter(name %in%c("贵州省","湖南省"))),fill=NA,color="black", size = 0.1) +
   scale_fill_viridis_c(begin = 0, end = 1,name = "") +
   theme_bw()+
   theme(plot.title = element_text(hjust=0.5),
         axis.ticks = element_blank(),
         axis.text  = element_blank(),
         axis.title = element_blank(),
         text = element_text(family = "STKaiti"))+
   labs(title = "山东") 
 
 ggarrange(csj,jjj ,sd,nrow = 1, ncol = 3,
           common.legend=TRUE,
           legend="right")
 
# 出行强度
b7 <- ggplot() +
  geom_sf(data = citymap, aes(fill = b720),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "internalflow 2020 \n before7") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

a7 <- ggplot() +
  geom_sf(data = citymap, aes(fill = a720),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "internalflow 2020 \n after7") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# 出行强度
a15 <- ggplot() +
  geom_sf(data = citymap, aes(fill = a1520),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "internalflow 2020 \n after15") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")


# 出行强度
b719 <- ggplot() +
  geom_sf(data = citymap, aes(fill = b719),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name ="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title =  "internalflow 2019 \n before7") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")


a719 <- ggplot() +
  geom_sf(data = citymap, aes(fill = a719),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name ="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title =  "internalflow 2019 \n after7") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# 出行强度
a1519<- ggplot() +
  geom_sf(data = citymap, aes(fill = a1519),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name ="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title =  "internalflow 2019 \n after15") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

### 输出pdf文件

g2 <- ggarrange(b719,a719,a1519,b7,a7,a15 ,nrow = 2, ncol = 3,
                common.legend=TRUE,
                legend="right")
ggexport(g2,filename="/Users/liding/E/Bdata/rtemp/g2-internalmb-20200214.png",width=3283,height=2208,res=250)



#颜色的其他设置
#library("RColorBrewer")
#myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")),bias=0.5)
#sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))
#scale_fill_gradient(high = "red",low = "blue") +
#scale_fill_continuous(type = "viridis")+ #gradient

## 依据市内出行强度的复工程度
a7rate <- ggplot() +
  geom_sf(data = citymap, aes(fill = a7rate), size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "internalflow \n after7") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

a7rateb <- ggplot() +
  geom_sf(data = citymap, aes(fill = a7rate_2),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "recover rate \n after7b") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")


a15rate <- ggplot() +
  geom_sf(data = citymap, aes(fill = a15rate),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name ="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title =  "recover rate \n after15") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

a15rateb <- ggplot() +
  geom_sf(data = citymap, aes(fill = a15rate_2),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "recover rate \n after15b") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

rate19 <- ggplot() +
  geom_sf(data = citymap, aes(fill = rate19),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "recover \n rate19") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

rate19b <- ggplot() +
  geom_sf(data = citymap, aes(fill = rate19_2),
          color = "white", size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name = "") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -15,hjust=0.5))+
  labs(title = "recover \n rate19b") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

##输出结果

g1 <- ggarrange(rate19,a7rate, a15rate,rate19b,a7rateb,a15rateb ,nrow = 2, ncol = 3,
                common.legend=TRUE,
                legend="right")
ggexport(g1,filename="/Users/liding/E/Bdata/rtemp/g1-recover-20200214.png",width=3283,height=2208,res=250)


##################################
# 10.分析 比较不同地区的人口流动模式
##################################
move %>%group_by(year,num,plc) %>% 
  summarise(net=sum(net)) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=1)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  facet_wrap(~plc,ncol = 2,nrow = 2)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text = element_text(family = "STKaiti",size=16))+
  labs(title = "Net Mig of 3 kind of Cities") 

rcindex <- move %>% left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020,
         prov=floor(adcode/10000)) %>% 
  filter(afcx=="afy",year=="2020") 

write.csv(rcindex,file="/Users/liding/E/Bdata/rtemp/rcindex-229.csv",row.names=FALSE)


# 外出进度
#411600,500000,341200,361100
#360700,411400,411500,421100
#430500,440900,411300,430400 
move %>% left_join(tmove,by="adcode") %>% 
  arrange(adcode,date) %>% 
  group_by(adcode,year,afcx) %>% 
  mutate(rcidx=-cumsum(net)/bfy_2020) %>% 
  filter(afcx=="afy",year=="2020") %>% 
  filter(adcode %in%c(430500,440900,411300,430400)) %>% 
  mutate(date=ymd(date)) %>% 
  ggplot(aes(x=date,y=rcidx,color=city,linetype=city,shape=city,group=city))+
  geom_line(stat="identity",size=0.8)+
  geom_point(aes(shape=city),size=2)+
  scale_linetype_manual(values = c(2,3,5,1)) +
  #scale_color_brewer(palette = "Set1") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))+
  labs(title = "人口流出大市外出进度",x="日期",y="外出率") 



# 北京市2019年和2020年的流入流出情况
bjmove <- move %>% filter(adcode==110000) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=1)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Net Mig of BJ ") 
bjmove

shmove <- move %>% filter(adcode==310000) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Net Mig of SH ") 
shmove

whmove <- move %>% filter(adcode==420100) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=2)+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Net Mig of WH ") 
whmove


gzmove <- move %>% filter(adcode==440100) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_line(stat="identity",size=2)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Net Mig of GZ ") 
gzmove

szmove <- move %>% filter(adcode==440300) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=2)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Net Mig of SZ ") 
szmove

# 佛山 440600
# 东莞 441900
move %>% filter(adcode==441900) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=2)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Net Mig of FS ") 


g5<- ggarrange(bjmove,shmove,szmove,whmove ,nrow = 2, ncol = 2,
               common.legend=TRUE,
               legend="right")
ggexport(g5,filename="/Users/liding/E/Bdata/rtemp/g5-recover-20200214.png",width=2218,height=2208,res=250)

# 大城市
bsgw <- move %>% filter(adcode %in% c(110000,310000,440100,440300)) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=1)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  facet_wrap(~city,ncol = 2,nrow = 2)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text = element_text(family = "STKaiti",size=16))+
  labs(title = "Net Mig of Main Cities") 

ggexport(bsgw,filename="/Users/liding/E/Bdata/rtemp/bsgw-recover-20200214.png",width=2218,height=2208,res=250)

#廊坊这种城市比较特殊  131000
#泉州本地人先出去，工人后进来350500
# 330100	杭州
# 650100	乌鲁木齐
# 330400	嘉兴
# 610100	西安
# 360100	南昌
# 330700	金华
# 420100	武汉
# 330500	湖州
# 350500	泉州
# 330600	绍兴
# 330300	温州
# 331000	台州

PT <- move %>% filter(adcode==350300) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=2)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(title = "Net Mig of PT")

#温台地区
wtdiqu <- move %>% filter(adcode %in% c(331000,330300,330600,350500)) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=1.5)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  facet_wrap(~city,ncol = 2,nrow = 2)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text = element_text(family = "STKaiti"))+
  labs(title = "Net Mig of ZJ Cities") 

ggexport(wtdiqu,filename="/Users/liding/E/Bdata/rtemp/wtdiqu-recover-20200214.png",width=2218,height=2208,res=250)

#330500,330700  浙江主要城市
zjcities <- move %>% filter(adcode %in% c(330100,330200,330300,330700)) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=1)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  facet_wrap(~city,ncol = 2,nrow = 2)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text = element_text(family = "STKaiti",size=16))+
  labs(title = "Net Mig of ZJ Cities") 

ggexport(zjcities,filename="/Users/liding/E/Bdata/rtemp/zjcity-recover-20200214.png",width=2218,height=2208,res=250)


#珠三角
move %>% filter(adcode %in% c(440300,441900,440100,440600)) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=1)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  facet_wrap(~city,ncol = 2,nrow = 2)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text = element_text(family = "STKaiti",size=16))+
  labs(title = "Net Mig of GD Cities") 


ZBCS <- move %>% filter(adcode %in% c(510100,430100,360100,420100)) %>% 
  ggplot(aes(x=num,y=net,color=year,linetype=year,group=year))+
  geom_line(stat="identity",size=1)+
  geom_hline(yintercept=0, linetype="dashed", color = "grey70")+
  facet_wrap(~city,ncol = 2,nrow = 2)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0.5),
        text = element_text(family = "STKaiti",size=16))+
  labs(title = "Net Mig of Central Cities") 

ggexport(ZBCS,filename="/Users/liding/E/Bdata/rtemp/ZB-recover-20200214.png",width=2218,height=2208,res=250)


##################################
# 11. 探索汇总的流入流入
# 2019年和2020年流入流出散点图
##################################
#净流入数据 
lrdata <- move %>%
  #filter(num<28) %>% 
  group_by(adcode,city,year,afcx) %>% 
  summarise(liuru=sum(net))%>% 
  spread(key=afcx,value=liuru) 

#https://stackoverflow.com/a/27959418
library(devtools)
source_gist("524eade46135f6348140")

#流出地流入情况
lrdata%>% 
  filter(bfy>5) %>% 
  mutate(clabel=if_else(bfy>=5,as.character(city),"")) %>% 
  ggplot(aes(x=bfy,y= -afy,shape=year,group=year,color=year))+
  geom_point()+
  geom_text(aes(label=clabel,y= - afy+1),family="STSong",color="grey30" ,size=4)+
  geom_smooth(method="lm")+
  stat_smooth_func(geom="text",method="lm",hjust=0.10,vjust=0,parse=TRUE) +
  theme_bw()+
  xlab("年前净流入人口")+
  ylab("年后净流出人口")+
  #labs(title = "2019 vs 2020")+
  theme(text=element_text(family="STKaiti",size=18))


## 流入地
lrdata%>% 
  filter(bfy < -3) %>% 
  mutate(clabel=if_else(bfy < -20,as.character(city),"")) %>% 
  ggplot(aes(x=-bfy,y= afy,shape=year,group=year,color=year))+
  geom_point()+
  geom_text(aes(label=clabel,y= afy+1),family="STSong",color="grey30" ,size=4)+
  geom_smooth(method="lm")+
  stat_smooth_func(geom="text",method="lm",hjust=0.15,vjust=7.9,parse=TRUE) +
  geom_segment(aes(x = 0, y = 0, xend = 200, yend = 200),color="grey50") +
  theme_bw()+
  xlab("年前净流出人口")+
  ylab("年后净流入人口")+
  #labs(title = "2019 vs 2020")+
  theme(text=element_text(family="STKaiti",size=18))



################### 
#春节前后净流入人口数  两天的比较 ——这个不太好看   
# 有一些大城市2019年春节就有人口迁出的趋势？
# 深圳 东莞、上海、苏州、佛山、宁波、温州、台州
# 2019年的人口流入地：重庆、赣州、清远、滁州

netdata <- move %>% filter(num<28)%>% 
           group_by(adcode,city,year) %>% 
  summarise(liuru=sum(net)) %>% 
  spread(key=year,value=liuru)  
names(netdata)[3:4] <- c("net2019","net2020")

library(ggrepel)
netdata%>% 
  ggplot(aes(x=net2019,y=net2020))+
  geom_point()+
  geom_text(aes(label=city,y=net2020+5),family="STSong",color="grey30" ,size=3)+
  geom_vline(xintercept=0, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  theme_bw()+
  theme(text = element_text(family = "STKaiti"),
        plot.title = element_text(hjust=0.5))
 

################### 
##最近一周的净流入量相对上周的变
wkmig <- read.csv("/Users/liding/E/Bdata/rtemp/wkmig-321.csv")
gmigwk12 <- wkmig%>% 
  #filter(bfy < -3) %>% 
  mutate(clabel=if_else((net_1 < 1 & net_2<1),"",as.character(city))) %>% 
  ggplot(aes(x=net_1,y= net_2))+
  geom_point()+
  geom_text_repel(aes(label=clabel),family="STSong",color="grey30" ,size=5,
                  box.padding = unit(0.25, "lines"))+
  geom_segment(aes(x = -2.5, y = -2.5, xend = 16, yend = 16)) +
  theme_bw()+
  xlab("节后第一周净流入")+
  ylab("节后第二周净流入")+
  #labs(title = "2019 vs 2020")+
  theme(text=element_text(family="STKaiti",size=18))
gmigwk12
# 取值比较小的点
gmigwk12b <- wkmig%>% 
  filter(net_1 < 1 & net_2<1) %>% 
  # mutate(clabel=if_else((net_1 < 1 & net_2<1),"",as.character(city))) %>% 
  ggplot(aes(x=net_1,y= net_2))+
  geom_point()+
  geom_text(aes(label=city,y=net_2+0.1),family="STSong",color="grey30" ,size=5)+
  geom_segment(aes(x = -2.5, y = -2.5, xend = 2, yend = 2)) +
  theme_bw()+
  xlab("节后第一周净流入人口")+
  ylab("节后第二周净流入人口")+
  #labs(title = "2019 vs 2020")+
  theme(text=element_text(family="STKaiti",size=18))
gmigwk12b 

gmigwk23 <- wkmig%>% 
  #filter(bfy < -3) %>% 
  mutate(clabel=if_else((net_2 < 1 & net_3<1),"",as.character(city))) %>% 
  ggplot(aes(x=net_2,y= net_3))+
  geom_point()+
  geom_text_repel(aes(label=clabel),family="STSong",color="grey30" ,size=5,
                  box.padding = unit(0.25, "lines"))+
  geom_segment(aes(x = -2.5, y = -2.5, xend = 12, yend = 12)) +
  theme_bw()+
  xlab("上周净流入")+
  ylab("本周净流入")+
  #labs(title = "2019 vs 2020")+
  theme(text=element_text(family="STKaiti",size=18))
gmigwk23
# 取值比较小的点
gmigwk23b <- wkmig%>% 
  filter(net_2 < 1 & net_3<1) %>% 
 # mutate(clabel=if_else((net_1 < 1 & net_2<1),"",as.character(city))) %>% 
  ggplot(aes(x=net_2,y= net_3))+
  geom_point()+
  geom_text(aes(label=city,y=net_3+0.1),family="STSong",color="grey30" ,size=5)+
  geom_segment(aes(x = -2.5, y = -2.5, xend = 2, yend = 2)) +
  theme_bw()+
  xlab("上周净流入")+
  ylab("本周净流入")+
  #labs(title = "2019 vs 2020")+
  theme(text=element_text(family="STKaiti",size=18))
gmigwk23b 


##################################
#12. 迁移数据合并
##################################

bk1920 <- read.csv("/Users/liding/E/Bdata/rtemp/bk1920-321.csv")


# 探索指标相关性
indexco2 <- bk1920 %>% 
  #filter(plc=="outplc") %>% 
  scatmat( columns = c(16:19), color = NULL, alpha = 1)+theme_bw()

# 因为有些指标取值超出2
bk1920 %>% 
  filter(bkrc19anet>-2&bkrc19anet<2,
         bkrc19ain>-2&bkrc19ain<2,
         otrc19aout>-2&otrc19aout<2,
         bkrc20bnet>-2&bkrc20bnet<2,
         bkrc20bout>-2&bkrc20bout<2,
         plc=="inplc") %>% 
scatmat( columns = c(16:20), color = NULL, alpha = 1)+theme_bw()


##################################
## 看市内出行轻度和流入流入指标的相关性？
citymap %>% scatmat( columns = c(27:28,31:32,47:50), color = NULL, alpha = 1)+theme_bw()
corelindex <- cor((as.data.frame(citymap2)[c(27:28,31:32,47:50)]),use = "pairwise")
# 这里需要修改，对应两套指标
write.csv(corelindex,"/Users/liding/E/Bdata/rtemp/corindex.csv")

# 最后一周人口流入指数,绝对规模
citymap %>% 
 # filter(net_bfy_2020>8.84) %>% 
  ggplot() +
  geom_sf(aes(fill=(inval_2 + inval_3 + inval_4+inval_5+ inval_6+inval_7+ inval_8)), size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "Move in last wk") 


##################################
#13. 返城率，外出率
##################################
##################################
#人口流出地  外出进度
library(ggplot2)
library(worldtilegrid)
library(hrbrthemes)
summary(citymap$bkrc20bnet)
p1 <-   ggplot() +
  geom_sf(data=(filter(citymap,net_bfy_2020>=3)),
          aes(geometry = geometry,
              fill =if_else(bkrc20bnet>0.8,0.8,if_else(bkrc20bnet<0,0,bkrc20bnet)) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "STKaiti") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "人口流出地流出进度") +
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(-2600000, 2400000), ylim = c(2000000, 6000000))

p2 <- ggplot() + 
  geom_sf(data=(filter(citymap,net_bfy_2020>=3)),
          aes(geometry = geometry,
              fill =if_else(bkrc20bnet>0.8,0.8,if_else(bkrc20bnet<0,0,bkrc20bnet)) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_modern_rc(base_family = enfont) +
  worldtilegrid::theme_enhance_wtg()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        plot.background = element_blank()) + 
  coord_sf(xlim = c(-80000, 1500000), ylim = c(200000, 2500000))
library(cowplot)
gmvout <- ggdraw() + 
  draw_plot(p1) + 
  draw_plot(p2, x = 0.78, y = -0.29, width = 0.25)
gmvout 

ggexport(gmvout,filename="/Users/liding/E/Bdata/rtemp/gmvout-recover-2020022.png",width=2218,height=2208,res=250)

##################################
bk1920 %>% filter(net_bfy_2020< -4,
                   prov==32 | prov==33)%>% # select(cityname,bkrc20bnet) %>% DT::datatable()
ggplot() +
  geom_sf(aes(geometry = geometry,
              fill =if_else(bkrc20bnet>0.8,0.8,if_else(bkrc20bnet<0,0,bkrc20bnet)) ),
          color = "white", size = 0.1) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "STKaiti") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "主要人口流入地返城进度") +
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank())

# 主要人口流入市返城进度
inp1 <-   ggplot() +
  geom_sf(data=(filter(citymap,net_bfy_2020< -4)),
          aes(geometry = geometry,
              fill =if_else(bkrc20bnet>0.8,0.8,if_else(bkrc20bnet<0,0,bkrc20bnet)) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = "STKaiti") + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "主要人口流入地返城进度") +
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(-2600000, 2400000), ylim = c(2000000, 6000000))

inp2 <- ggplot() + 
  geom_sf(data=(filter(citymap,net_bfy_2020< -4)),
          aes(geometry = geometry,
              fill =if_else(bkrc20bnet>0.8,0.8,if_else(bkrc20bnet<0,0,bkrc20bnet)) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_modern_rc(base_family = enfont) +
  worldtilegrid::theme_enhance_wtg()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        plot.background = element_blank()) + 
  coord_sf(xlim = c(-80000, 1500000), ylim = c(200000, 2500000))

gmvin <- ggdraw() + 
  draw_plot(inp1) + 
  draw_plot(inp2, x = 0.78, y = -0.29, width = 0.25)
gmvin


##################################
gbkrate <-citymap2 %>% 
  #filter(plc=="inplc") %>% 
  ggplot() +
  geom_sf(aes(fill=bkrc19ain), size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "move in \n to 2019ayr in ") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

gotrate <- citymap2 %>% 
  #filter(plc=="outplc") %>% 
  ggplot() +
  geom_sf(aes(fill=otrc19aout), size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "move out \n to 2019ayr out") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")


gotrate20b <- citymap2 %>% 
  #filter(plc=="outplc") %>% 
  ggplot() +
  geom_sf(aes(fill=otrc20bin), size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "move out \n to 2020byr in") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

gbkrate20b <- citymap2 %>% 
  #filter(plc=="outplc") %>% 
  ggplot() +
  geom_sf(aes(fill=bkrc20bout), size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "move in \n to 2020byr out") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")


g3<- ggarrange(gbkrate ,gotrate ,gbkrate20b ,gotrate20b  ,nrow = 2, ncol = 2,
               common.legend=TRUE,
               legend="right")
ggexport(g3,filename="/Users/liding/E/Bdata/rtemp/g3-recover-20200214.png",width=2218,height=2208,res=250)


##################################
# 前100个流出地人口流出情况
gotrate20b_2 <- citymap2 %>% 
  filter(net_bfy_2020>8.84) %>% 
  ggplot() +
  geom_sf(aes(fill=bkrc20bnet), size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "Net move out \n to 2020byr net in") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")


gbkrate20b_2 <- citymap2 %>% 
  filter(plc=="inplc") %>% 
  ggplot() +
  geom_sf(aes(fill=bkrc20bout), size = 0.01) +
  geom_sf(data = cn_boundary,fill=NA,color="black", size = 0.1) +
  scale_fill_viridis_c(begin = 0, end = 1,name="") +
  theme_bw()+
  theme(plot.title = element_text(vjust = -10,hjust=0.5))+
  labs(title = "move in \n to 2020byr out") +
  coord_sf(crs = "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

g4<- ggarrange(gbkrate20b_2 ,gotrate20b_2 ,nrow = 1, ncol = 2,
               common.legend=TRUE,
               legend="right")
ggexport(g4,filename="/Users/liding/E/Bdata/rtemp/g4-recover-20200214.png",width=2218,height=1104,res=250)

## 带南海版
## 人口流入地最近两周流入情况
wp1 <-   ggplot() +
  geom_sf(data=(filter(citymap2,net_bfy_2020< -4)),aes(geometry = geometry,
                                                       fill =if_else((net_3/net_2) <0,0,if_else((net_3/net_2) > 2.5,2.5,(net_3/net_2)))),color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = enfont) + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(75, 133), ylim = c(18, 53.5),
           crs = "+proj=longlat", expand = TRUE)

#使用下面的方式可以将九段线部分截取出来：
wp2 <- ggplot() +
  geom_sf(data=(filter(citymap2,net_bfy_2020 < -4 )),aes(geometry = geometry,
                                                         fill =if_else((net_3/net_2)<0,0,if_else((net_3/net_2)>2.5,2.5,(net_3/net_2)))),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_modern_rc(base_family = enfont) +
  worldtilegrid::theme_enhance_wtg()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        plot.background = element_blank()) + 
  coord_sf(xlim = c(107, 122), ylim = c(4, 24),
           crs = "+proj=longlat")

ggdraw() + 
  draw_plot(wp1) + 
  draw_plot(wp2, x = 0.78, y = -0.29, width = 0.25)

##################################
## 人口流出地 最近两周流出情况
wop1 <-   ggplot() +
  geom_sf(data=(filter(citymap2,net_bfy_2020>=3)),aes(geometry = geometry,
                                                       fill =if_else((net_3/net_2) <0,0,if_else((net_3/net_2) > 2.5,2.5,(net_3/net_2)))),color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = enfont) + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(75, 133), ylim = c(18, 53.5),
           crs = "+proj=longlat", expand = TRUE)

#使用下面的方式可以将九段线部分截取出来：
wop2 <- ggplot() +
  geom_sf(data=(filter(citymap2,net_bfy_2020 >=3 )),aes(geometry = geometry,
                                                         fill =if_else((net_3/net_2)<0,0,if_else((net_3/net_2)>2.5,2.5,(net_3/net_2)))),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_modern_rc(base_family = enfont) +
  worldtilegrid::theme_enhance_wtg()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        plot.background = element_blank()) + 
  coord_sf(xlim = c(107, 122), ylim = c(4, 24),
           crs = "+proj=longlat")

ggdraw() + 
  draw_plot(wop1) + 
  draw_plot(wop2, x = 0.78, y = -0.29, width = 0.25)


##################################
## 第三周出行强度相当于2019年
w3p1 <- ggplot() + 
  geom_sf(data = (filter(citymap2,net_bfy_2020< -4)), aes(geometry = geometry,
                              fill =if_else(w3rc19> 0.6,0.6,w3rc19) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_bw(base_family = enfont) + 
  worldtilegrid::theme_enhance_wtg() + 
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank()) + 
  coord_sf(xlim = c(75, 133), ylim = c(18, 53.5),
           crs = "+proj=longlat", expand = TRUE)

#使用下面的方式可以将九段线部分截取出来：
w3p2 <- ggplot() + 
  geom_sf(data = (filter(citymap2,net_bfy_2020< -4)), aes(geometry = geometry,
                              fill = if_else(w3rc19> 0.6,0.6,w3rc19) ),
          color = "white", size = 0.1) + 
  geom_sf(data = cn_boundary, aes(geometry = geometry),
          color = "black",fill=NA, size = 0.2) + 
  scale_fill_viridis_c() + 
  theme_modern_rc(base_family = enfont) +
  worldtilegrid::theme_enhance_wtg()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA),
        plot.background = element_blank()) + 
  coord_sf(xlim = c(107, 122), ylim = c(4, 24),
           crs = "+proj=longlat")

#合并两个图
library(cowplot)
ggdraw() + 
  draw_plot(w3p1) + 
  draw_plot(w3p2, x = 0.78, y = -0.29, width = 0.25)



##################################
#14. 疫情数据,来自新浪，最新的汇总数据
##################################
library(jsonlite)
library(tidyverse)
jsondata <- fromJSON('https://interface.sina.cn/news/wap/fymap2020_data.d.json')
#https://gwpre.sina.cn/interface/fymap2020_data.json
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
# Johns Hopkins CSSE 数据
# 当前时间
(times <- jsondata$data$times)
#> [1] "截至2月7日12时01分"

# 确诊数量
(confirm <- jsondata$data$gntotal)
#> [1] "31211"

# 死亡数量
(dead <- jsondata$data$deathtotal)
#> [1] "637"

# 疑似数量
(suspect <- jsondata$data$sustotal)
#> [1] "26359"

# 治愈数量
(cure <- jsondata$data$curetotal)
#> [1] "1542"

# 省份分布
prov_distribution <- jsondata$data$list %>%
  as_tibble()%>%
  type_convert()
prov_distribution

# 城市分布
city_distribution <- jsondata$data$list %>%
  as_tibble()%>%
  select(city) %>%
  unnest(city) %>%
  type_convert()
city_distribution

# 在国外的分布
othercountry <- jsondata$data$otherlist %>%
  as_tibble()

othercountry %>% DT::datatable()
# 省级地图
hchinamap::hchinamap(
  name = prov_distribution$name,
  value = log(prov_distribution$value),
     itermName = "确诊人数", 
     title = "新型冠状病毒肺炎确诊人数的分布", 
     subtitle = "nCov Project", 
    # minColor="white",
    # maxColor ="green",
     theme = "sunset")

# 全球疫情数据,补齐中国的数据
othercountry <- jsondata$data$otherlist %>%
  as_tibble()
othercountry <- add_row(othercountry,
                        conNum =jsondata$data$gntotal,
                        value = jsondata$data$gntotal,
                        name= "中国",
                        deathNum = jsondata$data$deathtotal,
                        susNum = jsondata$data$sustotal,
                        cureNum = jsondata$data$curetotal
)
# 俄罗斯名称修改
othercountry$name[othercountry$name=="俄罗斯"] <- "俄罗斯联邦"


summary(othercountry$value)

othercountry %>% 
mutate(value=if_else(is.na(value),0,as.numeric(value)) ) %>% 
filter(name!="中国"&value>50) %>% 
  select(name,value) %>% 
  arrange(desc(value)) %>% # head()
  ggplot(aes(x=name, y=value, fill=value)) +
  geom_bar(stat='identity') +theme_light() +
  scale_fill_gradient(low='red', high='white', limits=c(5,10000)) +
  coord_polar() + 
  aes(x=reorder(name,value)) +
  theme(axis.text.x = element_text(angle=-20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        text=element_text(family="STKaiti",size=16)) 


othercountry %>% 
  mutate(value=if_else(is.na(value),0,as.numeric(value)) ) %>% 
  filter(name!="中国"&value>5) %>% 
  ggplot(aes(x=name, y=value^0.5, fill=value,width=value^0.5)) +
  geom_bar(stat='identity') +theme_light() +
  scale_fill_gradient(low='red', high='white', limits=c(5,10000)) +
  coord_polar() + aes(x=reorder(name,value)) +
  theme(axis.text.x = element_text(angle=-20),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        text=element_text(family="STKaiti",size=16)) 


browsers<-structure(list(browser = structure(c(3L, 3L, 3L, 3L, 2L, 2L, 
                                               2L, 1L, 5L, 5L, 4L), .Label = c("Chrome", "Firefox", "MSIE", "Opera", "Safari"), class = "factor"), 
                         version = structure(c(5L, 6L, 7L, 8L, 2L, 3L, 4L, 1L, 10L, 11L, 9L), .Label = c("Chrome 10.0", "Firefox 3.5", "Firefox 3.6", "Firefox 4.0", "MSIE 6.0", "MSIE 7.0", "MSIE 8.0", "MSIE 9.0", "Opera 11.x", "Safari 4.0", "Safari 5.0"), class = "factor"), share = c(10.85, 7.35, 33.06, 2.81, 1.58, 13.12, 5.43, 9.91, 1.42, 4.55, 1.65), ymax = c(10.85, 18.2, 51.26, 54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 90.08, 91.73), ymin = c(0, 10.85, 18.2, 51.26, 54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 90.08)), .Names = c("browser", "version", "share", "ymax", "ymin"), row.names = c(NA, -11L), class = "data.frame")


ggplot(browsers) + 
  geom_rect(aes(fill=version, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #geom_rect(aes(fill=browser, ymax=ymax, ymin=ymin, xmax=3, xmin=0)) +
  xlim(c(0, 4)) + 
  theme(aspect.ratio=1) +
  coord_polar(theta="y")  



mydata <- data.frame(side1=rep(LETTERS[1:3],3,each=9),side2=rep(LETTERS[1:3],9,each=3),widget=rep(c("X","Y","Z"),9*3),val=runif(9*3),strength=rep(c(1,2,3),3,each=3))

ggplot(mydata, aes(x=strength/2, y = val, fill = widget, width = strength)) +
  geom_bar(position="fill", stat="identity") + 
  facet_grid(side1 ~ side2) + 
  coord_polar("y") + 
  opts(axis.text.x = theme_blank()) 


mydata<-data.frame(
  id=1:13,
  class=rep_len(1:4, length=13),
  Label=c("Events","Lead List","Partner","Markeiting & Advertising","Tradeshows","Paid Search","Webinar","Emial Campaign","Sales generated","Website","Other","Facebook/Twitter/\nOther Social","Employee & Customer\nReferrals"),
  Value=c(7.6,15.5,17.9,21.8,29.6,29.7,32.7,43.0,57.5,61.4,67.4,68.6,68.7)
)

ggplot(mydata)+
  geom_col(aes(x=id,y=Value/2+150,fill=factor(class)),colour=NA,width=1)+
 # geom_col(aes(x=id,y=150-Value/2),fill="white",colour="white",width=1)+
  scale_x_continuous(limits=c(0,26),expand=c(0,0))+
  coord_polar(theta = "x",start=-14.275, direction = 1)


# 面积图
df <- data.frame(angle = rep(seq(0, 360, 45), 4), frequency = rep(1:9, 4), z = as.character(c(rep(1, 9), rep(2, 9))))
df <- aggregate(frequency ~ angle + z, data = df, sum)

pl <- ggplot(data=df,aes(x=angle,y=frequency,fill=z)) +
  geom_area() + 
  coord_polar()+
  scale_x_continuous(breaks=seq(0, 360, 45), labels=c("N", "NE", "E", "SE", "S", "SO", "O", "NO", "N"))
pl


library(plotly)
k <- 100
df <- data.frame(angle = rep(seq(0, 360, length.out=k), 4), 
                 frequency = rep(1:k, 4), 
                 z = as.character(c(rep(1, k), rep(2, k))))
df <- aggregate(frequency ~ angle + z, data = df, sum)   
df$frequency[df$z==2] <- df$frequency[df$z==2]/2

p <- plot_ly(data=df, type = 'scatterpolar', mode = "lines") %>%
  add_trace(theta=~angle,r=~frequency,fill="toself", color=~z)

p <- p %>% layout(
  polar = list(radialaxis = list(angle = 90),
               angularaxis = list(
                 rotation = 90,
                 direction = 'clockwise',
                 ticktext =  c("N", "NE", "E", "SE", "S", "SO", "O", "NO", "N"),
                 tickvals = seq(0, 360, 45)
               )
  )
)
p


#中文地区标准码
#wdf <- st_read("/Users/liding/E/Bdata/2019R/l12spa/intro/world.shp")
#http://www.mohrss.gov.cn/SYrlzyhshbzb/zhuanti/jinbaogongcheng/Jbgcbiaozhunguifan/201112/t20111206_47429.html
library(readxl)
url <- "http://www.mohrss.gov.cn/UserFiles/File/3a0bc4a1-ba57-4b4d-b9b4-a7c7d788231d.xls"
destfile <- "contrycode.xls"
curl::curl_download(url, destfile)
cname  <- read_excel(destfile, skip = 8)
names(cname) <- c("id","cname","NAME","NAME_LONG","A2","A3","numid","note")
cname <- cname  %>% filter(!is.na(id)) %>% select(-note)

# 国际地理标准码、世界各国人口数据、疫情数据三个数据合并
#cname <- read.csv("/Users/liding/E/Bdata/rtemp/worldcountry.csv") # 手工准备的中文名对应表（不太全）
library("rvest") 
URL<-"https://www.geonames.org/countries/" 
doc <- URL %>%  read_html(encoding ="UTF-8")  
doc%>% html_table(header=TRUE) 
worldpop <- doc%>% html_table(header=TRUE) %>% `[[`(2)  %>% 
  mutate(Population=as.numeric(gsub(",", "",Population))) %>% 
  left_join(cname,by=c("ISO-3166alpha3"="A3")) %>% 
  mutate(cname=as.character(cname),
         cname=if_else(is.na(cname),Country,cname)) %>% 
  left_join(othercountry,by=c("cname"="name"))

library(sf)
# 数据比较大
wdf <- st_read('/Users/liding/E/Bdata/RSource/map/world.json')
# 检查数据匹配情况的一些额外操作，可以不用了
# dwdf <- wdf %>% as.data.frame() %>% 
#         select(1:65) %>% 
#         full_join(cname,by=c("NAME_LONG"="name"))
# 
# write.csv(dwdf,file="/Users/liding/E/Bdata/rtemp/wdcountryename.csv",row.names = FALSE)
#write.csv(othercountry,file="/Users/liding/E/Bdata/rtemp/othcountry.csv",row.names = FALSE)

# 地图与数据合并
wdf  <- wdf %>% 
  left_join(worldpop,by=c("ADM0_A3"="ISO-3166alpha3")) %>% 
  mutate(value=if_else(is.na(value),0,as.numeric(value)) )

ggplot() + 
  geom_sf(data = wdf, aes(geometry = geometry,
                          fill = log(value)),
          size = 0.05,color = "white") + 
  coord_sf(crs = "+proj=longlat") + 
  scale_fill_viridis_c() +
  labs(title ="全球疫情log(n)",
       caption = "数据来源：新浪 2020-03-04 \n<https://interface.sina.cn/news/wap/fymap2020_data.d.json>") + 
  worldtilegrid::theme_enhance_wtg() + 
  # hrbrthemes::theme_ipsum(base_family = enfont)+
  theme(legend.position =c(0.1,0.4),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))


#世界地图
# "/Users/liding/E/Bdata/RSource/map/world.json"
# library(geojsonsf)
# worldsf <- readLines("https://finance.sina.com.cn/other/src/world_geo.json") %>% 
#   paste0(collapse = "") %>% 
#   geojsonsf::geojson_sf()


#来自丁香园的疫情数据
#covid <- jsonlite::fromJSON('/Users/liding/Downloads/worldcovid19.json')
URL<-"http://ncov.dxy.cn/ncovh5/view/pneumonia" 
doc <- URL %>%  read_html(encoding ="UTF-8") 
xml_children(doc)[[2]]
covid <- doc%>% xml_find_first("//script[@id='getListByCountryTypeService2']") %>% xml_text()
covid <- substr(covid,45,(nchar(covid)-11))
covid <- jsonlite::fromJSON(covid )
#来自丁香园的地图数据
#wdf0 <- st_read('/Users/liding/Downloads/world.json')
wdf0 <- st_read('https://file1.dxycdn.com/2020/0306/524/3400453673477029002-62.json')
# 有两个国家的id为-99，Northern Cyprus Somaliland 
wdf1 <- wdf0 %>% left_join(covid,by=c('id'='countryShortCode'))


# 中国数据
covidchina <-doc%>% xml_find_first("//script[@id='getListByCountryTypeService1']") %>% xml_text()
covidchina <- substr(covidchina,45,(nchar(covidchina)-11))
covidchina <- jsonlite::fromJSON(covidchina )
chinamap <- st_read("https://file1.dxycdn.com/2020/0131/090/3394052471398860228-62.json" )
chinamap <- chinamap %>% left_join(covidchina,by=c("name"="provinceShortName"))


# 丁香园的数据是嵌入在js中
ggplot() + 
  geom_sf(data = chinamap, aes(geometry = geometry,
                           fill = log(confirmedCount)),
          size = 0.05,color = "white") + 
  coord_sf(crs = "+proj=longlat") + 
  scale_fill_viridis_c() +
  labs(title ="中国疫情log(n)",
       caption = "数据来源：丁香园 2020-03-04 \n<http://ncov.dxy.cn/ncovh5/>") + 
  worldtilegrid::theme_enhance_wtg() + 
  # hrbrthemes::theme_ipsum(base_family = enfont)+
  theme(legend.position =c(0.1,0.2),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))

## 世界疫情，需要补齐中国的数据
ggplot() + 
  geom_sf(data = wdf1, aes(geometry = geometry,
                          fill = log(confirmedCount)),
          size = 0.05,color = "white") + 
  coord_sf(crs = "+proj=longlat") + 
  scale_fill_viridis_c() +
  labs(title ="全球疫情log(n)",
       caption = "数据来源：丁香园 2020-03-04 \n<http://ncov.dxy.cn/ncovh5/>") + 
  worldtilegrid::theme_enhance_wtg() + 
  # hrbrthemes::theme_ipsum(base_family = enfont)+
  theme(legend.position =c(0.1,0.4),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5),
        text=element_text(family="STKaiti",size=16))



##手工处理地理编码
#url <- "http://qianxi.baidu.com/"
#html <- read_file("/Users/liding/E/Bdata/rtemp/baiducity.txt")
#doc <- read_html(html)
#city <- xml_text(xml_find_all(doc,"//a[@class='sel_city_name']"))
#write.csv(city,file="/Users/liding/E/Bdata/rtemp/baiducity.csv")

# 尝试部分
# url <- "http://huiyan.baidu.com/migration/historycurve.jsonp?dt=province&id=340100&type=move_in&type=move_in"
# response <- getURL(url)
# response <- substr(response,4,(nchar(response)-1))
# dat <- fromJSON(response, simplify=TRUE)
# dat <- dat$data
# data3401 <- t(as.data.frame(dat))
# 
# whouturl <- "http://huiyan.baidu.com/migration/provincerank.jsonp?dt=city&id=420100&type=move_out&date=2"
# response <- getURL(whouturl)
# response <- substr(response,4,(nchar(response)-1))
# dat <- jsonlite::fromJSON(response)
# dat$data
# 
# ourl <- "http://huiyan.baidu.com/migration/cityrank.jsonp?dt=province&id=110000&type=move_in&date=1"
# response <- getURL(ourl)
# response <- substr(response,4,(nchar(response)-1))
# dat <- jsonlite::fromJSON(response)
# data1 <- dat$data
# write.csv(data1,file="/Users/liding/E/Bdata/rtemp/fromcity.csv")

################################
#15. 获取疫情数据的一种方式
################################
library(xml2)
library(tidyverse)
doc <- read_html("/Users/liding/E/Bdata/rtemp/convirvusdata.html",encoding = "utf-8")
city <- xml2::xml_find_all(doc,"//div[@class='clearfix placeItem placeCity']") %>% xml_text()

all <-xml2::xml_find_all(doc,"//div[@class='clearfix placeItem placeCity']")%>% xml_text()

all <- stringi::stri_replace_all_fixed(all,"\n",",")
all <- as.data.frame(all)
write.csv(all,"/Users/liding/E/Bdata/rtemp/convirvusdata.csv",row.names=FALSE)

library(tidyverse)
all <- read.csv("/Users/liding/E/Bdata/rtemp/convirvusdata.csv",header=FALSE)

illall <- all %>% 
  mutate_all(~ replace(., .=="-", NA)) %>% 
  filter(V1!="地区待确认") %>% 
  select(V1,V3) %>% rename("city"="V1",anum="V3")

write.csv(illall,"/Users/liding/E/Bdata/rtemp/convirvusdata2.csv",row.names=FALSE)
# 手工处理得到csv数据
#将地图的编码输出
#write.csv(data.frame(citymap)[1:2],"/Users/liding/E/Bdata/rtemp/mapcitylist.csv",row.names=FALSE)

# 使用R包的方式获取可能更好
library(nCov2019)
help(package="nCov2019")
ncov2019 <- load_nCov2019()
x <- get_nCov2019()


##################################
#16.定义的函数
##################################
stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                                             list(a = format(coef(m)[1], digits = 3), 
                                                  b = format(coef(m)[2], digits = 3), 
                                                  r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq))
                            
                            if(is.null(xpos)) xpos = max(data$x)*0.7
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x=xpos, y=ypos, label=func_string)
                            
                          },
                          
                          required_aes = c("x", "y")
)

  