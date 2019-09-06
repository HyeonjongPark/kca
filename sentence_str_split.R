## 콤마기준_문장내용 쪼개기
pre_split = data.frame(문장번호 = c(1,2,3,4,5,6,7,8,9,10,11:20,21,22),
                           사건번호 = c(1,1,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,7,7),
                           문장순서 = c(1,2,1,2,3,1,2,3,4,5,1,2,3,4,5,1,2,3,1,2,1,2),
                           문장내용 = c("ab.계약. 질문","asd환급.fw파손e.","이물qwa...이물sd.","as..dda","z.x.c.e.e.eqq","asdasd.aa오염sd","a오염ss.","bdebfm.",
                                    "asds.e문의e","qqwlb","eb.vjnfk문의noj","pdob.j환급ipeniper","s문의.df문의sd문의.f","iq환급wiwiww","asks","f오염.jfji4.",
                                    "82t4g.2hb문의ei.","9문의.f23","v문.의b" ,"문의isd.jj환급d","f.환급안녕","환급하문의함.세아어"),
                           불만유형번호 = NA,
                           전체사건판정번호 = NA)
pre_split
pre_split$문장내용 = as.character(pre_split$문장내용)
library(stringr)
library(dplyr)
dfList = data.frame()
for(j in 1:nrow(pre_split)){
  for(i in 1:length(strsplit(pre_split$문장내용,fixed=T,split=".")[[j]])){
    dfTemp = data.frame(문장번호 = 1 , 사건번호 = pre_split$사건번호[j] , 문장순서 =  1,
                            문장내용 = strsplit(pre_split$문장내용,fixed=T,split=".")[[j]][i] ,
                            불만유형번호 = NA , 전체사건판정번호 =NA)
    dfList = rbind(dfList , dfTemp)
  }
}
dfList
sen_total = data.frame()
for(i in 1:length(unique(pre_split$사건번호))) {
  sen_order = dfList %>% filter(사건번호 == unique(pre_split$사건번호)[i])  %>% select(문장순서)
  sen_order$문장순서 = 1:nrow(sen_order)
  sen_total = rbind(sen_total,sen_order)
}
sen_total
dfList$문장순서 = sen_total
dfList
dfList$문장번호 = 1:nrow(dfList)
dfList
