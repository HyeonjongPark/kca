sobig = data.frame(문장번호 = c(1,2,3,4,5,6,7,8,9,10,11:20,21,22),
                       사건번호 = c(1,1,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,7,7),
                       문장순서 = c(1,2,1,2,3,1,2,3,4,5,1,2,3,4,5,1,2,3,1,2,1,2),
                       문장내용 = c("abc","asdfwe","qwasd","asdda","zxceeeqq","asdasdaasd","ass","bdebfm",
                                "asdsee","qqwlb","ebvjnfknoj","pdobjipeniper","sdfsdf","iqwiwiww","asks","fjfji4",
                                "82t4g2hbei","9f23","vb" ,"isdjjd","f안녕","하세아어"),
                       각각불만유형 = c(2,5,4,4,6,1,5,1,1,1,5,7,2,1,1,1,9,5,7,1,1,1),
                       전체사건판정 = NA,
                       불만유형이름 = NA)
sobig$문장내용 = as.character(sobig$문장내용)
library(dplyr)
#nchar(sobig$문장내용)
#nstring = sobig %>% filter(사건번호 == sobig[12,]$사건번호) %>% group_by(각각불만유형) %>% summarise(nstring = sum(nchar(문장내용))) %>% arrange(desc(nstring))
#as.integer(nstring[nstring$각각불만유형 != 1,][1,1])
for(i in 1:22) {
  ncount = sobig %>%
    filter(사건번호 == sobig[i,]$사건번호) %>%
    group_by(각각불만유형) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  nstring = sobig %>%
    filter(사건번호 == sobig[i,]$사건번호) %>%
    group_by(각각불만유형) %>%
    summarise(nstring = sum(nchar(문장내용))) %>%
    arrange(desc(nstring))
  est = ifelse(as.integer(ncount[1,1]) == 1 ,
               if(dim(ncount)[1] == 1){ ## 각각의 불만유형이 모두 1인 경우
                 as.integer(ncount[1,1])
               }
               else if(dim(ncount)[1] == 2){
                 as.integer(ncount[2,1])
               }
               else{
                 if(as.integer(ncount[2,2]) == as.integer(ncount[3,2])) {
                   as.integer(nstring[nstring$각각불만유형 != 1,][1,1])
                 }
                 else as.integer(ncount[2,1])
               } ,
               if(dim(ncount)[1] == 1) {
                 as.integer(ncount[1,1])
               }
               else{
                 if(as.integer(ncount[1,2]) == as.integer(ncount[2,2])) {
                   as.integer(nstring[nstring$각각불만유형 != 1,][1,1])
                 }
                 else as.integer(ncount[1,1])
               })
  sobig$전체사건판정[i] = est
}