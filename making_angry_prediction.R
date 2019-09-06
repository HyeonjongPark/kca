sobig = data.frame(문장번호 = c(1,2,3,4,5,6,7,8,9,10,11:20,21,22),
                       사건번호 = c(1,1,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,7,7),
                       각각불만유형 = c(2,5,4,4,6,1,5,1,1,1,5,7,2,1,1,1,9,5,7,1,1,1),
                       전체사건판정 = NA,
                       불만유형이름 = NA)
sobig
library(dplyr)
for(i in 1:22) {
  a = sobig %>% filter(사건번호 == sobig[i,]$사건번호) %>% group_by(각각불만유형) %>% summarise(count = n())
  b = ifelse(as.integer(a[1,1]) == 1 ,
             if(dim(a)[1] == 1){ ## 각각의 불만유형이 모두 1인 경우
               as.integer(a[1,1])
             }
             else if(dim(a)[1] == 2){
               as.integer(a[2,1])
             }
             else{
               if(as.integer(a[2,2]) == as.integer(a[3,2])) NA
               else as.integer(a[1,1])
             } ,
             if(dim(a)[1] == 1) {
               as.integer(a[1,1])
             }
             else{
               if(as.integer(a[1,2]) == as.integer(a[2,2])) NA
               else as.integer(a[1,1])
             })
  sobig$전체사건판정[i] = b
}
sobig
for(i in 1:22) {
  if(sobig$각각불만유형[i] == 1 ) {
    sobig$불만유형이름[i] = "a"
  }
  else if(sobig$각각불만유형[i] == 2 ) {
    sobig$불만유형이름[i] = "b"
  }
  else if(sobig$각각불만유형[i] == 3 ) {
    sobig$불만유형이름[i] = "c"
  }
  else if(sobig$각각불만유형[i] == 4 ) {
    sobig$불만유형이름[i] = "d"
  }
  else if(sobig$각각불만유형[i] == 5 ) {
    sobig$불만유형이름[i] = "e"
  }
  else if(sobig$각각불만유형[i] == 6 ) {
    sobig$불만유형이름[i] = "f"
  }
  else if(sobig$각각불만유형[i] == 7 ) {
    sobig$불만유형이름[i] = "g"
  }
  else if(sobig$각각불만유형[i] == 8 ) {
    sobig$불만유형이름[i] = "h"
  }
  else if(sobig$각각불만유형[i] == 9 ) {
    sobig$불만유형이름[i] = "i"
  }
  else if(sobig$각각불만유형[i] == 10 ) {
    sobig$불만유형이름[i] = "j"
  }
}
sobig