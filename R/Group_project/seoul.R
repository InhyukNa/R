library(stringr)
library(ggplot2)
library(dplyr)
library(readxl)

cctv2$'자치구'=str_replace_all(cctv2$'자치구',' ',"")
#cctv 자치구는 '강 동 구' 글자마다 띄어쓰기가 되어있고 report 자치구는 강동구 글자가 붙어있어 공통되게만들어줌.
#공통되게 만들어주지않을시 데이터가 불러와지지않음.
seoul <- left_join(cctv2,report5,by="자치구")
#left join 명령어를사용해 cctv2와 report5데이터를 붙이고 자치구를 기준으로했다.
head(seoul)
# 찍어보니 결과가나옴

attach(seoul)
#데이터를 R 검색 경로에 추가하여 변수명으로 바로 접근할 수 있게 한다.
head(검거율)  
#attach가 작동하는지확인
plot(합계~검거율,main="서울시 cctv와 검거율의 관계", xlab="14~18년도 검거율", ylab="14~18년도 구별cctv 합계", cex =1, pch=1, col="red")
#합계~검거율 x합계 y 검거율 로 산점도 표시
cor.test(seoul$합계,seoul$검거율)
#검정을통해 pvalue값이 0.2392로 0.05보다 큼.
# cctv 많다고 검거 잘되는거 아니다. 
plot(seoul)
#전체의 내용을 보여준다 