library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
cctv <- read.csv('cctv.csv',fileEncoding = "UCS-2LE")
#cctv csv 불러와 읽음 .fileencoding으로 ucs-2le를써야 한글파일이열림
head(cctv)
#잘불러와졋는지 확인
cctv1<-cctv %>% select(기관명,X2014년,X2015년,X2016년,X2017년,X2018년)
#필요로하는 열만 사용하기위해 select 명령어사용하여 cctv1이라는 새로운변수에담음
head(cctv1)
#cctv1이라는 변수에 필요한열만 들어와져있느지 확인
cctv1<-rename(cctv1,'자치구'=기관명,'2014년'=X2014년,'2015년'=X2015년,'2016년'=X2016년,'2017년'=X2017년,'2018년'=X2018년)
#cctv1 에 새로운 컬럼명을 부여 
head(cctv1)
#cctv1에 새로운컬럼명이 부여되었는지확인
is.na(cctv1)
#null 값 있는지 확인
table(is.na(cctv1))
#null값과 null값이 아닌값을 확인해줌 false true로 나옴.
cctv2 <- na.omit(cctv1)
#cctv2라는 변수에 null값을 제거해주는 na.omit이라는 명령어를사용하여 null값이 제거된데이터를 담음
str(cctv2)
#cctv2에 str명령어를 사용하여 타입 확인 2014년이 문자열로 되어있는것을 확인 나머지는 num 타입(float).
cctv2$'2014년' = str_replace(cctv2$'2014년',',','')
#2014에 , 를 제거해 주고 값을 붙여줌 
cctv2$'2014년' <- as.numeric(cctv2$'2014년')
#2014 년 데이터를 num타입으로 변환
is.numeric(cctv2$'2014년')
#2014년 데이터가 num 타입으로 변환되었는지 확인
str(cctv2)
#str명령어를 사용하여 타입 확인 num 으로 변환됨
View(cctv2)
#cctv2데이터를 view명령어를사용하여 확인 
cctv2$'합계'<-(cctv2$'2014년'+cctv2$'2015년'+cctv2$'2016년'+cctv2$'2017년'+cctv2$'2018년')
#cctv2에 합계라는 새로운 컬럼명을 만들어 2014~2018년까지의 cctv데이터를 합침.
head(cctv2)
#ccctv2 합계를추가하고 추가되었는지 확인
View(cctv2)
ggplot(data=cctv2, aes(x = 합계, y = 자치구,fill=자치구))+ggtitle('서울시 구별 cctv')+geom_bar(stat="identity")
#막대그래프를 이용하여 구별 2014~2018년 cctv 합계 를 표현 
dim(cctv2)
