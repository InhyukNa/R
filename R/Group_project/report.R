library(stringr)
library(ggplot2)
library(dplyr)
library(readxl)
report <-read.csv('report.csv',fileEncoding = "UCS-2LE",sep='\t') #encoding ucs-2le사용해야 한글파일이 열림.
head(report)

report2<-report[-1,] #1번째 행 제거 

head(report2)
#1번째행 제거되었는지 확인.
report2<-rename(report2,'발생 합계'=합계,'검거 합계'=합계.1,'살인 발생'=살인, 
                '살인 검거'= 살인.1,'강도 발생'=강도,'강도 검거'=강도.1,'강간강제추행 발생'=강간강제추행,'강간강제추행 검거'=강간강제추행.1,'절도 발생'= 절도,'절도 검거'=절도.1,'폭력 발생'=폭력,'폭력 검거'=폭력.1)
#새로운 컬럼명을 줌.
head(report2)
#새로운 컬럼명이 지정되었는지 확인.
report3<- report2 %>% filter(기간!=2019)# 2019년도 제거 

tail(report3) #2019 제거 되었는지확인

report3$`발생 합계` = str_replace(report3$`발생 합계`,',','')    
report3$`검거 합계` = str_replace(report3$`검거 합계`,',','')
report3$`강간강제추행 발생`= str_replace(report3$`강간강제추행 발생`,',','')
report3$`강간강제추행 검거` = str_replace(report3$`강간강제추행 검거`,',','')
report3$`절도 발생` = str_replace(report3$`절도 발생`,',','')
report3$`절도 검거` = str_replace(report3$`절도 검거`,',','')
report3$`폭력 발생` = str_replace(report3$`폭력 발생`,',','')
report3$`폭력 검거` = str_replace(report3$`폭력 검거`,',','')
report3$`발생 합계`<-as.integer(report3$`발생 합계`)
report3$`검거 합계`<-as.integer(report3$`검거 합계`)
report3$`살인 발생`<-as.integer(report3$`살인 발생`)
report3$`살인 검거`<-as.integer(report3$`살인 검거`)
report3$`강도 발생`<-as.integer(report3$`강도 발생`)
report3$`강도 검거`<-as.integer(report3$`강도 검거`)
report3$`강간강제추행 발생`<-as.integer(report3$`강간강제추행 발생`)
report3$`강간강제추행 검거`<-as.integer(report3$`강간강제추행 검거`)
report3$`절도 발생`<-as.integer(report3$`절도 발생`)
report3$`절도 검거`<-as.integer(report3$`절도 검거`)
report3$`폭력 발생`<-as.integer(report3$`폭력 발생`)
report3$`폭력 검거`<-as.integer(report3$`폭력 검거`)
report3$기간<-as.integer(report3$기간)
#,제거해주고 int 타입으로 변환
str(report3)#변환되었는지확인
tail(report3) # tail 찍어서 자치구에 합계라는 열이 있음.
report3<-report3[!(report3$자치구 == "합계" ),] #합계 필요없어 삭제
View(report3) #삭제되었는지확인
report4<- report3 %>% arrange(report3$자치구) #이름별  오름차순 정렬
View(report4) #오름차순되었는지 확인
ggplot(report4) +ggtitle('서울시 기간별 범죄율 히트맵')+ geom_tile(aes(x = 기간, y = 자치구, fill = `발생 합계`),alpha = 0.6) + scale_fill_gradient2(low = 'white', high = 'red') + theme_classic() #서울시 연도별 범죄율 확인 히트맵.

report5<- report4 %>% group_by(자치구) %>% summarise(발생총합=sum(`발생 합계`),검거총합=sum(`검거 합계`))
#자치구별 발생합계 검거합계 총합구하기.
head(report5)
#값 확인
report5$'검거율' <- report5$검거총합/ report5$발생총합
#검거율이라는 새로운 컬럼 만들고 그값으로 검거총합/발생총합을 해줌

head(report5)
dim(report5)
View(report5)
#값이 들어갔는지확인



ggplot(data=report5, aes(x = 검거율, y =reorder(자치구),fill=자치구))+ggtitle('서울시 검거율')+geom_bar(stat="identity")
# 자치구별 검거율 확인하는 막대그래프 
ggplot(data=cctv2, aes(x = 합계, y = 구별,fill=구별))+ggtitle('서울시 구별 cctv')+geom_bar(stat="identity")
#구별 cctv 설치현황 확인 막대그래프 


