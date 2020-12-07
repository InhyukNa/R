library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)

# csv 파일 불러오기
family = read.csv('FAMILY.csv', fileEncoding = "UCS-2LE")
family
class(family)

# 2014년 이후 초산 연령 관련 데이터만 뽑아서 OECD14에 담음
OECD14 = filter(family, Indicator == "Mean age of women at childbirth", Year >= 2014)
OECD14

# OECD14에서 세가지 컬럼(Country, Year, Value만 남김)
OECD14 = select(OECD14, Country, Year, Value)
head(OECD14)

# null값 확인
is.na(OECD14)

# null값이 하나라도있으면 그 행을 제거하여 OECD에 담음
OECD = na.omit(OECD14)
OECD
str(OECD)

# ggplot2 패키지 부착
library(ggplot2)

# 국가별/년도별 초산 연령 히트맵
ggplot(OECD, aes(x=Year, y=Country, fill=Value)) + geom_tile() + scale_fill_gradient(low = "yellow", high = "red")

#OE = filter(OECD, Country == c('Sweden', #'Japan', 'Korea', 'Germany', 'United #States'))
#OE

# 스웨덴, 일본, 한국, 독일, 스페인의 평균 초산 연령을 뽑아내 따로따로 담아둠
sw = filter(family, Indicator == "Mean age of women at childbirth", Country == 'Sweden') %>% select(Country, Year, Value)
sw
jp = filter(family, Indicator == "Mean age of women at childbirth", Country == 'Japan') %>% select(Country, Year, Value)
jp
ko = filter(family, Indicator == "Mean age of women at childbirth", Country == 'Korea') %>% select(Country, Year, Value)
ge = filter(family, Indicator == "Mean age of women at childbirth", Country == 'Germany') %>% select(Country, Year, Value)
sp = filter(family, Indicator == "Mean age of women at childbirth", Country == 'Spain') %>% select(Country, Year, Value)

# 수집한 위의 데이터를 oecd라는 곳에 합침
oecd = rbind(sw,jp,ko,ge,sp)
oecd

# 각 나라별로 다른 색을 지정하여 선 그래프를 그림
ggplot(oecd, aes(x=Year, y=Value, group=Country, color=Country)) + geom_line(size=1.3, arrow=arrow()) + geom_point(aes(x=Year, y=Value, group=Country, color=Country), size=2) + ggtitle("OECD 국가 년도별 첫출산 평균연령")

# csv 파일 불러오기
birthage = read.csv('평균초산연령.csv', fileEncoding = "UCS-2LE")
birthage

# subset 함수를 활용하여 select=-로 필요없는 항목들을 버림 
birthage2 = subset(birthage, select = -c(Reference.Period.Code,Reference.Period,Flag.Codes,Flags,IND,PowerCode,YEAR,PowerCode.Code,COU,SEX,Sex,Unit.Code))
birthage2
head(birthage2)

# 초산 연령 관련 데이터 중 Country, Year, Value만 뽑음
birthage3 = filter(birthage2, Indicator == "Mean age of women at childbirth") %>% select(Country, Year, Value)
birthage3
tail(birthage3)

# 위 데이터에서 Country는 삭제
birthage4 = subset(birthage3, select = -c(Country))
birthage4

# 함수 group_by()를 이용해 Year별로 그룹화함
birthage5 = group_by(birthage4, Year)
birthage5

# Year별 Value의 평균을 구하여 birthage6에 담아둠
birthage6 = aggregate(Value~Year, birthage5, mean)
birthage6

# OECD 국가 평균 첫출산연령을 선그래프로 그림
ggplot(birthage6, aes(x=Year, y=Value)) + geom_line(size=1.3, arrow=arrow()) + geom_point(aes(x=Year, y=Value), size=2) + ggtitle("OECD 국가 평균 첫출산 평균연령")

# 한국 데이터만 따로 뽑아 ba_ko에 담음
ba_ko = filter(birthage3, Country == "Korea")
ba_ko

# 나라명을 뺌
ba_kor = subset(ba_ko, select = -c(Country))
birthage6
ba_kor

# OECD 평균과 한국 평균 데이터를 합침
ba = rbind(birthage6, ba_kor)
ba

# 새로운 Country 컬럼을 추가해 OECD와 KOR
ba$Country = c('OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD','OECD', 'KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR','KOR')
ba

# OECD와 한국 첫출산 연령 선그래프로 비교
ggplot(ba, aes(x=Year, y=Value, group=Country, color=Country)) + geom_line(size=1.3, arrow=arrow()) + geom_point(aes(x=Year, y=Value, group=Country, color=Country), size=2) + ggtitle("OECD와 한국 첫출산 연령 비교")


# 데이터 프레임 만들기
ko_p = data.frame('구분'=c('중학교 이하', '고등학교', '대학교 이상'), '도수' = c(12, 39, 49))
ko_p

# 상대 도수 컬럼 추가
ko_p$'상대도수' = round(ko_p$도수/100,3)
ko_p

# 한국 학력별 인구비율을 파이차트로 그림
ggplot(ko_p, aes(x='',y=상대도수,fill=구분)) + geom_bar(width=1, stat = 'identity', color='white') + coord_polar('y') + geom_text(aes(label=paste0(round(상대도수*100,1),"%")), position=position_stack(vjust = 0.5)) + theme_void() + ggtitle("한국 학력별 인구비율")

# 데이터 프레임 만들기(oecd 학력별 인구비율)
oecd_p = data.frame('구분'=c('중학교 이하', '고등학교', '대학교 이상'), '도수' = c(22, 44, 39))
oecd_p

# 상대 도수 컬럼 추가
oecd_p$'상대도수' = round(oecd_p$도수/105,3)
oecd_p

# OECD 학력별 인구비율을 파이차트로 그림
ggplot(oecd_p, aes(x='',y=상대도수,fill=구분)) + geom_bar(width=1, stat = 'identity', color='white') + coord_polar('y') + geom_text(aes(label=paste0(round(상대도수*100,1),"%")), position=position_stack(vjust = 0.5)) + theme_void() + ggtitle("OECD 학력별 인구비율")


# 학력 관련 csv 불러오기기
edu = read.csv('성인학력.csv', fileEncoding = "UCS-2LE")
edu

# 평균값에 해당하는 OAVG만 중에서 LOCATION, TIME, Value 값만 뽑음
oecd_edu = filter(edu, LOCATION == "OAVG") %>% select(LOCATION, TIME, Value)
oecd_edu

# 한국 데이터만 뽑음
ko_edu = filter(edu, LOCATION == "KOR") %>% select(LOCATION, TIME, Value)
ko_edu

# 두 데이터를 합침
EDU = rbind(oecd_edu, ko_edu)
colnames(EDU) = c('Country', 'Year', 'Value')
EDU

# 라이브러리 설치하고 불러옴
install.packages("car")
library(car)

# 문자열 OAVG를 OECD로 바꿈
EDU$Country = recode(EDU$Country, "'OAVG'='OECD'")
EDU

# 여성의 대학교 이상 교육 진학률을 알아 볼 수 있는 선그래프를 그려 한국과 OECD 평균과 비교함
ggplot(EDU, aes(x=Year, y=Value, group=Country, color=Country)) + geom_line(size=1.3, arrow=arrow()) + geom_point(aes(x=Year, y=Value, group=Country, color=Country), size=2) + ggtitle("여성의 대학교 이상 교육 진학률")


library(dplyr)

# 겹치지 않는 데이터 삭제
EDU2 = filter(EDU, Year <= 2017, Year >= 1998)
EDU2
ba_2 = filter(ba, Year <= 2017, Year >= 1998)
ba_2

# 두 개의 데이터프레임을 합침
baby = cbind(EDU2, ba_2)
baby

# 컬럼명 변경
names(baby) = c('국가', '년도', '진학률','Year', '나이', 'Country')
baby

# 필요없는 열 제거
baby = baby[, -c(4, 6)]
baby

# 국가별로 분리 
k_o = split(baby, baby$국가)
k_o

# KOR에 속한 데이터 담기
k_b = k_o$KOR
k_b

# p-value 추출
cor.test(k_b$진학률, k_b$나이)

# 한국 대학 진학률과 나이의 상관관계 분석
attach(k_b)
plot(진학률~나이, main="한국 대학 진학률과 나이")
# -----> 한국의 대학 진학률이 증가함과 동시에 초산 나이 또한 유사한 증가폭을 보임