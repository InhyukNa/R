library(KoNLP)
library(dplyr)
useNIADic()
library(readxl)

# 데이터 준비하기
txt = readLines("hiphop.txt")
txt
head(txt)

# 특수문자 제거하기
library(stringr)
txt = str_replace_all(txt, "\\W", " ")
head(txt)

# 명사 추출하기
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

# 가사에서 명사 추출
nouns = extractNoun(txt)

# 추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount = table(unlist(nouns))

# 데이터 프레임으로 변환
df_word = as.data.frame(wordcount, stringsAsFactors = F)

# 변수명 수정
df_word = rename(df_word, word = Var1, freq = Freq)

# 두 글자 이상 단어 추출
df_word = filter(df_word, nchar(word) >= 2)
head(df_word)

# 빈도 순으로 정렬한 후 상위 20개 단어 추출
top_20 = df_word %>% arrange(desc(freq)) %>% head(20)
top_20


## 워드 클라우드 만들기
# 패키지 설치
install.packages("wordcloud")

# 패키지 로드
library(wordcloud)
library(RColorBrewer)

# Dark2 색상 목록에서 8개 생상 추출
pal = brewer.pal(8, "Dark2")

# 난수 고정하기
set.seed(1234)

# 워드 클라우드 만들기
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)

# 단어 색상 바꾸기
pal = brewer.pal(9, "Blues")[5:9]  # 색상 목록 생성
set.seed(1234)                     # 난수 고정

wordcloud(words = df_word$word,    # 단어
          freq = df_word$freq,     # 빈도
          min.freq = 2,            # 최소 단어 빈도
          max.words = 200,         # 표현 단어 수
          random.order = F,        # 고빈도 단어 중앙 배치
          rot.per = .1,            # 회전 단어 비율
          scale = c(4, 0.3),       # 단어 크기 범위
          colors = pal)            # 색상 목록


## 국정원 트윗 텍스트 마이닝
twitter = read.csv("twitter.csv",
                   header = T,
                   stringsAsFactors = F,
                   fileEncoding = "UTF-8")

twitter = rename(twitter,
                 no = 번호,
                 id = 계정이름,
                 date = 작성일,
                 tw = 내용)

twitter$tw = str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)

nouns = extractNoun(twitter$tw)

wordcount = table(unlist(nouns))

df_word = as.data.frame(wordcount, stringsAsFactors = F)

df_word = rename(df_word,
                 word = Var1,
                 freq = Freq)

df_word = filter(df_word, nchar(word) >= 2)

top20 = df_word %>% arrange(desc(freq)) %>% head(20)

top20

head

findFreqTerms(top20, 10)

library(ggplot2)

order = arrange(top20, freq)$word

ggplot(data = top20, aes(x=word, y=freq)) + ylim(0, 2500) + geom_col() + coord_flip() +
  scale_x_discrete(limit=order) + geom_text(aes(label=freq), hjust=-0.3)

pal = brewer.pal(8, "Dark2")
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)

pal = brewer.pal(9, "Blues")[5:9]
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)

install.packages("wordcloud2")
library(wordcloud2)

wordcloud2(twitter)
wordcloud2(twitter, size=0.5, col="random-dark")
wordcloud2(twitter, size=0.5, col="random-dark", rotateRatio= 0)
wordcloud2(twitter, figPath="twitter.png", size=0.5)


# 기사 발췌
moon = readLines("moon.txt")
head(moon)

moon = str_replace_all(moon, "\\W", " ")
nouns = extractNoun(moon)
wordcount = table(unlist(nouns))
df_word = as.data.frame(wordcount, stringsAsFactors = F)
df_word = rename(df_word,
                 word = Var1,
                 freq = Freq)
df_word = filter(df_word, nchar(word) >= 2)
pal = brewer.pal(8, "Dark2")
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(6, 0.2),
          colors = pal)
df_word



wordcloud2(data=df_word, fontFamily = '나눔바른고딕')

library(wordcloud)
library(RColorBrewer)


## 외부 데이터 이용하기
library(readxl)
df_exam = read_excel("excel_exam.xlsx")
df_exam
mean(df_exam$english)
mean(df_exam$science)

df_exam_novar = read_excel("excel_exam_novar.xlsx")
df_exam_novar
df_exam_novar = read_excel("excel_exam_novar.xlsx", col_names = F)
df_exam_novar

df_exam_sheet = read_excel("excel_exam_sheet.xlsx", sheet=3)
df_exam_sheet

df_csv_exam = read.csv("csv_exam.csv")
df_csv_exam

df_midterm = data.frame(english = c(90,80,60,70),
                        math = c(50,60,100,20),
                        class = c(1,1,2,2))
df_midterm
write.csv(df_midterm, file='df_midterm.csv')

saveRDS(df_midterm, file='df_midterm.rds')
rm(df_midterm)
df_midterm
df_midterm = readRDS('df_midterm.rds')
df_midterm

## 데이터 파악하기
exam = read.csv("csv_exam.csv")
head(exam)
head(exam, 10)
tail(exam)
View(exam)
dim(exam)
str(exam)
summary(exam)

# mpg 데이터 파악하기
mpg = as.data.frame(ggplot2::mpg)
head(mpg)
dim(mpg)
str(mpg)
summary(mpg)
View(mpg)
class(mpg)

# 변수명 바꾸기
df_raw = data.frame(var1=c(1,2,1),
                    var2=c(2,3,2))
df_raw
library(dplyr)
df_new = df_raw
df_new = rename(df_new, v2=var2)
df_new

# 파생 변수 만들기
df = data.frame(var1=c(4,3,8),
                var2=c(2,6,1))
df
df$var_sum = df$var1 + df$var2
df
df$var_mean = (df$var1 + df$var2) / 2
df

mpg$total = (mpg$cty + mpg$hwy)/2  # 통합 연비 변수 생성
head(mpg)

mean(mpg$total)  # 통합 연비 변수 평균
summary(mpg$total)  # 요약 통계량 산출
hist(mpg$total)  # 히스토그램 생성
