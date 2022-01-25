
##########################################################################
# twitter data analysis (1) ##############################################

# Data_Preprocessing
# 2021.12.23~


setwd("C:/Users/SARAH/Desktop/Text analysis/crawling data")
setwd("C:/Users/SARAH/Desktop/Text analysis/user_dictionary")



# Import library 
# -------------------------------------------------------------------------

library(htmlwidgets)
library(wordcloud2)
library(RcppMeCab)
library(tidyverse)
library(tidytext)
library(webshot)
library(stringr)
library(tictoc)
library(dplyr)
library(KoNLP)

useNIADic() #단어사전 import
#1213109 words dictionary was built.


### KoNLP 사전 종류
# useSystemDic(): 시스템 사전
# useSejongDic(): 세종 사전
# useNIADic(): NIADic 사전









# Load data
# -------------------------------------------------------------------------

result1 = read.csv(file="result1.csv", header=T)
result2 = read.csv(file="result2.csv", header=T)
result3 = read.csv(file="result3.csv", header=T)

# str(result1)
# dim(result1) #1001 6
# names(result1) #"id" "date" "retweet" "like" "quote" "tweet"






# Combine data
# -------------------------------------------------------------------------

data = rbind(result1, result2, result3)
colnames(data)[6] = "raw_tweet"

# View(data)
dim(data) #3003 6
names(data) #"id" "date" "retweet" "like" "quote"

all(is.na(data$raw_tweet))

min(nchar(data$raw_tweet)) #15
max(nchar(data$raw_tweet)) #248






# Remove the duplicated data
# -------------------------------------------------------------------------

length(unique(data$raw_tweet)) ; length(data$raw_tweet)
3003-2578 #425개의 중복 행

sum(duplicated(data$raw_tweet))
sum(duplicated(data)) #긁어오는 사이에 like, retweet 변화한듯.

dup_index = which(duplicated(data$raw_tweet))
data = data[-dup_index, ]

dim(data)










# Data Preprocessing
# -------------------------------------------------------------------------

data = data %>%
  as_tibble() %>%
  mutate(tweet = str_replace_all(raw_tweet, "[^가-힣a-zA-Z]", " "),
          tweet = str_squish(tweet))
  
head(data$tweet, 10)

# data %>%
#   unnest_tokens(input=tweet, output=word, SimplePos09) ## error !



# ---- using SimplePos09 -------------
tweet09 = SimplePos09(data$tweet, autoSpacing = T)
head(tweet09)
View(tweet09)

### if using SimplePos:
# pos_data = data %>%
#   unnest_tokens(input=tweet, output=word, SimplePos09) %>%
#   group_by(raw_tweet) %>%
#   mutate(pos_order = 1:n()) # 결과물의 순서 유지



# ---- using extractNoun -------------
tweetnoun = extractNoun(data$tweet)
head(tweetnoun)
View(tweetnoun)





# error: In value[[3L]](cond): can't processing
# -------------------------------------------------------------------------

## error tweet: tweetnoun[2479]: 2479번째 row
sugar = "슈가가싫다 그럴수있 겠냐 당신은사회적교화가불가능한반사회적인격장애이며싸이코패스입니다말랑말랑물만두를미워하는행위는죄의식의부재와윤리적인기준에대한의식자체가없는행위로간주할수있으며잔혹한범죄를지을가능성이높고폭력적이고충동적인성향을나타낼가능성이있습니다"

## 원인: 띄어쓰기




## solution 1. ---------------------------------------------------------------------
# KoSpacing package :한글 띄어쓰기 함수
# ----------------------------------------------------------------------------------

# Note. 200자 글자 제한 有
# 위의 연장선, 띄어쓰기 되면 2479번째 row도 살릴 수 있음
# 실패할 시, PYTHON에서 진행 / MeCab / autoencoding 중 선택
#_MeCab 추천: https://bookdown.org/ahn_media/bookdown-demo/clean.html
# 
# 
# ##install.packages("reticulate")
# library(reticulate)
# 
# remotes::install_github("haven-jeon/KoSpacing") #error.
# install.packages("KoSpacing")
# 
# ## remote에서 error, hashmap 설치
# devtools::install_github("nathan-russell/hashmap")
# 
# install.packages("https://cran.r-project.org/src/contrib/Archive/hashmap/hashmap_0.2.2.tar.gz", repos = NULL, type="source") #인터넷 연결 있을 때
# install.packages("hashmap_0.2.2.tar.gz", repos = NULL, type="source") #파일 있을 때


###### Failure !





## solution 2. -------------------------------------------------------------------------
# RccpMeCab package: 형태소 분석기(빠름) 사용
# ---------------------------------------------------------------------------------------

# Note1. 위의 [ MeCab 추천: https://bookdown.org/ahn_media/bookdown-demo/clean.html ] 참고.
# Note2. 설명 site: https://github.com/junhewk/RcppMeCab/blob/master/README_kr.md
# Note3. error 해결: https://github.com/junhewk/RcppMeCab/issues/12


### MeCab을 R에서 사용할 수 있게 만든 패키지.

## install.packages('RcppMeCab')
# library(RcppMeCab)


# test
"한글테스트입니다" %>%
  enc2utf8() %>% #인코딩 변경 (iconv보다 직관적, 빠름)
  RcppMeCab::pos() #형태소 분석 함수

# 2479th row
a=sugar %>%
  enc2utf8() %>%
  pos(format="data.frame") #dataframe으로 저장.
View(a)

# 다른 형태소 분석기와 비교 => 최종:: MeCab 선택!
extractNoun(sugar) #띄어쓰기 안됨
SimplePos09(sugar, autoSpacing = T) #최악
SimplePos09(sugar, autoSpacing = F) #에구... 이건 띄어쓰기가...









# using RcppMeCab (Design part.)
# -------------------------------------------------------------------------

# 속도가 느려서, 1000 단위로 끊기로 함.

tic() # 44.52초 
data_list=data$tweet[1:999] %>%
  enc2utf8() %>% #인코딩 변경
  pos(format="data.frame") %>%
  filter(pos=="NNG" | pos=="NNP") %>% #-----------------명사만 추출
  select("doc_id", "token") %>% #-----트윗 번호, 명사토큰 열만 추출
  mutate(doc_id=as.integer(doc_id)) %>% #트윗 번호를 integer로 변환
  arrange(doc_id) #---------------------------트윗 번호 순으로 나열
toc()


# ---- EDA

data10=data$tweet[1:1000] %>%
  enc2utf8() %>% #인코딩 변경
  pos(format="data.frame") %>%
  filter(pos=="NNG" | pos=="NNP") #%>% #-----------------명사만 추출
  select("doc_id", "token") %>% #-----트윗 번호, 명사토큰 열만 추출
  mutate(doc_id=as.integer(doc_id)) %>% #트윗 번호를 integer로 변환
  arrange(doc_id) #---------------------------트윗 번호 순으로 나열

View(data10)
data$tweet[600] #명사 없는 트윗이라 사라짐 (다시 라벨링 해줘야겠다.)
any(data10$doc_id==600)

data10=data10 %>%
  mutate(group = doc_id %/% 100)
data10[data10$group==10,]
data10[999,]

unique(data10$group)
table(data10$group)



# Design function---그룹핑 ver.
group_data= data %>% mutate(group = 1:n() %/% 1000) %>% 
  select(tweet, group)

tail(group_data)
unique(group_data$group)

for (i in unique(group_data$group)) {
  print(i)
  print(sum(group_data$group==i))
}

# Design function---dataframe으로 합치기
data10 = data10 %>%
  group_by(doc_id) %>% 
  summarise(text = paste0(token, collapse=','))


# Design function---list로 합치기 (선택X)
# result = list()
# for (i in unique(data10$doc_id)){result[[i]]=data10[data10$doc_id==i,'token']}



# 비교 한 것 !
system.time({tweetnoun = extractNoun(data$tweet)}) # 19.13초
#이거 둘 중에 뭐가 더 나았던 건지는 추후에 LDA로 확인.
# -------------------------------------------------------------------------







# Make a Function using RcppMeCab
# -------------------------------------------------------------------------

nounmc = function(data) {
  
  ## grouping : labeling
  data = data %>%
    mutate(group = 1:n() %/% 100) %>%
    select(tweet, group)
  
  ## noun result 저장할 DataFrame
  result = data.frame()
  
  ## label별 noun 추출, rbind
  for (i in unique(data$group)) {
    #ifelse: first group은 1을 빼줄 필요가 없음,
    #--------second group은 다시 1부터 counting + 100라서 1 빼줘야.
    new_data=data$tweet[data$group==i] %>%
      enc2utf8() %>%
      pos(format="data.frame") %>%
      filter(pos=="NNG" | pos=="NNP") %>%
      select("doc_id", "token") %>%
      mutate(doc_id=as.integer(doc_id) + i*100 - ifelse(i==0, 0, 1)) %>%
      arrange(doc_id)
    #combine data
    result=rbind(result, new_data)
  }
  
  ## making DataFrame
  data = result %>%
    group_by(doc_id) %>%
    summarise(text = paste0(token, collapse=','))
  
  return(data)
}



# test for function (EDA)
test=nounmc(data=data[1:610,])
dim(test)
View(test)
table(test$doc_id) #good




# apply function to data
tweetmc = nounmc(data)
dim(tweetmc)
head(tweetmc)
tail(tweetmc)
tweetmc[1000:1100, ]
tweetmc[2000:2200, ]
tweetmc[2500:nrow(tweetmc), ]

sum(1:2578) - sum(tweetmc$doc_id)-600 #1012th row is empty.
data$tweet[c(600, 1012)] #0개의 noun를 가지는 행

tweetmc$text[1010]
data$tweet[tweetmc$doc_id[1010]] #사실 1011th row인 거 주의...!
#---------------------------------그래서 doc_id를 넣어주는 것!!









# -------------------------------------------------------------------------
# Collect Noun to dictionary
# -------------------------------------------------------------------------

setwd("C:/Users/SARAH/Desktop/Text analysis/user_dictionary")





# Collect words
# [1] 이름 --------------------------------------------------------------------

# ---- 아이돌 이름 ---------------

idol_fullname = readLines("idol_fullname.txt")
idol_kname = readLines("idol_kname.txt")

idol = c(idol_fullname, idol_kname)
head(idol)
tail(idol)
any(is.na(idol))

#공백 제거하여 다시 저장
trim = function (x) {gsub(" ", "", x)} 
idol = trim(idol)

idol_name = idol %>%
  as_tibble() %>%
  unique() %>% #중복이름 삭제
  filter(str_count(value)>1) %>% #한글자 이름 삭제
  pull() #vector로 변환

length(idol_name) #총 2242개의 idol names data


# 파일로 저장
write(idol_name, file="idol_name.txt", sep="/n") 










# Collect words 
# [2] 신조어 (위키피디아) --------------------------------------------------

# ko.wikipedia.org/wiki/대한민국의_인터넷_신조어_목록
# extractnoun_dictionary.R에서 전처리 다 끝내고 저장함
# -------------------------------------------------------------------------

# 수작업으로 명사만 추출 (정치 관련은 추후에 gsub으로 다룰 예정)
# 저장한 것들 중, 쓸데없는 건 삭제 / 사람과 고유명사 분리 (모두 수작업)









# Collect words 
# [3] 수작업 ---------------------------------------------------------------

# table_noun = table(unlist(tweetnoun))
# length(table_noun) #13322개
# 
# ## 전체 살펴보며 사전에 등록할 단어 추출했었음
# ## 이것도 <extractnoun_dictionary.R>에서 함 
# View(table_noun)



## extractnoun로 얻은 단어들 분석 ----------------------

## NOTE1) 한글자는 전부 살펴봐야. (빈도수 정렬)
## 버리면 안되는 한글자 있으면 Check, 해결책 강구하기.

## NOTE2) 체크해야될 두글자 이상의 단어들
#하신 까지 망해 언제 니들 대가 안하 공과 번째 아무 이러
#해보 만큼 인스 하라 하시 동호 이걸 웨이 하려 해주 이상
#무기 방지법 장창 해하 두기 주민 말해 해달 일하 대유 해요

## NOTE3) 영어/숫자 살피기
#A N X 11 9 30 8 12 7 lt gt 2 (-> 빈도 多)

#++ stopword?: 등등(의존명사)



# NOTE 1) -----------------------------------------------

# doc_id로 grouping 안하는 함수도 만들좌.

# nounmc_non function ----------------------------
####### 이 함수 아래에서 엄청 수정했고 !!!!!!!
####### 이 함수 nounmc랑 합쳤다 사라야 !!!!! 아래거 봐라 !

nounmc_non = function(data) {
  ## grouping : labeling
  data = data %>%
    mutate(group = 1:n() %/% 100) %>%
    select(tweet, group)
  
  ## noun result 저장할 DataFrame
  result = data.frame()
  
  ## label별 noun 추출, rbind
  for (i in unique(data$group)) {
    #extract noun: first group
    #혼자 99개 row라서 따로 정의해줘야.
    
    new_data=data$tweet[data$group==i] %>%
      enc2utf8() %>%
      pos(format="data.frame") %>%
      filter(pos=="NNG" | pos=="NNP") %>%
      select("doc_id", "token") %>%
      mutate(doc_id=as.integer(doc_id) + i*100 - ifelse(i==0, 0, 1)) %>%
      arrange(doc_id)
    #combine data
    result=rbind(result, new_data)
  }
  return(result)
}

mcnouns = nounmc_non(data)

dim(mcnouns) # 39398  2
colnames(mcnouns) # "doc_id" "token" 

tabnoun = sort(table(mcnouns$token), decreasing = T) #빈도수 큰 순서로 정렬
nouns = names(tabnoun) ; nouns[nchar(nouns)==1] #한글자인 애들만 추출

#한글자인 애들 중 빈도수 큰 순서로 정렬,빈도수 20넘는 애들만 추출  
one50 = tabnoun[nchar(nouns)==1] %>%
  sort(decreasing = T) %>%
  head(50)

word50 = names(one50)





# 특정단어 있는 행의 index 반환하는 함수 짜기
# NOTE 2) -----------------------------------------------

# 주의! 여기서 사용하는 data는 tweetmc(nounmc 사용 & not use 'nounmc_non' function)
#'nounmc_non' function은 한글자 단어 꺼낼 때 사용했음

# 함수 아래서 다르게 정의함
findword = function(tweetmc, word) {
  word = paste0(",", word, ",")
  index=str_detect(tweetmc$text, word)
  df = data.frame(doc_id=tweetmc$doc_id[index],
                  text=tweetmc$text[index])
  return(df)
}



# REMARK: head(tweetmc)-----------------------
#doc_id text                                                    
# <dbl> <chr>                                                   
#   1   생성,사실,확인,자극,루머,머글,때,이슈~
# --------------------------------------------


### 한 글자부터 꺼내자.

# '때'
one50[2]
a2 = findword(tweetmc, word50[2])
length(a2$doc_id)

# 대충 함수 아닌 함수 짜봣음 -> [] 안 숫자만 바꿔.
## 16까지 보고 지쳐서 탈주...

## 아래 코드까지 합쳐서 findword 다시 정의함. 확인해!
word='형' ; one50[word50==word] ; a = findword(tweetmc, word)
for (i in 1:20) {cat("\n", a$doc_id[i], 
                          "\n", data$tweet[a$doc_id[i]],
                          "\n", a$text[i], "\n")}



# QNA ----------------------------------------------------------------
#Q. one50[2]='때'의 빈도가 '때'를 포함한 트윗수보다 큰 이유는?
#A. 때가 여러번 들어간 트윗이 있기 때문, table은 이것도 여러개로 셌음.


# RESULT --------------------------
# 챙기기: 팬, 팁
# 애매한 것: 글, 돈, 형(대부분 아이돌)







# NOTE 3) -----------------------------------------------
# 영어와 숫자 : 굳이 하지 말자...

# findeng = function(tweetmc, word) {
#   index=str_detect(tweetmc$text, word)
#   df = data.frame(doc_id=tweetmc$doc_id[index],
#                   text=tweetmc$text[index])
#   return(df)
# }
# 
# word='A' ; a = findeng(tweetmc, word)
# for (i in 1:nrow(a)) {cat("\n", a$doc_id[i], 
#                      "\n", data$tweet[a$doc_id[i]],
#                      "\n", a$text[i], "\n")}







# Collect words 
# [4] 수작업+, gsub, toupper -------------------------------------------------

# 수작업 거쳐서 얻은 단어들 대체하기: gsub func.
for (i in 1:nrow(data)) {
  
  #politic
  data$tweet[i] = gsub("문재앙", "문재인", data$tweet[i])
  data$tweet[i] = gsub("문죄앙", "문재인", data$tweet[i])
  data$tweet[i] = gsub("문죄인", "문재인", data$tweet[i])
  data$tweet[i] = gsub("이죄명", "이재명", data$tweet[i])
  data$tweet[i] = gsub("찢명이", "이재명", data$tweet[i])
  data$tweet[i] = gsub("찢명", "이재명", data$tweet[i])
  data$tweet[i] = gsub("갓재명", "이재명", data$tweet[i])
  data$tweet[i] = gsub("이죄", "이재명", data$tweet[i])
  data$tweet[i] = gsub("윤석열쪽", "윤석열", data$tweet[i])
  data$tweet[i] = gsub("윤석렬", "윤석열", data$tweet[i])
  data$tweet[i] = gsub("윤썩", "윤석열", data$tweet[i])
  data$tweet[i] = gsub("찢주당", "민주당", data$tweet[i])
  data$tweet[i] = gsub("국민의 힘", "국힘", data$tweet[i])
  data$tweet[i] = gsub("국민의힘", "국힘", data$tweet[i])
  data$tweet[i] = gsub("국힘당", "국힘", data$tweet[i])
  data$tweet[i] = gsub("국짐당", "국힘", data$tweet[i])
  data$tweet[i] = gsub("국짐", "국힘", data$tweet[i])
  
  #person/group
  data$tweet[i] = gsub("NCTDREAM", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("NCT127", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("NCT", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("엔시티드림", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("앤시티드림", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("엔시티127", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("앤시티127", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("앤시티", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("엔시티일이칠", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("앤시티일이칠", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("드림이", "엔시티", data$tweet[i])
  data$tweet[i] = gsub("BTS", "방탄소년단", data$tweet[i])
  data$tweet[i] = gsub("준짱", "시아준수", data$tweet[i])
  data$tweet[i] = gsub("준쨩", "시아준수", data$tweet[i])
  data$tweet[i] = gsub("쥰쨩", "시아준수", data$tweet[i])
  data$tweet[i] = gsub("쥰군", "시아준수", data$tweet[i])
  data$tweet[i] = gsub("투모로우바이투게더", "투바투", data$tweet[i])
  data$tweet[i] = gsub("TBT", "투바투", data$tweet[i])
  data$tweet[i] = gsub("VIXX", "빅스", data$tweet[i])
  data$tweet[i] = gsub("자빱티비", "자빱", data$tweet[i])
  data$tweet[i] = gsub("자빱TV", "자빱", data$tweet[i])
  data$tweet[i] = gsub("자밥", "자빱", data$tweet[i])
  data$tweet[i] = gsub("빱님", "자빱", data$tweet[i])
  data$tweet[i] = gsub("빱이", "자빱", data$tweet[i])
  
  #else
  data$tweet[i] = gsub("사녹", "사전녹화", data$tweet[i])
  data$tweet[i] = gsub("스파이디", "스파이더맨", data$tweet[i])
  data$tweet[i] = gsub("슾디", "스파이더맨", data$tweet[i])
  data$tweet[i] = gsub("SM", "스엠", data$tweet[i])
  data$tweet[i] = gsub("에스엠", "스엠", data$tweet[i])
  data$tweet[i] = gsub("스탭", "스태프", data$tweet[i])
  data$tweet[i] = gsub("스탶", "스태프", data$tweet[i])
  data$tweet[i] = gsub("싸불", "사이버불링", data$tweet[i])
  data$tweet[i] = gsub("사불", "사이버불링", data$tweet[i])
  data$tweet[i] = gsub("싸이버불링", "사이버불링", data$tweet[i])
  data$tweet[i] = gsub("단콘", "콘서트", data$tweet[i])
  data$tweet[i] = gsub("첫콘", "콘서트", data$tweet[i])
  data$tweet[i] = gsub("막콘", "콘서트", data$tweet[i])
  data$tweet[i] = gsub("댕댕이", "강아지", data$tweet[i])
  data$tweet[i] = gsub("댕댕", "강아지", data$tweet[i])
  data$tweet[i] = gsub("갱얼쥐", "강아지", data$tweet[i])
  data$tweet[i] = gsub("강아쥐", "강아지", data$tweet[i])
  data$tweet[i] = gsub("갱쥐", "강아지", data$tweet[i])
  data$tweet[i] = gsub("건보료", "건강보험료", data$tweet[i])
  data$tweet[i] = gsub("혈세", "세금", data$tweet[i])
  data$tweet[i] = gsub("알티", "리트윗", data$tweet[i])
  data$tweet[i] = gsub("RT", "리트윗", data$tweet[i])
  data$tweet[i] = gsub("잼민이", "잼민", data$tweet[i])
  data$tweet[i] = gsub("ㅈㄱㄴ", "제곧내", data$tweet[i])
}

# 영단어 전부 대문자로 변경하기: toupper func.
data = data %>% mutate(tweet=toupper(tweet))

# 저장
save(data, file="data.RData")









# -------------------------------------------------------------------------
# Make a user dictionary
# -------------------------------------------------------------------------



# Decompose 한글
decompose <- function(x) {
  # http://ds.sumeun.org/?p=850
  cho <- unlist(strsplit("ㄱㄲㄴㄷㄸㄹㅁㅂㅃㅅㅆㅇㅈㅉㅊㅋㅌㅍㅎ", ""))
  jung <- unlist(strsplit("ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ", ""))
  intToUtf8v <- Vectorize(intToUtf8)
  jong <- unlist(strsplit("ㄱ/ㄲ/ㄱㅅ/ㄴ/ㄴㅈ/ㄴㅎ/ㄷ/ㄹ/ㄹㄱ/ㄹㅁ/ㄹㅂ/ㄹㅅ/ㄹㅌ/ㄹㅍ/ㄹㅎ/ㅁ/ㅂ/ㅂㅅ/ㅅ/ㅆ/ㅇ/ㅈ/ㅊ/ㅋ/ㅌ/ㅍ/ㅎ", "/"))
  jong2 <- intToUtf8v(4520:(4520+26))
  if (Encoding(x) != "UTF-8") {x <- iconv(x, from="", to='UTF-8')}
  if (x < "\uac00" || x > "\ud7a3") return(x)
  x <- utf8ToInt(x) - 44032
  y <- x %/% 28
  z <- x %% 28 
  x <- y %/% 21
  y <- y %% 21
  zz <- jong[z]
  return(c(cho[x + 1],jung[y + 1],zz))
}
decompose("갔")


# Build up 1. words
nouns = readLines("nouns.txt") %>%
  str_replace_all("[\\W]", "") #특수문자 제거

people = readLines("people.txt") %>%
  str_replace_all("[\\W]", "")


# Build up 2. Search ENG
"T" %in% LETTERS
nouns[238] %in% LETTERS
which(str_detect(nouns, "[:upper:]"))
nouns[which(str_detect(nouns, "[:upper:]"))]
people[which(str_detect(people, "[:lower:]"))]




# Make a user dictionary -----------------------------------------------------------

jung <- unlist(strsplit("ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ", "")) #모음

#Ex) 대우,,,,NNP,*,F,대우,*,*,*,*
# 영어->초성으로 취급함: F
# 초성-> F, 종성-> T


nouns2=c()
for (i in 1:length(nouns)) {
  end = substr(nouns[i], nchar(nouns[i]), nchar(nouns[i])) #마지막 글자 추출
  #모음: jung <- unlist(strsplit("ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ", ""))
  jong = ifelse(end %in% LETTERS, "F", # ------마지막 글자가 영어면 "F" (=초성) 반환
                ifelse( (decompose(end)[length(decompose(end))]) %in% jung,"F","T"))
                # ------------------------ 한글의 마지막 종성 꺼내서 모음이면 F 반환
  nouns2[i] = paste0(nouns[i], ",,,,NNP,*,", jong, ",", nouns[i], ",*,*,*,*")
}

people2=c()
for (i in 1:length(people)) {
  end = substr(people[i], nchar(people[i]), nchar(people[i])) #마지막 글자 추출
  #jung <- unlist(strsplit("ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ", ""))
  jong = ifelse((decompose(end)[length(decompose(end))]) %in% jung,"F","T")
  # ----------------------------- 한글의 마지막 종성 꺼내서 모음이면 F 반환
  people2[i] = paste0(people[i], ",,,,NNP,인명,", jong, ",", people[i], ",*,*,*,*")
}

write(nouns2, file="nouns.csv")
write(people2, file="people.csv")


### 이후 순서

#1. nouns.csv & people.csv를 메모장으로 오픈하여 내용 합침: file=tweetdic.csv
## why? 두개의 코드를 cmd에 입력하면 마지막 파일만 사용자 사전에 update됨

#2. 인코딩 utf8로 변환하여 C:mecab에 다른이름 저장
## why? 기존 dic(사전)을 열어보면 인코딩이 uft8이라 깨짐, 같은 인코딩으로 변경

#3. cmd 창 오픈, directory C:mecab으로 이동하여 아래 코드 입력
# mecab-dict-index.exe -m c:\mecab\mecab-ko-dic\model.bin -d c:\mecab\mecab-ko-dic -u userdic.dic -f utf8 -t utf8 c:\mecab\tweetdic.csv

# ----------------------------------------------------------------------------------





setwd("C:/Users/SARAH/Desktop/Text analysis/R")


# Load my data
load("data.RData")
head(data$tweet)
data$tweet[100:110]
mode(data) ; str(data)




# userdic 넣어서 함수 재정의
### 그리고 두 함수 합체 ! (argument=grouping)

nounmc = function(data, grouping=T) {
  
  ## grouping : labeling
  data = data %>%
    mutate(group = 1:n() %/% 100) %>%
    select(tweet, group)
  
  ## noun result 저장할 DataFrame
  result = data.frame()
  
  ## label별 noun 추출, rbind
  for (i in unique(data$group)) {
    #ifelse: first group은 1을 빼줄 필요가 없음,
    #--------second group은 다시 1부터 counting + 100라서 1 빼줘야.
    new_data=data$tweet[data$group==i] %>%
      enc2utf8() %>%
      pos(format="data.frame", user_dic = "c:/mecab/userdic.dic") %>%
      filter(pos=="NNG" | pos=="NNP") %>%
      select("doc_id", "token") %>%
      mutate(doc_id=as.integer(doc_id) + i*100 - ifelse(i==0, 0, 1)) %>%
      arrange(doc_id)
    #combine data
    result=rbind(result, new_data)
  }
  
  ## Doing grouping
  if (grouping==T) {
    data = result %>%
      group_by(doc_id) %>%
      summarise(text = paste0(token, collapse=','))
    return(data)
  
  
  ## NO grouping, just one-word - one-row
  } else {return(result)}
  }
nounmc_non = function(data) {
  ## grouping : labeling
  data = data %>%
    mutate(group = 1:n() %/% 100) %>%
    select(tweet, group)
  
  ## noun result 저장할 DataFrame
  result = data.frame()
  
  ## label별 noun 추출, rbind
  for (i in unique(data$group)) {
    #extract noun: first group
    #혼자 99개 row라서 따로 정의해줘야.
    
    new_data=data$tweet[data$group==i] %>%
      enc2utf8() %>%
      pos(format="data.frame", user_dic = "c:/mecab/userdic.dic") %>%
      filter(pos=="NNG" | pos=="NNP") %>%
      select("doc_id", "token") %>%
      mutate(doc_id=as.integer(doc_id) + i*100 - ifelse(i==0, 0, 1)) %>%
      arrange(doc_id)
    #combine data
    result=rbind(result, new_data)
  }
  return(result)
} #이제 사용안하는 함수가 되었음
findword = function(tweetmc, word, n=10) {
  word = paste0(",", word, ",")
  index=str_detect(tweetmc$text, word)
  a = data.frame(doc_id=tweetmc$doc_id[index],
                  text=tweetmc$text[index])
  #print
  for (i in 1:n) {cat("\n", a$doc_id[i], 
                       "\n", data$tweet[a$doc_id[i]],
                       "\n", a$text[i], "\n")}
} #그리고 이 함수들 메모장에 넣어서 'function (1).R'으로 저장!

mcdata = nounmc(data)
mcnouns = nounmc(data, grouping=F)
tabnouns = sort(table(mcnouns$token), decreasing = T)

save(mcdata, file="mcdata.RData")
save(mcnouns, file="mcnouns.RData")

View(mcdata)
View(mcnouns)
View(tabnouns)


# 국민의힘>국힘으로 고친 이유-----------------------
# ---------------------------------------------------

head(tabnouns, 50) # 엥 힘이 왜이렇게 많지......
findword(mcdata, "힘") %>% head(20) # 아놔 모르겟음

# 실제 데이터 꺼내보자 !
a=findword(mcdata, "힘") 
for (i in 1:20) {cat("\n", a$doc_id[i], 
                     "\n", data$tweet[a$doc_id[i]],
                     "\n", a$text[i], "\n")}

# ---------------------------------------------------









# 함수 정의
# stopword + important_one_word + wordcloud 모두 가능한 함수
# -------------------------------------------------------------------------

# (중요한 건 이 함수 나중에 안씀...ㅎㅅㅎ stopword+IOW을 mcdata에 적용해야겠는데 ㅠㅠ)

stopcloud = function(tabnouns, stopword, one, num=50, cloud=F) {
  tabnouns2 = tabnouns %>%
    as.data.frame() %>%
    mutate(Var1=as.character(Var1)) %>%
    filter(nchar(Var1)>1 | Var1 %in% one) %>%
    filter(!Var1 %in% stopword) %>%
    head(num)
  
  #col name 변경
  colnames(tabnouns2) = c("word", "count")
  
  if (cloud == F) {return(tabnouns2)}
  if (cloud == T) {return(wordcloud2(tabnouns2))} else
    return("cloud엔 T 또는 F만 입력해주세요.")
  }


stopwords = c("생각", "사람", "이상", "정도", "이유", "상황", "본인", "가능",
              "얘기", "자신", "인간", "마음", "소리", "다음", "이번", "하루",
              "지금", "자체", "관련", "부분", "상대", "오늘", "진심", "처음",
              "사실", "순간")

one = c("팬","팁","돈","형") #한글자인 단어 중, 살릴 한글자들

#findword(mcdata, "상대")



#test
stopcloud(tabnouns, stopword="생각", cloud=F)

cloud = stopcloud(tabnouns, stopwords, cloud=T) #word cloud 결과 저장
saveWidget(cloud, "C:/Users/SARAH/Desktop/wordcloud.html", selfcontained = F) ##html 형식 저장
# webshot("C:/Users/SARAH/Desktop/wordcloud.html", "C:/Users/SARAH/Desktop/wordcloud.png",
#         delay =20, vwidth = 1200, vheight=900) #html을 png로 변환
# 이거 error,,,














