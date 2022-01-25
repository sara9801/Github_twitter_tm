##########################################################################
# twitter data analysis (2) ##############################################

# LDA
# 2022.01.21~

# K = 6 # 임의로 정한 topic 개수
# iter = 1000 # interation

setwd("C:/Users/SARAH/Desktop/Text analysis/R")



# Library packages
# -------------------------------------------------------------------------

library(topicmodels)
library(wordcloud2)
library(RcppMeCab)
library(tidyverse)
library(ldatuning) 
library(tidytext) #DTM 객체를 직접 사용할 수 없지만,
                  #이를 tidy data frame 형태로 변환해주는 함수 제공: 아마 tidy()
library(webshot)
library(ggplot2)
library(stringr)
library(stringi)
library(tictoc)
library(LDAvis)
library(KoNLP)
library(dplyr)
library(servr)
library(lda)
library(tm) #text mining을 위한 pack

## NOTE
# LDA할 수 있는 패키지는 LDA도 있지만,
#사용성 면에서는 topicmodels가, 알고리즘 면에서는 LDA가 좀 더 나은 면





# Load data
# -------------------------------------------------------------------------

# Load functions
source('functions (1).R', encoding="utf-8") #인코딩 신경쓰기, 사실 한글 안쓰는 게 best.

# Load data
load("data.RData")
load("mcdata.RData") #nounmc(data, grouping=T)
load("mcnouns.RData") #nounmc(data, grouping=F)

head(mcdata) 
head(mcnouns) 



# stopword + important_one_word 적용 (*********************이것까지 모두 적용한 함수는 다음에 만들자.)

stopword = c("생각", "순간", "이상", "정도", "상황", "본인", "가능",
              "얘기", "자신", "마음", "소리", "다음", "이번", "하루",
              "지금", "자체", "관련", "부분", "오늘", "진심", "처음")

one = c("팬","팁","돈","형", "눈")


# stopword + IOW => sep=' '으로 데이터 grouping
mcnouns = mcnouns %>%
  # IOW 제외 한글자 제거
  filter(nchar(token)>1 | token %in% one) %>%
  filter(!token %in% stopword) %>%
  group_by(doc_id) %>%
  summarise(text = paste0(token, collapse=' '))

# 데이터 list화, list에 이름 지정
nouns = as.list(mcnouns$text)
names(nouns) = unique(mcnouns$doc_id)

# lexicalize 함수 사용, corpus 생성되었음을 확인할 수 있음
corpusLDA=lexicalize(nouns)
head(corpusLDA)


K = 8
G = 1000 # iteration
alpha = 0.1
eta = 0.01
set.seed(0121)



# Model -------------------------------------------------------------------
# burnin: 확률 추정 시 제외되는 초반 interation 값

LDAresult=lda.collapsed.gibbs.sampler(corpusLDA$documents, K=K,
                                     vocab=corpusLDA$vocab, burnin=9999,
                                     num.iterations=G, alpha=alpha, eta=eta)



# topic의 상위 n번째 단어 출력 ----------------------------------
n = 20 
top.words = top.topic.words(LDAresult$topics, n, by.score = TRUE)
print(top.words)


# topic별 비중 --------------------------------------------------
LDAresult$topic_sums # topic 단어 수
topic.proportion = LDAresult$topic_sums/sum(LDAresult$topic_sums)


# topic별 word 비중 ---------------------------------------------
dim(LDAresult$topics)
LDAresult$topics[1:10]
length(unique(LDAresult$topics)) #전체 단어 수


# 여기부턴 코드 이해못해서 좀 끄적여본 거 -----------------------
as.vector(1/LDAresult$topic_sums)
word.proportion.top100 = list()
iter = 0 #몇 개까지 표본을 수집할지


## 개쩌는 블로그주소 (이분도 LDAvis 사용함)
# https://junhewk.github.io/text/2017/08/15/complaint-LDA/

## 시각화는 이 블로그 참고하장!
# https://brunch.co.kr/@mapthecity/2

## 이것도 상세해보여서 갖고옴, 시간되면 도전해보자
# https://replet.tistory.com/48









# DTM matrix
# ---------------------------------------------------------------------------------------

# install.package

library(LDAvis) #LDA의 결과를 시각화하는 도구
library(lubridate) #character 형식을 date 형식으로 바꿔주는 패키지
library(Rmpfr) #정교한 floating point number 연산 지원, 추후에 Topic 수 결정을 위해 사용


# 다시 mcnouns 데이터 사용

head(mcnouns$text)
length(mcnouns$doc_id) #number of document = 2576개 
stopword = c("생각", "순간", "이상", "정도", "상황", "본인", "가능", 
             "얘기", "자신", "마음", "소리", "다음", "이번", "하루",
             "지금", "자체", "관련", "부분", "오늘", "진심", "처음") #위에서 정의했던 stopword
one = c("팬","팁","돈","형", "눈") #위에서 정의했던 살리기로 했던 한글자들




# 말뭉치로 변환 ----------------------------------------------------
corpus = Corpus(VectorSource(mcnouns$text))
#corpus$content %>% head() : docment 내용
#corpus[[1]]$content : 1번째 doc 내용
#length(corpus) : # of document


# DTM matrix 생성 --------------------------------------------------
dtm = DocumentTermMatrix(corpus, control=list(stopwords=stopword, #근데 위에서 이미 stopword 작업해줘...
                                              wordLengths=c(2, 20))) #단어길이는 1글자~10글자로 제한

## NOTE: control에 들어가는 argument -------------------------
#removePunctuation: 구두점 제거 (난 구두점 안챙겼지용)
#removeNumbers=F: 숫자 제거 (난 숫자 안챙겼지용)
#weighting=weightTf: term frequency
##즉 단어출현빈도로 document-term matrix를 구축하도록 함
# ------------------------------------------------------------


## 구조 파악
# dim(dtm)
# colnames(dtm) %>% head(30)
# rownames(dtm) %>% head(30)
# rownames(dtm) %>% tail(30) #문서순서로 labeling됐네... (마지막: 2578이 아니라 2576)
# any(rownames(dtm) == "600") #얘 있어 ㄷㄷ
# 
# dtm.matrix = as.matrix(dtm) #as.matrix 해줘야 행렬 볼 수 있음
# dtm.matrix[1:10, 1:10]
# 
# 
## 문서의 단어에 접근하기
# terms = Terms(dtm)
# head(terms, 10)








# LDA visualization (1). Basic (not Gibbs)
# ---------------------------------------------------------------------------------------

# LDA function in package 'topicmodels'

K=2 # 두 집단으로 가정 (토픽 2개)
lda2 = LDA(dtm, k=K, seed=42) #method: default=VEM으로 되어있음, Gibbs 아님 !
topic2 <- tidy(lda2, matrix="beta") #tidy : 가공하기 쉽게 dtm을 tidy data frame 형태로 변환
                                    #argument 'matrix' : Whether to tidy beta(per-term-per-topic, default)
                                                                    # or gamma (per-document-per-topic)

## NOTE: Gibbs -----------------------------
# lda_gibbs = LDA(dtm, k=K,method="Gibbs")
# str(lda_gibbs)
# tidy 적용 X, 나중에 시간되면 다뤄보자...
# ------------------------------------------




topic2 %>% head #토픽, 단어별 등장확률

#그룹별 상위 10개 추출
top10 = topic2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>% #beta에 대해 상위 10개
  ungroup() %>% #데이터 그룹 해제, group_by를 통해 발생할 수 있는 error 방지
  arrange(topic, -beta) #%>%: topic으로 먼저 정렬, 그 다음 beta로 정렬하는데 순서 역순으로.
#  mutate(term=reorder(term, beta) # 여기 부분 날렸음 ------------------------------------
## term을 beta에 대하여 정렬, 이때 mutate를 사용하여 term 재정의 => 열이름 지정할 필요X
## mutate 부분은, 아래 코드에서 aes 넣어주고 이름 재정의하는 걸로 해결. 이게 더 나아서....



# 두 집단의 beta(term-topic prob)값 시각화 -------------------------------

top10 %>%
  ggplot(aes(x=reorder_within(term,beta,within=topic), y=beta, fill=factor(topic))) +
                          #가로:term(topic 내에서 beta 기준으로 재정렬), 세로:beta
  geom_col(show.legend=FALSE) + #막대그래프로 만들기 (goem_bar 함수는 error 뜬대.)
  facet_wrap(~ topic, scales="free") + #topic 별 그래프 분할, prob=0인 애는 버리기
  scale_x_reordered() + # rectify x axis labels
  xlab('term') + #rename(x axis)
  coord_flip() #transpose
  
  
  



# 두 집단의 beta의 log-ratio 시각화 -------------------------------------
## 너무 작은 값이므로, 확률의 비교를 위해선 log를 사용하는 것이 합리적.

beta_spread = topic2 %>%
  spread(topic, beta) %>% #spread: https://m.blog.naver.com/bosongmoon/221587219214
  rename(term=1, topic1=2, topic2=3) %>% #열 이름 변경
  filter(topic1>0.0001 | topic2>0.0001) %>% #na는 없음
  mutate(log_ratio=log(topic2/topic1)) #topic2가 오른쪽에 오도록
  
bind_rows(beta_spread %>% top_n(10, log_ratio), beta_spread %>% top_n(-10, log_ratio)) %>%
  ggplot(aes(reorder(term, log_ratio), log_ratio)) +
  geom_col(show.legend = F) +
  labs(x="term", y="Log ratio of beta in topic2 / topic1") +
  coord_flip()


# NOTE: bind_rows VS rbind ------------------------
# rbind는 combine하려는 DF의 col이 다르면 error!
# bind_row는 combine하게 되면 NA로 들어간다.
# -------------------------------------------------




# 텍스트가 어느 집단으로 분류되었는지 확인 -------------------------------------

doc2 = tidy(lda2, matrix="gamma")
doc2 %>% filter(gamma>0.5) %>% select(document) # 50% 이상 topic1에 속할 문서들
doc2 %>% filter(gamma>0.6) %>% select(document)
## 문제점: 0.6을 넘는 gamma가 없음 ㅋㅋ 아 이거 분석 완전히 잘못한듯...

# document 1에 속하는 단어들의 빈도
tidy(dtm) %>% filter(document==10) %>% arrange(desc(count))
as.data.frame(mcnouns[10,]) # 비교


################################################################### 나중에 topic browser 해보기
# 실제로 문헌 분석을 위해서 Jacobi 등은 topic browser를 만들어서 활용할 것을 권하고 있습니다.
# Topic brower란, topic의 중요 단어와 가장 일치도가 높은 텍스트를 같이 보면서 해당 topic의 주제를
# 추측할 수 있도록 하는 방식을 의미합니다. (문헌: paper 폴더에 jacobi_lda.pdf로 다운받아놓음)













# LDA visualization (2). LDAvis (using Gibbs)
# ---------------------------------------------------------------------------------------

# LDAvis 사용을 위해서는 phi, theta, vocab, doc.length, term.frequency list를 만들어서
# createJSON 함수에 넣어준 다음, serVis 함수를 적용하면 지정한 폴더에 결과가 저장됩니다.

K = 7
G = 5000
alpha = 0.01
fit = LDA(dtm, k=K, method='Gibbs', control=list(iter=G, alpha=alpha))

fit


# posterior -------------------------------------------

str(posterior(fit))
dim(posterior(fit)$term) #topic-term prob
posterior(fit)$term[1:7, 1:5]

dim(posterior(fit)$topic) #doc-topic prob
posterior(fit)$topic[1:5, 1:7]


# topic-term prob -------------------------------------
phi = posterior(fit)$terms %>% as.matrix 
phi[1:5, 1:5]

# doc-topic prob --------------------------------------
theta = posterior(fit)$topics %>% as.matrix

# vocab -----------------------------------------------
vocab = colnames(phi)

# doc.length ------------------------------------------
doc_length2 = c()
for(i in 1:length(corpus)) {
  temp = corpus$content[i]
  doc_length2[i] = stri_count(temp, regex='\\S+')
  #regex: 정규표현식 이용해 문자수 세기 (\S: 공백아닌 문자)
}

# term.frequency --------------------------------------
temp_frequency = as.matrix(dtm) # doc x term
temp_frequency = colSums(temp_frequency)

# freq_matrix = data.frame(ST=colnames(temp_frequency),
#                         Freq=colSums(temp_frequency))



# Making json file ------------------------------------
json = options(encoding="utf-8") #인코딩 변경
json = createJSON(phi=phi,
                  theta=theta,
                  vocab=vocab,
                  doc.length=doc_length,
                  term.frequency=temp_frequency)


# Making a browser -------------------------------------
setwd("C:/Users/SARAH/Desktop/Text analysis/Github_twitter_tm")
serVis(json, out.dir='twitter_LDAvis', open.browser=T)

# 안열려, 깃허브로 옮겨서 page open함
# https://sara9801.github.io/Github_twitter_tm/twitter_LDAvis/index.html

# lambda에 대한 해석: 나중에 따로 정리
# https://lovit.github.io/nlp/2018/09/27/pyldavis_lda/



 







