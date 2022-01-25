##########################################################################
# twitter data analysis (3) ##############################################

# Determining the number of topics
# 2022.01.25~





setwd("C:/Users/SARAH/Desktop/Text analysis/R")



# Library packages
# -------------------------------------------------------------------------


library(topicmodels)
library(wordcloud2)
library(RcppMeCab)
library(tidyverse)
library(ldatuning) 
library(lubridate)
library(tidytext)
library(webshot)
library(ggplot2)
library(stringr)
library(stringi)
library(tictoc)
library(LDAvis)
library(Rmpfr)
library(KoNLP)
library(dplyr)
library(servr)
library(lda)
library(tm)

# +

library(ldatuning) # FindTopicsNumber_plot








# determining # of topics (1). 혜린언니 방법 이용
# -------------------------------------------------------------------------
# https://rpubs.com/MNidhi/NumberoftopicsLDA

load("mcnouns.RData")

stopword = c("생각", "순간", "이상", "정도", "상황", "본인", "가능",
             "얘기", "자신", "마음", "소리", "다음", "이번", "하루",
             "지금", "자체", "관련", "부분", "오늘", "진심", "처음")

one = c("팬","팁","돈","형", "눈")

mcnouns = mcnouns %>%
  filter(nchar(token)>1 | token %in% one) %>%
  filter(!token %in% stopword) %>%
  group_by(doc_id) %>%
  summarise(text = paste0(token, collapse=' '))


# making a DTM --------------------------------------------
corpus = Corpus(VectorSource(mcnouns$text))
dtm = DocumentTermMatrix(corpus, control=list(wordLengths=c(2, 20)))

# 한 문서에 단어 2개 이하 => 그 문서 없애봄. (그 전까진 안했던 시도)
rowsum = apply(dtm, 1, sum)
dtm = dtm[rowsum>2, ]


# find topic num ------------------------------------------
tic()
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 42),
  mc.cores = 4L,
  verbose = TRUE
)
toc() #걸린 시간: 35.98초


# draw a plot ---------------------------------------------
FindTopicsNumber_plot(result)





##################################################
# + 혜린언니자료에 있는 노인일자리 논문 읽어보기 #
##################################################



# determining # of topics (2). Perplexiry(PPL)
# -------------------------------------------------------------------------
# https://rpubs.com/MNidhi/NumberoftopicsLDA
# PPL 짧은 설명: https://wikidocs.net/21697

ppl_df = data.frame(train=numeric()) #test 없어서 삭제
topics = 2:20
burnin = 100
iter = 1000
keep = 50

set.seed(42)

for (i in topics){
  fitted <- LDA(dtm, k = i, method = "Gibbs",
                control = list(burnin = burnin, iter = iter, keep = keep))
  ppl_df[i,1] = perplexity(fitted, newdata = dtm)
}

# plotting the perplexity ---------------------------------

g = ggplot(data=ppl_df, aes(x=as.numeric(row.names(ppl_df)))) +
  labs(y="Perplexity", x="Number of topics") +
  ggtitle("Perplexity") +
  geom_point(aes(y=train), size=2, colour="blue") +
  geom_line(aes(y=train), colour="blue")























