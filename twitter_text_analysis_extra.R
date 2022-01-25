##########################################################################
# twitter data analysis_extra ###########################################

# R.shiny
# 2022.01.24~




setwd("C:/Users/SARAH/Desktop/Text analysis/R")



# Library packages
# -------------------------------------------------------------------------

library(topicmodels)
library(RcppMeCab)
library(tidyverse)
library(ldatuning) 
library(tidytext)
library(stringr)
library(stringi)
library(KoNLP)
library(dplyr)
library(servr)
library(lda)
library(tm) 







# Load data & Data preprocessing
# -------------------------------------------------------------------------

load("mcnouns.RData")

stopword = c("생각", "순간", "이상", "정도", "상황", "본인", "가능", "얘기","자신", "마음",
             "소리", "다음", "이번", "하루", "지금", "자체", "관련", "부분", "오늘", "진심", "처음")
one = c("팬","팁","돈","형", "눈")

# stopword + IOW
mcnouns = mcnouns %>%
  filter(nchar(token)>1 | token %in% one) %>%
  filter(!token %in% stopword) %>%
  group_by(doc_id) %>%
  summarise(text = paste0(token, collapse=' '))

nouns = as.list(mcnouns$text)
names(nouns) = unique(mcnouns$doc_id)
corpusLDA=lexicalize(nouns)








# LDA
# -------------------------------------------------------------------------

set.seed(0121)

G = 1000
alpha = 0.1
eta = 0.01

top20_by_k = list()

for (k in 2:20) {
  LDAresult=lda.collapsed.gibbs.sampler(corpusLDA$documents, K=k,
                                        vocab=corpusLDA$vocab, burnin=9999,
                                        num.iterations=G, alpha=alpha, eta=eta)
  
  top20 = top.topic.words(LDAresult$topics, 20, by.score = TRUE)
  top20_by_k[[k]] = top20
}

top20_by_k[[1]] #NULL
top20_by_k[[2]]
top20_by_k[[10]]
top20_by_k[[20]]

save(top20_by_k, file="top20_by_k.RData")








# R.shiny
# -------------------------------------------------------------------------
# 아래 코드 한번에 싹 돌리면 됨!


# install.packages('shiny')
library(shiny)


# ui.R ----------------------------------------------------
ui = fluidPage(
  sidebarPanel(# 모든 유저인터페이스 컨트롤이 여기 놓인대용
    
    # 내가 선택할 요소
    selectInput("k", "number of topic:", 
                choice = c("2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                           "8"=8, "9"=9, "10"=10, "11"=11, "12"=12, 
                           "13"=13, "14"=14, "15"=15, "16"=16,
                           "17"=17, "18"=18, "19"=19, "20"=20))
  ),
  mainPanel(# 모든 아웃풋 요소는 여기에 들어간대용
    
    h3("LDA result using LDAvis in twitter data"), # 타이틀
    tableOutput("df")  # server.R에서 사용할 아웃풋 이름
  )
)


# server.R ------------------------------------------------
server <- function(input, output) {
  
  output$df <- renderTable({
    
    #Load data
    setwd("C:/Users/SARAH/Desktop/Text analysis/R")
    load("top20_by_k.RData")
    
    #columns names 설정
    col = paste0("topic", 1:as.numeric(input$k))
    
    #make a DF (=render Table)
    DF = top20_by_k[[as.numeric(input$k)]]
    colnames(DF) = col
    DF
    
  })
}

shinyApp(ui = ui, server = server)








# R.shiny ___get url
# -------------------------------------------------------------------------
# https://m.blog.naver.com/hsj2864/220915578619 참고
# https://www.shinyapps.io/ 페이지
# GitHub 아이디로 로그인


#install.packages('rsconnect')
library(rsconnect)

setAccountInfo(name='sara9801',
               token='7E7B60851327239F01C0FB9151577B6A',
               secret='UhEehao4PR6VFtqQGFaWFX9w4JM0QH4SR9Jrnty8')

deployApp("C:/Users/SARAH/Desktop/Text analysis/R/Rshiny")



## 성공 !
# Rshiny 폴더에 있는 ui.R, server.R, top20_by_k.RData 파일들로 만들었음

# 링크: https://sara9801.shinyapps.io/rshiny/









