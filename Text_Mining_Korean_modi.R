#======================================================#
# Title: 'Text Mining with Quanteda for Social Science #
# Author: "CHOI, Doo Young Wicks"                      #
# Date: 2020-10-01                                     #
#======================================================#

# This code is to do co-occurrence analysis with "R" quanteda-package.
# The code used to show links between words with the semantic perspectives.


#====================================#
# Adding basic Library Environment   #
#====================================#

# Loading the libraries for the 'Text Mining' Process
# 1) loading Text or Excel files
library(readtext)
library(readxl)
# 2) LSE Mining Tool
library(quanteda)
library(quanteda.textstats)
library(SnowballC)
# 3) To perform "part-of-speech tagging", "named entity recognition's", dependency relationship analysis.
library(spacyr)
# 4) Stopwords library
library(stopwords)
# 5) plot library
library(ggplot2)
# 6) Topic Model library
library(topicmodels)
library(seededlda)
# 7) Sampling Library
library (rsample)
# 8) Extra Fonts
library(extrafont)
# 9) Managing dates
library(lubridate)
# 10) Sentiment Analysis
library(sentometrics)
# Table Manipulation
library(plyr)
library(stringr) #for controlling raw data
library(tidyr) #for convert to wide format
library(tidyverse)
library(tcltk2)
library(igraph)
library(ldatuning) #measure optimum topic
#====================================
# File Loading
#====================================
# Choose a file to process
fileToLoad <- file.choose()
# In cases of Excel file
Text.source <- read_excel(fileToLoad)
# In cases of a CSV or a TXT file
# Text.source <- readtext(fileToLoad)

# Chage Cell Name, if needs
# X <- X %>% rename(text=texts)
#Text.source <- Uganda_New_Vision


#====================================
# Preprocessing Data
#====================================

# Preparing Corpus Target
Mining.text.corpus <- Text.source

Mining.text.corpus

# Processing Corpus
Mining.text.corpus <- quanteda::corpus(Mining.text.corpus$texts)

# Check Corpus Summary

summary(Mining.text.corpus)

#============================================#
# Pre data clensing for the Text analysis    #
# Setting personal stopwords                 #
#============================================#

mystopwords <- c("가", "가까스로", "가량", "가령", "가운데", "가지", "각", "각각", "각자",
                   "각종", "간", "갖고말하자면", "갖은", "같다", "같이", "개", "개국", "개년",
                   "개소", "개월", "개의치않고", "걔", "거", "거기", "거니와", "거리", "거바",
                   "거의", "건", "것", "것과", "것들", "게다가", "게우다", "겨를", "겨우", "격",
                   "견지에서", "결과에", "결국", "결론을", "겸", "고", "고려하면", "고로", "곧",
                   "공동으로", "과", "과연", "관계가", "관계없이", "관련이", "관하여", "관한",
                   "관해서는", "구", "구체적으로", "구토하다", "군", "군데", "권", "그", "그거",
                   "그것", "그곳", "그까짓", "그네", "그녀", "그놈", "그대", "그들","너희들",
                   "그때", "그래","아니","아","뭐","응","네","예","자","야","글쎄","참", "그래도",
                   "그래서", "그러나", "그러니", "그러니까", "그러다가", "그러면", "그러면서", 
                   "그러므로", "그러자", "그러한즉", "그런", "그런데", "그런즉", "그럼", 
                   "그럼에도", "그렇게", "그렇지","그렇지만", "그렇지않으면", "그루", "그리고",
                   "그리하여", "그만이다", "그분", "그서", "그에", "그위에", "그이", "그저", 
                   "그중에서", "그쪽", "그치지", "근", "근거로", "근거하여", "근데", "글쎄", 
                   "글쎄요", "기", "기대여", "기점으로", "기준으로", "기타", "김", "까닭으로",
                   "까악", "까지", "까지도", "꽈당", "끙끙", "끼익", "나", "나름", "나머지는",
                   "나위", "남들", "남짓", "내", "냥", "너", "너희", "네", "네놈", "네번째로", 
                   "넷", "넷째", "녀석", "년", "년대", "년도", "년부터", "논하지", "놀라다", 
                   "놈", "누가", "누구", "니", "다른", "다만", "다섯", "다소", "다수", "다시",
                   "다시말하면", "다음", "다음에", "다음으로", "단", "단지", "달", "달러", "답다",
                   "당신", "당장", "대", "대로", "대하면", "대하여", "대해", "대해서", "댕그",
                   "더구나", "더군다나", "더라도", "더불어", "더욱더", "더욱이", "더욱이는",
                   "데", "도", "도달하다", "도착하다", "동", "동시에", "동안", "동안","어때",
                   "되", "된바에야", "된이상", "두", "두번째로", "두세", "두어", "둘째", "둥",
                   "둥둥", "뒤따라", "뒤이어", "든간에", "들", "듯", "듯이", "등", "등등", 
                   "등지", "딩동", "따라", "따라서", "따르면", "따름", "따위", "따지지", "딱",
                   "딴", "때", "때가", "때문", "때문에", "또", "또는", "또한", "뚝뚝", "라",
                   "령", "로", "로부터", "로써", "륙", "를", "리", "마당", "마련", "마리", 
                   "마음대로", "마저", "마저도", "마치", "막론하고", "만", "만약", "만약에",
                   "만은", "만이", "만일", "만큼", "말", "말하자면", "말할것도", "매", "매번",
                   "맨", "명", "명으로", "몇", "몇몇", "모", "모금", "모두", "모든", "무렵",
                   "무슨", "무엇", "무엇때문에", "물론", "뭐", "뭣", "미치는", "미터", "및", 
                   "바", "바꾸어말하면", "바꾸어말하자면", "바꾸어서", "바꿔", "바람", "바로",
                   "바와같이", "바퀴", "박", "밖에", "반대로", "반드시", "발", "발짝", "버금",
                   "번", "벌", "법", "별", "보는데서", "보다더", "보드득", "보아", "본", 
                   "본대로", "봐", "봐라", "부", "부류의", "부터", "분", "불구하고", "불문하고",
                   "붕붕", "비걱거리다", "비교적", "비길수", "비로소", "비록", "비슷하다", 
                   "비추어", "비하면", "뻔", "뿐", "뿐만", "뿐만아니라", "뿐이다", "삐걱",
                   "삐걱거리다", "사", "살", "삼", "상대적으로", "새", "생각한대로", "서너", 
                   "석", "설", "설령", "설마", "설사", "섬", "세", "세기", "세번째로", "셈", 
                   "셋", "셋째", "소생", "소인", "솨", "쇤네", "수", "순", "쉿", "스무", "습니까",
                   "습니다", "승", "시", "시각", "시간", "시작하여", "시초에", "시키다", "식", 
                   "실로", "심지어", "씨", "아", "아","휴","아이구","아이쿠","아이고","어","나",
                   "우리","저희","따라", "아냐", "아니", "아니나다를가", "아니라", "아니라면", 
                   "아니면", "아니야", "아니었다면", "아래윗", "아무", "아무개", "아무거나",
                   "아무도", "아무런", "아아", "아야", "아울러", "아이", "아이고", "아이구", 
                   "아이야","아니", "아하", "아홉", "안", "않고", "않기", "않는다", "않다",
                   "않았다", "알", "알았어", "앗", "앞에서", "앞의것", "야", "약", "약간", 
                   "양", "양자", "얘", "어", "어기여차", "어느", "어느것", "어느곳", "어느때", 
                   "어느쪽", "어느해", "어디", "어디","그럼", "어때", "어떠한", "어떤", 
                   "어떤것", "어떤것들", "어떻게", "어떻해", "어머", "어이", "어째서", "어쨋든",
                   "어쩔수", "어찌", "어찌됏든", "어찌됏어", "어찌하든지", "어찌하여", "언제",
                   "언젠가", "얼마", "얼마간", "얼마나", "얼마든지", "얼마만큼", "얼마큼", 
                   "엉엉", "에", "에게", "에서", "에이", "엔", "여", "여기", "여느", "여덟",
                   "여러", "여러분", "여보", "여보세요", "여보시오", "여부", "여섯", "여전히",
                   "여지", "여차", "역시", "연관되다", "연이서", "영","일", "영차", "옆사람",
                   "예", "예를", "예컨대", "예하면", "옛", "오", "오랜", "오로지", "오르다",
                   "오자마자", "오직", "오호", "오히려", "온", "온갖", "올", "와", "와르르",
                   "와아", "왜", "왜냐하면", "왠", "외", "외에도", "요", "요만큼", "요만한",
                   "요만한걸", "요컨대", "우르르", "우리", "우리들", "우선", "운운", "원", "월",
                   "웬", "위", "위에서", "위하여", "윙윙", "육", "으로", "으로서", "으로써",
                   "을", "음", "응", "응당", "의", "의거하여", "의지하여", "의해", "의해되다",
                   "의해서", "이", "이거", "이것", "이곳", "이놈", "이때", "이라고", "이라면",
                   "이래", "이러이러하다", "이러한", "이런", "이런저런", "이럴정도로", "이렇게",
                   "이렇게되면", "이렇게말하자면", "이렇구나", "이로", "이르기까지", "이른바",
                   "이리하여", "이만큼", "이번", "이봐", "이상", "이어서", "이었다", "이와", 
                   "이와같다면", "이외에도", "이용하여", "이유만으로", "이젠", "이지만", "이쪽",
                   "이천구","하나","둘", "이천육", "이천칠", "이천팔", "인", "인젠", "일", 
                   "일것이다", "일곱", "일단", "일대", "일대","것", "일때", "일반적으로", 
                   "일지라도", "임마", "임에", "입각하여", "입장에서","위해서", "잇따라", "있다",
                   "있지만", "자", "자기", "자기집", "자네", "자네","장", "자마자", "자신", 
                   "잠깐", "잠시", "장", "저", "저것", "저것만큼", "저기", "저놈", "저런", "저쪽",
                   "저편", "저희", "적", "전", "전부", "전자", "전후", "점", "점에서", "정도에", 
                   "제", "제각기", "제외하고", "조", "조금", "조차", "조차도", "졸졸", "좀", 
                   "종합한것과", "좋아", "좍좍", "주", "주년", "주룩주룩", "주일", "주저하지",
                   "줄", "줄은", "중", "중에서", "중의", "즈음", "즈음하여", "즉", "즉시", "지",
                   "지경", "지난", "지든지", "지만", "지말고", "진짜로", "집", "짝", "쪽", 
                   "쪽으로", "쯤", "차", "차라리", "참", "참나", "채", "척", "첫", "첫번째로",
                   "첫째", "체", "쳇", "초", "총", "총적으로", "측", "치", "칠", "콸콸", "쾅쾅",
                   "쿵", "큰", "큰술", "킬로미터", "타", "타다", "타인", "탕탕", "터", "턱", 
                   "톤", "통", "통하여", "투", "툭", "퉤", "틈타", "팍", "판", "팔", "퍼센트",
                   "퍽", "펄렁", "편", "평", "푼", "하", "하게될것이다", "하게하다", "하겠는가",
                   "하고", "하고있었다", "하곤하였다", "하구나", "하기", "하기는한데", "하기만",
                   "하기보다는", "하기야", "하기에", "하긴", "하나", "하느니", "하는", "하는것도",
                   "하는것만", "하는것이", "하는바", "하더라도", "하도다", "하도록시키다",
                   "하도록하다", "하든지", "하려고하다", "하마터면", "하면", "하면된다", 
                   "하면서", "하물며", "하여금", "하여야", "하자마자", "하지", "하지마", "하지마라",
                   "하지만", "하하", "한", "한", "한다면", "한다면", "한데", "한두", "한두","한편",
                   "한마디", "한적이있다", "한켠으로는", "한편", "한항목", "할", "할때", "할만하다",
                   "할망정", "할뿐", "할수있다", "할수있어", "할줄알다", "할지라도", "할지언정",
                   "함께", "해도된다", "해도좋다", "해봐요", "해서는", "해야한다", "해요", "했어요",
                   "향하다", "향하여", "향해서", "허", "허걱", "허허", "헉", "헉헉", "헌", "헐떡헐떡",
                   "현", "형식으로", "호", "혹시", "혹은", "혼자", "회", "훨씬", "휘익","윙윙", "휴",
                   "흐흐", "흥", "힘입어", "가서", "같다", "같은", "같이","총적으로 것", "그러면",
                   "김에","겸사겸사", "까닭에", "낫다", "낼", "년도", "달려 대해", "되다", "되어",
                   "들면", "들자면", "듯하다", "따르는", "따름이다", "때문에", "많은 말하면",
                   "말하자면", "모른다", "몰라도", "몰랏다", "못하다", "미치다", "밖에", "반대로",
                   "방면으로", "보면","총적으로 보아", "불구하고", "사람들", "생각이다", 
                   "서술한바와같이", "수", "쓰여", "아니다", "아니라", "안다", "안된다", "않고", 
                   "않는다면", "않다", "않다면", "않도록", "않으면", "알겠는가", "없고","무릎쓰고",
                   "없다", "외에", "위하여", "위해서", "이르다", "이유는", "인하여", "있다", "정도의",
                   "줄 지경이다", "틀림없다", "편이", "하나", "하다", "하면", "한다면", "한하다", 
                   "할수록", "함으로써", "해도", "후", "힘이","추티마의","지난해", "전적인", "있습니다",
                   "시작된", "한다는", "인터뷰에서", "역할을", "이들은", "것이다", "가지고","중심으로",
                   "것입니다", "한겨례", "정글의", "포먼이", "정관용", "유달승", "우리는", "다양한",
                   "되었다","최대", "되는", "저는", "가진", "나는", "그가", "그의", "당시", "있는",
                   "있을", "있고", "년간", "보고", "많이", "있고", "더욱", "가장", "것을", "것이", 
                   "말했다","있다는", "은", "이날", "통해", "이들이", "상당히", "주간동아", "밝혔다",
                   "현지시간", "압둘라","자신의","영향을","하나님의","Copyrights","디지털뉴스이용규칙에",
                   "한국온라인신문협회","있다고","우리가","이시간","것으로", "재배포", "경향신문","보도했다",
                   "한겨레", "있었다","저자는", "이라는","때문이다","알려졌다","서울신문","경향닷컴","경향닷컴은",
                   "저작권을", "행사합니다", "전했다","연합뉴스", "YTN","동아일보","조선일보",
                   "중앙일보", "있었다", "패했고","좋을","김봉규", "기자", "무단전재","재배포금지","뉴시스", 
                   "앨라배마", "핫클릭", "지적했다", "것이라고", "주장했다", 
                   "몰렌베이크", "있도록", "요구할", "않습니다", "없습니다", "사실상", "시행하고", "상당한", 
                   "자료를","이병한", "스마트홈", "포켓몬고", "게스트", "강조했다", "것이라고", "이라며", 
                   "주장했다", "전망이다", "CNN", "대상으로", "비판했다", "고서치", "지적했다", "르펜은",
                   "합니다", "박수갈채", "이유로", "보두앵", "발리안은", "발리안에게", "십자군", "완전한",
                   "브런슨", "MBS", "강윤중", "파브나", "김영미", "굉장히", "인터뷰", "이동형", "사람들이", 
                   "판결을", "케이블스", "새로운", "한국일보", "구독하기", "메인에서", "네이버", "서울경제",
                   "전진영", "굉장히", "있는데", "있거든요", "박원곤", "것처럼", "인터뷰", "말씀드린", "김영미",
                   "오태훈", "김성완", "김혜민", "생각한다", "NYT", "SBS", "MBN","i","me","my","myself","we", "the","and",
                   "ㆍ", "●","있으며","뒤","보인다","지난달","그들은","한다고","없었다","중요한","그들의",
                   "오늘의","지역의","그동안","된","개의","on","것이라는","안에서","국민일보","GoodNews",
                   "때까지", "받았다", "이끄는", "벌였다", "결과를","등으로","비롯한","명이",
                   "돈키호테", "돈키호테는", "매일경제", "우리의", "이들을", "프레시안","저작권자",
                   "헤럴드경제","뉴스스탠드","헤럴드스탁마켓","모바일앱","NEWS1",
                   "무단복제","AFP","연구를", "거지요", "같아요", "같습니다", "그렇습니다", "겁니다", "그것이", "대해서는",
                   "위해서는", "말한다", "못했다", "필요가", "이들이", "실제로","정의길","오마이뉴스",
                   "이희수", "독서통","문제를","문제가","그것은","사건이", "사건을","있어요", "나온다",
                   "통해서", "자신이", "수많은","KBS","MBC","생각합니다","됩니다", "때문입니다","자신들의", "필요가",
                   "모르시는", "알리는", "무하마드", "지지하는", "인터랙티브뉴스", "나섰다", "모바일앱", "나스르시티", 
                   "박인규", "과정에서", "입장을", "핼퍼린", "특파원", "바다에", "다운받기", "아들리",
                   "없다는", "수많은", "이들의", "사람이", "모습을", "이들의", "그대로", "알려진", "하나의", "그들을", 
                   "현장영상", "영상을", "샌버나디노", "하와이", "서명했다",  "저신다", "온라인", "네이버에서", "과정에서")

# Tokenizing
Mining.TXT.token <- tokens(Mining.text.corpus, what = "word",
                           remove_numbers = TRUE, remove_punct = TRUE,
                           remove_symbols = TRUE, split_hyphens = TRUE,
                           remove_url=TRUE) %>%
  # Merge two words
  tokens_compound(list(c("middle", "class")))%>%
  tokens_remove(pattern = mystopwords,
                valuetype = 'fixed', padding = T)%>%
  tokens_remove(stopwords("english")) %>%
  tokens_tolower(keep_acronyms = FALSE) %>%
  tokens_wordstem("english")

# Remove e-mails
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+",
                                  valuetype = 'regex', padding = TRUE)
# remove web sites
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+\\.[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+",
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+\\.[a-zA-Z0-9-]+\\.[com]+",
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9-]+\\.[com]+",
                                  valuetype = 'regex', padding = TRUE)

# Merge two words
# Mining.TXT.token <- tokens_compound(Mining.TXT.token, dictionary(list(key1 = "United states",
#                                                                      key2 = "United States")))

##================================================
# Refine stopwords
#=================================================
#여기에 추가 불용어 추가
Stopword.add<- c ("main", "area", "locat")

mystopwords2 <- Stopword.add

# document feature matrix (dfm) with the  bag-of-words
Mining.TXT.token.dfm <- dfm(Mining.TXT.token,remove = mystopwords2)
# remove the term of frequency lesser than 5 times
Mining.TXT.token.dfm <- dfm_trim(Mining.TXT.token.dfm, min_termfreq = 5) %>%
  dfm_weight("boolean")


# Confirm data matrix
Mining.TXT.token.matrix <- as.matrix (Mining.TXT.token.dfm)
View (Mining.TXT.token.matrix)
dim (Mining.TXT.token.matrix)

# Term frequency (TF)
term.frequency <- function(row) {
  row / sum (row)
}

# Inverse Document Frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col >0))
  
  log10(corpus.size / doc.count)
}

# TF-IDF 계산
tf.idf <- function(x, idf) {
  x * idf
}

# 1차 step, normalize all documents via TF.
Mining.TXT.token.df <- apply(Mining.TXT.token.matrix, 1, term.frequency)
dim(Mining.TXT.token.df)
View(Mining.TXT.token.df)

# 2차 step,IDF vector를 연산
# for training data and for test data!
Mining.TXT.token.idf <- apply(Mining.TXT.token.matrix, 2, inverse.doc.freq)
str(Mining.TXT.token.idf)


# 최종적으로 corpus를 학습하기 위해 TF-IDF를 training
Mining.TXT.token.tfidf <-  apply(Mining.TXT.token.df, 2, tf.idf, idf = Mining.TXT.token.idf)
dim(Mining.TXT.token.tfidf)
View(Mining.TXT.token.tfidf)


# matrix로 전환
Mining.TXT.token.tfidf <- t(Mining.TXT.token.tfidf)
dim(Mining.TXT.token.tfidf)
View(Mining.TXT.token.tfidf)


# 미진한 부분 체크
incomplete.cases <- which(!complete.cases(Mining.TXT.token.tfidf))
Mining.TXT.token$text1[incomplete.cases]


# 미진한 부분 수정
Mining.TXT.token.tfidf[incomplete.cases,] <- rep(0.0, ncol(Mining.TXT.token.tfidf))
dim(Mining.TXT.token.tfidf)
sum (which(!complete.cases (Mining.TXT.token.tfidf)))


# Make a clean data frame using the same process as before.
Mining.TXT.token.df <- cbind(data.frame(Mining.TXT.token.tfidf))
names(Mining.TXT.token.df) <- make.names(names(Mining.TXT.token.df))

Mining.TXT.token.dfm<-dfm_select(Mining.TXT.token.dfm, min_nchar=3)


# =======================================
# 상대 빈도 분석
# =======================================
key <- textstat_keyness(Mining.TXT.token.dfm)
head(key, 10) %>% knitr::kable()


# =======================================
# 빈도 그래프 만들기
# =======================================


Mining.TXT.token.dfm.inaug <- textstat_frequency(Mining.TXT.token.dfm, n = 100)

# Sort by reverse frequency order
Mining.TXT.token.dfm.inaug$feature <- with(Mining.TXT.token.dfm.inaug, reorder(feature, -frequency))

ggplot(Mining.TXT.token.dfm.inaug, aes(x =feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

frequency.table <- Mining.TXT.token.dfm.inaug

# =======================================
# Co-occurrences Network 만들기
# =======================================

# 만들어진 dfm을 이용
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token.dfm)
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token.dfm, tri = FALSE) # Co-occurance Network 작성
feat <- names(topfeatures(Mining.TXT.textplot.Network, 50)) # 가장 빈도가 높은 Co-occurance를 선택
set.seed(100)
Mining.TXT.textplot.Network <- fcm_select(Mining.TXT.textplot.Network, pattern = feat, verbose = FALSE)

# CON 그리기

#textplot_network(Mining.TXT.textplot.Network,
#                 vertex_labelsize = 2.5 * rowSums(Mining.TXT.textplot.Network)/
#                   min(rowSums(Mining.TXT.textplot.Network)),
#                 min_freq = 0.90,
#                 vertex_labelfont = "Tahoma" , edge_size = 1)


textplot_network(Mining.TXT.textplot.Network,
                 min_freq = 0.95,
                 vertex_labelfont = "Tahoma", edge_size = 0.8)




# =======================================
# Topic Model 만들기
# =======================================

#Topic.Model <- textmodel_lda(Mining.TXT.token.dfm, k = 5)
#Topic.Model <- terms(Topic.Model, 6)
#head(topics(Topic.Model), 20)
#Topic.Model


tmod_lda <- textmodel_lda(Mining.TXT.token.dfm, k = 4)
terms(tmod_lda, 8)

# assign topic as a new document-level variable
Mining.TXT.token.dfm$topic <- topics(tmod_lda)

# cross-table of the topic frequency
table(Mining.TXT.token.dfm$topic)

Topic.Model <- data.frame(terms(tmod_lda, 8))

#최적의 Topic 산출 (이건 검증을 위한거)
result <- FindTopicsNumber(
  Mining.TXT.token.dfm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)



#====================================#
# Get specif line with targeted words#
#====================================#
Topic<- Topic.Model$topic4

Text.source <- Text.source %>%
  filter(str_detect(Text.source$text, Topic))
Text.source



# =======================================
# Sentiment Analycis
# =======================================


#====================================#
# Sentiment Analysis Process         #
#====================================#



# Text.source <- rename(Text.source,c("id"="docId"))
Text.source <- rename(Text.source,c("texts"="text"))
# Drop colums
Text.source <- subset(Text.source, select = -c(url,category,title) )
# Make it to the dataframe
# construct a object with sentiment measures
corpus <- sento_corpus(corpusdf = Text.source)

Corpus <- sento_corpus(Text.source)



class(Corpus)

tail(quanteda::docvars(Corpus))
cat("\n")
# list of lexicons:  "HENRY_en", "GI_en", "LM_en"
corpusSample <- corpus_sample(Corpus)
l <- sento_lexicons(list_lexicons[("LM_en")], list_valence_shifters[["en"]])
ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "day", lag = 2)
sentomeasures <- sento_measures(corpusSample, l, ctr)

#=====================================
# plot sentiment measures
#=====================================

plot(sentomeasures, group = "lexicons")
View(sentomeasures)
Article.sentiment <-data.frame(sentomeasures$sentiment)
Article.sentiment[is.na(sentomeasures$sentiment)] <- 0
mean(Article.sentiment$LM_en..dummyFeature)


