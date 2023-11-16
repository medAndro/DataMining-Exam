#####################################################
## DataMining MidtermExam                          ##
## Meda                                            ##
## 2023-05-05                                      ##
#####################################################

#레포지토리 설정
setRepositories(ind = 1:7)

#작업디렉토리 설정
WORK_DIR <- "C:/Users/Meda/Documents/DataMining-Exam"
setwd(WORK_DIR)
getwd()

# 필요 패키지 로딩 
library(RSelenium)
library(rvest)
library(netstat)
library(dplyr)
library(tibble)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(gganimate)
library(jsonlite)
library(tidyverse)
library(httr)
library(data.table)
library(caret)
library(caTools)
library(nnet)
library(gbm)

# 난수 시드값 지정
set.seed(1234)

#####################################################
## Q1. Step 1                                      ##
#####################################################

# url
url <- "https://www.fifa.com/fifa-world-ranking/men?dateId=id13974"

# PC에 최신버전의 firefox 브라우저 사전 설치 필요
rD <- rsDriver(browser="firefox", port=netstat::free_port(), chromever = NULL)
remDr <- rD[["client"]]
remDr$navigate(url)

#로드까지 1초 대기
Sys.sleep(1)

#개인정보 수집 동의 버튼 클릭
privacy_button <- remDr$findElement(using = 'id', value = 'onetrust-accept-btn-handler')
privacy_button$clickElement()

options <- remDr$findElements(using = 'css selector', value = '.ff-dropdown_dropupContentButton__WC4zi')
dropdown <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/div/div[2]/main/section[1]/div/div/div[1]/div[2]/div')
dropdown$clickElement()
num_options <- length(options)
num_options
texts <- lapply(options, function(opt) opt$getElementText())
texts
#새로고침후 1초 대기
remDr$navigate(url)
Sys.sleep(1)

df_Rselenium <- data.frame()

for (i in 1:num_options) {
  options <- remDr$findElements(using = 'css selector', value = '.ff-dropdown_dropupContentButton__WC4zi')
  dropdown <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/div/div[2]/main/section[1]/div/div/div[1]/div[2]/div')
  dropdown$clickElement()
  options[[i]]$clickElement()
  
  # 클릭후 0.3초 대기
  Sys.sleep(0.3)
  # 300만큼 스크롤
  remDr$executeScript("window.scrollTo(0, 300);")
  # 웹 페이지에서 class를 사용하여 테이블 가져오기
  table <- remDr$findElement(using = "class", value = "table_rankingTable__7gmVl")
  # 테이블 HTML 가져오기
  table_html <- table$getElementAttribute("outerHTML")[[1]]
  
  # html_node()와 html_table()을 이용하여 테이블 가져오기
  table_data <- read_html(table_html) %>% 
    html_node("table") %>%
    html_table(header = TRUE)%>% 
    select(-c(2,7,8)) %>% 
    mutate(Date = texts[[i]])
  df_Rselenium <- bind_rows(df_Rselenium, table_data)
}

FIFARank <- as_tibble(df_Rselenium)
FIFARank <- FIFARank %>% 
  mutate(Team = substr(Team, 1, nchar(Team) - 3)) %>% 
  mutate(Date = dmy(unlist(Date)))
features <- c("RK", "Team", "Total_Points", "Previous_Points", "+/-", "Date")

# 컬럼명 지정
colnames(FIFARank) <-features
FIFARank



#####################################################
## Q1. Step 2                                      ##
#####################################################

FIFARank_In_30 <- FIFARank %>% 
  filter(RK <= 30) %>%
  select(Team) %>% 
  count(Team, sort = TRUE) %>% 
  arrange(desc(n))

colnames(FIFARank_In_30)[2] <- "Count"

dim(FIFARank_In_30)
View(FIFARank_In_30)

png("Rank_FIFA.png", width=110*ncol(FIFARank_In_30),height=22*nrow(FIFARank_In_30))
p <- tableGrob(FIFARank_In_30)
grid.arrange(top = "Ranking 30 Count", p)
dev.off()

#[How many countries are there in total in the organized table?]
paste0(dim(FIFARank_In_30)[1],"국가")



#####################################################
## Q1. Step 3                                      ##
#####################################################

FIFARank_In_20 <- FIFARank %>% 
  filter(RK <= 20)

FIFARank_In_20

staticplot = ggplot(FIFARank_In_20, aes(RK, group = Team, 
                                        fill = as.factor(Team), color = as.factor(Team))) +
  geom_tile(aes(y = Total_Points/2,
                height = Total_Points,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Team, " ")), vjust = 0.2, hjust = 1, size = 5, fontface = "bold") +
  geom_text(aes(y = Total_Points, label = as.character(Total_Points), hjust = 0),
            size = 5, fontface = "bold")+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,3, 2, 7, "cm"))

staticplot

anim = staticplot + transition_states(Date, transition_length = 2, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = "{closest_state} 피파랭킹",  
       subtitle  =  "상위 20위")


animate(anim, 4860, fps = 15,  width = 1100, height = 1000, 
        renderer = gifski_renderer("Q1_Step3.gif"))


#####################################################
## Q2.                                             ##
#####################################################

url <- "https://www.fifa.com/fifa-world-ranking/men?dateId=id13974"

# 페이지 가져오기
page <- read_html(url)

# id가 "__NEXT_DATA__"인 태그 가져오기
script_content <- page %>%
  html_nodes("#__NEXT_DATA__") %>%
  html_text()

# JSON 파싱 (props>pageProps>pageData>ranking>dates만 추출)
date_list <- fromJSON(script_content) %>%
  `[[`("props") %>%
  `[[`("pageProps") %>%
  `[[`("pageData") %>%
  `[[`("ranking") %>%
  `[[`("dates")
date_list <- as_tibble(date_list)
date_list <- date_list %>% 
  mutate(Date = dmy(text)) %>% 
  select(-text)

date_list
#열 이름 작성
features <- c("RK", "Team", "Total_Points", "Previous_Points")

#빈 데이터프레임 생성
df_Merged <- data.frame()

for (id in date_list$id) {
  print(id)
  json_url <- paste0("https://www.fifa.com/api/ranking-overview?locale=en&dateId=", id)
  df <- tibble(jsonlite::fromJSON(json_url)[[1]])
  #중첩된 차원 펼치기
  df_wide <- df %>%
    unnest_wider(rankingItem, names_sep = "_")
  
  #4개의 열 선택
  df_sub <- df_wide[, c(1, 3, 4, 9)]
  #열 이름 지정
  colnames(df_sub) <-features
  
  date_val <- date_list$Date[date_list$id == id]
  #+/-와 date열 만들기
  df_cleaned <- df_sub %>%
    mutate("+/-" = Total_Points - Previous_Points) %>% 
    mutate(Date = date_val)
  
  # 데이터프레임 합치기
  df_Merged <- rbind(df_Merged, df_cleaned)
  
  print(head(df_cleaned))
}

df_Merged
View(df_Merged)


# Q1-step1과 동일한 형식의 데이터를 non-RSelenium way로 획득하였으므로
FIFARank<-df_Merged
# 를 진행한뒤 Q1의 step2, step3부분의 코드를 그대로 돌리면
# non-RSelenium way로 step2, step3까지 가능한 셈이므로
# Q2에서의 Q1의 step2, step3에 해당하는 과정은 생략합니다.


#####################################################
## Q3. Step 1                                      ##
#####################################################

# 요청 헤더 설정
headers = c(
  `authority` = "footballapi.pulselive.com",
  `accept` = "*/*",
  `accept-language` = "ko-KR,ko;q=0.9,en-US;q=0.8,en;q=0.7,ja;q=0.6",
  `account` = "premierleague",
  `cache-control` = "no-cache",
  `content-type` = "application/x-www-form-urlencoded; charset=UTF-8",
  `origin` = "https://www.premierleague.com",
  `pragma` = "no-cache",
  `referer` = "https://www.premierleague.com/",
  `sec-ch-ua` = '"Chromium";v="112", "Google Chrome";v="112", "Not:A-Brand";v="99"',
  `sec-ch-ua-mobile` = "?0",
  `sec-ch-ua-platform` = '"Windows"',
  `sec-fetch-dest` = "empty",
  `sec-fetch-mode` = "cors",
  `sec-fetch-site` = "cross-site",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36"
)

#파라미터 설정
params = list(
  `page` = "0",
  `pageSize` = "100",
  `startDate` = "1992-01-01"
)

# httr::GET으로 Seasons id값 가져오기
res <- httr::GET(url = "https://footballapi.pulselive.com/football/competitions/1/compseasons", 
                 httr::add_headers(.headers=headers), query = params)

date_compSeason <- as_tibble(fromJSON(content(res, "text"))[[2]])
date_compSeason

#빈 데이터프레임 생성
PremierleagueRank <- data.frame()

for (id in date_compSeason$id) {
  print(id)
  
  params = list(
    `compSeasons` = as.character(id),
    `altIds` = 'true',
    `detail` = '2',
    `FOOTBALL_COMPETITION` = '1'
  )
  
  res <- httr::GET(url = 'https://footballapi.pulselive.com/football/standings', 
                   httr::add_headers(.headers=headers), query = params)
  
  # JSON 파싱
  league_json <- fromJSON(content(res, "text"))
  
  entries_df <-as_tibble(league_json[4][[1]][2]$entries[[1]])
  
  Season_val <- date_compSeason$label[date_compSeason$id == id]
  
  df <- tibble(Position = entries_df$position, 
                   Club = entries_df$team$name,
                 Played = entries_df$overall$played,
                    Won = entries_df$overall$won,
                  Drawn = entries_df$overall$drawn,
                   Lost = entries_df$overall$lost,
                     GF = entries_df$overall$goalsFor,
                     GA = entries_df$overall$goalsAgainst,
                     GD = entries_df$overall$goalsDifference,
                 Points = entries_df$overall$points) %>% 
          mutate(Season = Season_val)
  print(head(df))
  
  PremierleagueRank <- rbind(PremierleagueRank, df)
}

View(PremierleagueRank)



#####################################################
## Q3. Step 2                                      ##
#####################################################

Premierleague_In_4 <- PremierleagueRank %>% 
  filter(Position <= 4) %>%
  select(Club) %>% 
  count(Club, sort = TRUE) %>% 
  arrange(desc(n))

colnames(Premierleague_In_4)[2] <- "Count"

dim(Premierleague_In_4)
View(Premierleague_In_4)

png("Rank_PL.png", width=110*ncol(Premierleague_In_4),height=25*nrow(Premierleague_In_4))
p <- tableGrob(Premierleague_In_4)
grid.arrange(top = "Ranking 4 Count", p)
dev.off()

#[How many pro clubs are there in total in the organized table?]
paste0(dim(Premierleague_In_4)[1],"클럽")


#####################################################
## Q3. Step 3                                      ##
#####################################################

PL_In_20 <- PremierleagueRank %>% 
  filter(Position <= 20)

PL_In_20

staticplot = ggplot(PL_In_20, aes(Position, group = Club, 
                                        fill = as.factor(Club), color = as.factor(Club))) +
  geom_tile(aes(y = Points/2,
                height = Points,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Club, " ")), vjust = 0.2, hjust = 1, size = 5, fontface = "bold") +
  geom_text(aes(y = Points, label = as.character(Points), hjust = 0),
            size = 5, fontface = "bold")+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,3, 2, 7, "cm"))

staticplot

anim = staticplot + transition_states(Season, transition_length = 2, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = "{closest_state}시즌 프리미어리그 랭킹",  
       subtitle  =  "상위 20위")


animate(anim, 450, fps = 15,  width = 1100, height = 1000, 
        renderer = gifski_renderer("Q3_Step3.gif"))



#####################################################
## Q4. Step 1                                      ##
#####################################################

#빈 리스트
dataList <- list()

# CSV 파일 경로 설정
csv_path <- "./Q4_Data"

# 파일 목록 가져오기
csv_files <- list.files(csv_path, pattern = "\\.csv$", full.names = TRUE)

# 각 파일에서 NA가 포함된 열 파악하기
for (file in csv_files) {
  # fread를 사용하여 CSV 파일 불러오기
  data <- fread(file)
  # NA가 포함된 열 파악하기
  na_cols <- which(colSums(is.na(data)) > 0)
  # 결과 출력하기
  if (length(na_cols) > 0) {
    cat("File:", file, "\n")
    cat("NA가", paste(na_cols, collapse = ", "), "번째 열에 있습니다\n")
    data <- subset(data, select = -na_cols)
    cat("NA가 발견된 열을 제거하였습니다\n\n")
  } else {
    cat("File:", file, "\n")
    cat("모든 컬럼에 NA가 없습니다.\n\n")
  }
  dataList <- append(dataList, list(data))
}

headerList <- lapply(dataList, colnames)
headerList

# headerList 리스트의 모든 원소 간 교집합, 즉 이름이 겹치는 헤더만 추출
headers <- Reduce(intersect, headerList)

# headers의 길이만큼의 컬럼수를 가지는 빈 데이터프레임을 만들고 컬럼명을 headers로 지정
df_Q4_merge <- data.frame(matrix(ncol = length(headers), nrow = 0))
colnames(df_Q4_merge) <- headers

# 각 csv 데이터에서 headers와 일치하는 컬럼들만 df_Q4_merge에 rbind로 결합
for (i in 1:5) {
  df_Q4_merge<- rbind(df_Q4_merge, dataList[[i]][, ..headers])
}

str(df_Q4_merge)
sum(is.na(df_Q4_merge))
dim(df_Q4_merge)


#####################################################
## Q4 step 2                                       ##
#####################################################

# train/test분할해주는 함수 정의
create_train_test <- function(df, size = 0.8, train = TRUE) {
  n_row <- nrow(df)
  total_row <- size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (df[train_sample, ])
  } else {
    return (df[-train_sample, ])
  }
}


# 손글씨 데이터 라벨링하기
isHandPrint <- factor(c(rep("Other", nrow(dataList[[1]])),
                        rep('Other', nrow(dataList[[2]])),
                        rep('HandPrint', nrow(dataList[[3]])),
                        rep('Other', nrow(dataList[[4]])),
                        rep('Other', nrow(dataList[[5]]))))

df_HandPrint <- data.frame(cbind(df_Q4_merge, isHandPrint))

# 행 무작위 섞기
randomIdx <- sample(1:nrow(df_HandPrint))
randomIdx
df_HandPrint <- df_HandPrint[randomIdx,]
dim(df_HandPrint)

# train Set / test Set 8:2 분할
TrainSet_Q4_2 <- create_train_test(df_HandPrint, 0.8, train = TRUE)
TestSet_Q4_2 <- create_train_test(df_HandPrint, 0.8, train = FALSE)


# 무작위로 섞은 데이터프레임을 8:2로 분할한 데이터프레임 확인
head(rownames(TrainSet_Q4_2))
dim(TrainSet_Q4_2)
head(rownames(TestSet_Q4_2))
dim(TestSet_Q4_2)

# 10Fold CV trainControl
# verboseIter를 TRUE로 설정하면 매 fold마다 로그를 출력함
trainControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)



#### 모델 학습 ####
# Train Set에 대해 10fold Cross Validation을 사용하여
# 11번째열부터 나머지열(r0c0 ~ r19c19)의 컬럼을 사용해 isHandPrint를 판별하는 모델 train

# LDA 모델 학습 (선형 모델)
model_lda_step2 <- train(isHandPrint~., 
                         data = TrainSet_Q4_2[,11:ncol(TrainSet_Q4_2)], 
                         method = "lda",
                         trControl = trainControl)
# 모델을 이용한 Test set 예측
predict_lda_step2 <- predict(model_lda_step2, newdata = TestSet_Q4_2[,11:ncol(TestSet_Q4_2)])
# Test set 예측 정확도 계산
accuracy_lda_step2 <- mean(predict_lda_step2 == TestSet_Q4_2$isHandPrint)
print(accuracy_lda_step2)



# QDA 모델 학습 (선형 모델)
model_qda_step2 <- train(isHandPrint ~ .,
                         data = TrainSet_Q4_2[, 11:ncol(TrainSet_Q4_2)],
                         method = "qda",
                         trControl = trainControl)
# 모델을 이용한 Test set 예측
predict_qda_step2 <- predict(model_qda_step2, newdata = TestSet_Q4_2[,11:ncol(TestSet_Q4_2)])
# Test set 예측 정확도 계산
accuracy_qda_step2 <- mean(predict_qda_step2 == TestSet_Q4_2$isHandPrint)
print(accuracy_qda_step2)



# mlpML모델 학습 (다층 퍼셉트론)
model_mlpML_step2 <- train(isHandPrint ~ ., 
                           data = TrainSet_Q4_2[,11:ncol(TrainSet_Q4_2)], 
                           method = "mlpML", 
                           trControl = trainControl,
                           preProcess = "range", #스케일링으로 전처리
                           tuneLength = 20, #하이퍼파라미터 튜닝 조합의 수
                           verboseIter = 2) # fold마다 출력할 정보의 양
# 모델을 이용한 Test set 예측
predict_mlpML_step2 <- predict(model_mlpML_step2, newdata = TestSet_Q4_2[,11:ncol(TestSet_Q4_2)])
# Test set 예측 정확도 계산
accuracy_mlpML_step2 <- mean(predict_mlpML_step2 == TestSet_Q4_2$isHandPrint)
print(accuracy_mlpML_step2)



# Gradient Boosting 모델 학습 (앙상블)
model_gbm_step2 <- train(isHandPrint ~ .,
                         data = TrainSet_Q4_2[, 11:ncol(TrainSet_Q4_2)],
                         method = "gbm",
                         trControl = trainControl,
                         verbose = FALSE) #학습 과정에서 진행 상황을 출력하지 않음
# 모델을 이용한 Test set 예측
predict_gbm_step2 <- predict(model_gbm_step2, newdata = TestSet_Q4_2[,11:ncol(TestSet_Q4_2)])
# Test set 예측 정확도 계산
accuracy_gbm_step2 <- mean(predict_gbm_step2 == TestSet_Q4_2$isHandPrint)
print(accuracy_gbm_step2)



# 랜덤 포레스트 모델 학습 (앙상블)
model_rf_step2 <- train(isHandPrint ~ .,
                        data = TrainSet_Q4_2[, 11:ncol(TrainSet_Q4_2)], 
                        method = "rf",
                        trControl = trainControl)
# 모델을 이용한 Test set 예측
predict_rf_step2 <- predict(model_rf_step2, newdata = TestSet_Q4_2[,11:ncol(TestSet_Q4_2)])
# Test set 예측 정확도 계산
accuracy_rf_step2 <- mean(predict_rf_step2 == TestSet_Q4_2$isHandPrint)
print(accuracy_rf_step2)


# adaboost 모델 학습 (앙상블)
model_AdaBoost_step2 <- train(isHandPrint ~ .,
                              data = TrainSet_Q4_2[, 11:ncol(TrainSet_Q4_2)],
                              method = "AdaBoost.M1",
                              trControl = trainControl)
# 모델을 이용한 Test set 예측
predict_AdaBoost_step2 <- predict(model_AdaBoost_step2, newdata = TestSet_Q4_2[,11:ncol(TestSet_Q4_2)])
# Test set 예측 정확도 계산
accuracy_AdaBoost_step2 <- mean(predict_AdaBoost_step2 == TestSet_Q4_2$isHandPrint)
print(accuracy_AdaBoost_step2)


#####################################################
## Q4 step 3                                       ##
#####################################################

# 폰트 이름 데이터 라벨링하기
fontNames <- factor(c(rep("Arial", nrow(dataList[[1]])),
                      rep('CreditCard', nrow(dataList[[2]])),
                      rep('HandPrint', nrow(dataList[[3]])),
                      rep('Italic', nrow(dataList[[4]])),
                      rep('Times', nrow(dataList[[5]]))))

df_FontNames <- data.frame(cbind(df_Q4_merge, fontNames))

# 행 무작위 섞기
randomIdx <- sample(1:nrow(df_FontNames))
randomIdx
df_FontNames <- df_FontNames[randomIdx,]
dim(df_FontNames)

# train Set / test Set 8:2 분할
TrainSet_Q4_3 <- create_train_test(df_FontNames, 0.8, train = TRUE)
TestSet_Q4_3 <- create_train_test(df_FontNames, 0.8, train = FALSE)


# 무작위로 섞은 데이터프레임을 8:2로 분할한 데이터프레임 확인
head(rownames(TrainSet_Q4_3))
dim(TrainSet_Q4_3)
head(rownames(TestSet_Q4_3))
dim(TestSet_Q4_3)


# 10Fold CV trainControl
# verboseIter를 TRUE로 설정하면 매 fold마다 로그를 출력함
trainControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)


#### 모델 학습 ####
# Train Set에 대해 10fold Cross Validation을 사용하여
# 11번째열부터 나머지열(r0c0 ~ r19c19)의 컬럼을 사용해 fontNames를 판별하는 모델 train

# LDA 모델 학습 (선형 모델)
model_lda_step3 <- train(fontNames~., 
                         data = TrainSet_Q4_3[,11:ncol(TrainSet_Q4_3)], 
                         method = "lda",
                         trControl = trainControl)
# 모델을 이용한 Test set 예측
predict_lda_step3 <- predict(model_lda_step3, newdata = TestSet_Q4_3[,11:ncol(TestSet_Q4_3)])
# Test set 예측 정확도 계산
accuracy_lda_step3 <- mean(predict_lda_step3 == TestSet_Q4_3$fontNames)
print(accuracy_lda_step3)


# QDA 모델 학습 (선형 모델)
model_qda_step3 <- train(fontNames~., 
                         data = TrainSet_Q4_3[,11:ncol(TrainSet_Q4_3)], 
                         method = "qda",
                         trControl = trainControl)
# 모델을 이용한 Test set 예측
predict_qda_step3 <- predict(model_qda_step3, newdata = TestSet_Q4_3[,11:ncol(TestSet_Q4_3)])
# Test set 예측 정확도 계산
accuracy_qda_step3 <- mean(predict_qda_step3 == TestSet_Q4_3$fontNames)
print(accuracy_qda_step3)


# mlpML모델 학습 (다층 퍼셉트론)
model_mlpML_step3 <- train(fontNames ~ ., 
                           data = TrainSet_Q4_3[,11:ncol(TrainSet_Q4_3)], 
                           method = "mlpML", 
                           trControl = trainControl,
                           preProcess = "range", #스케일링으로 전처리
                           tuneLength = 20, #하이퍼파라미터 튜닝 조합의 수
                           verboseIter = 2) # fold마다 출력할 정보의 양
# 모델을 이용한 Test set 예측
predict_mlpML_step3 <- predict(model_mlpML_step3, newdata = TestSet_Q4_3[,11:ncol(TestSet_Q4_3)])
# Test set 예측 정확도 계산
accuracy_mlpML_step3 <- mean(predict_mlpML_step3 == TestSet_Q4_3$fontNames)
print(accuracy_mlpML_step3)


# Gradient Boosting 모델 학습 (앙상블)
model_gbm_step3 <- train(fontNames ~ .,
                         data = TrainSet_Q4_3[, 11:ncol(TrainSet_Q4_3)],
                         method = "gbm",
                         trControl = trainControl,
                         verbose = FALSE) #학습 과정에서 진행 상황을 출력하지 않음
# 모델을 이용한 Test set 예측
predict_gbm_step3 <- predict(model_gbm_step3, newdata = TestSet_Q4_3[,11:ncol(TestSet_Q4_3)])
# Test set 예측 정확도 계산
accuracy_gbm_step3 <- mean(predict_gbm_step3 == TestSet_Q4_3$fontNames)
print(accuracy_gbm_step3)


# 랜덤 포레스트 모델 학습 (앙상블)
model_rf_step3 <- train(fontNames ~ .,
                        data = TrainSet_Q4_3[, 11:ncol(TrainSet_Q4_3)], 
                        method = "rf",
                        trControl = trainControl)
# 모델을 이용한 Test set 예측
predict_rf_step3 <- predict(model_rf_step3, newdata = TestSet_Q4_3[,11:ncol(TestSet_Q4_3)])
# Test set 예측 정확도 계산
accuracy_rf_step3 <- mean(predict_rf_step3 == TestSet_Q4_3$fontNames)
print(accuracy_rf_step3)


# adaboost 모델 학습 (앙상블)
model_AdaBoost_step3 <- train(fontNames ~ .,
                              data = TrainSet_Q4_3[, 11:ncol(TrainSet_Q4_3)],
                              method = "AdaBoost.M1",
                              trControl = trainControl)
# 모델을 이용한 Test set 예측
predict_AdaBoost_step3 <- predict(model_AdaBoost_step3, newdata = TestSet_Q4_3[,11:ncol(TestSet_Q4_3)])
# Test set 예측 정확도 계산
accuracy_AdaBoost_step3 <- mean(predict_AdaBoost_step3 == TestSet_Q4_3$fontNames)
print(accuracy_AdaBoost_step3)


########################################################
## Q4-step2의 정확도 예측값들
cat("Q4-step2 lda모델 정확도: ", accuracy_lda_step2)
cat("Q4-step2 qda모델 정확도: ", accuracy_qda_step2)
cat("Q4-step2 mlpML모델 정확도: ", accuracy_mlpML_step2)
cat("Q4-step2 gbm모델 정확도: ", accuracy_gbm_step2)
cat("Q4-step2 rf모델 정확도: ", accuracy_rf_step2)
cat("Q4-step2 adaboost모델 정확도: ", accuracy_AdaBoost_step2)

########################################################
## Q4-step3의 정확도 예측값들
cat("Q4-step3 lda모델 정확도: ", accuracy_lda_step3)
cat("Q4-step3 qda모델 정확도: ", accuracy_qda_step3)
cat("Q4-step3 mlpML모델 정확도: ", accuracy_mlpML_step3)
cat("Q4-step3 gbm모델 정확도: ", accuracy_gbm_step3)
cat("Q4-step3 rf모델 정확도: ", accuracy_rf_step3)
cat("Q4-step3 adaboost모델 정확도: ", accuracy_AdaBoost_step3)

## 최고의 정확도를 가지는 모델을 RData로 저장
saveRDS(model_AdaBoost_step2, file = "Q4-1_[Meda].RData")
saveRDS(model_rf_step3, file = "Q4-2_[Meda].RData")

model_x <- readRDS("Q4-1_[Meda].RData")
model_y <- readRDS("Q4-2_[Meda].RData")

predict_4_2 <- predict(model_y, newdata = TestSet_Q4_3[,11:ncol(TestSet_Q4_3)])
# Test set 예측 정확도 계산
accuracy_4_2 <- mean(predict_4_2 == TestSet_Q4_3$fontNames)
print(accuracy_4_2)

