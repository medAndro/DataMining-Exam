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