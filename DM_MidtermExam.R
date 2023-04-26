#####################################################
## DataMining MidtermExam                          ##
## Meda                                            ##
## 2023-04-26                                      ##
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

#####################################################
## Q1. Step 1                                      ##
#####################################################

# url
url <- "https://www.fifa.com/fifa-world-ranking/men?dateId=id13974"

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

df <- data.frame()
for (i in 1:num_options) {
  options <- remDr$findElements(using = 'css selector', value = '.ff-dropdown_dropupContentButton__WC4zi')
  dropdown <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/div/div[2]/main/section[1]/div/div/div[1]/div[2]/div')
  dropdown$clickElement()
  options[[i]]$clickElement()
  # 웹 페이지에서 XPath를 사용하여 테이블 가져오기
  table <- remDr$findElement(using = "class", value = "table_rankingTable__7gmVl")
  # 테이블 HTML 가져오기
  table_html <- table$getElementAttribute("outerHTML")[[1]]
  
  # html_node()와 html_table()을 이용하여 테이블 가져오기
  table_data <- read_html(table_html) %>% 
    html_node("table") %>%
    html_table(header = TRUE)%>% 
    select(-c(2,7,8)) %>% 
    mutate(Date = texts[[i]])
  df <- bind_rows(df, table_data)
}

View(df)

#####################################################
## Q1. Step 2                                      ##
#####################################################

FIFARank <- as_tibble(df)
FIFARank_In_30 <- FIFARank %>% 
  filter(RK <= 30) %>% 
  mutate(Team = substr(Team, 1, nchar(Team) - 3))%>% 
  select(Team) %>% 
  count(Team, sort = TRUE) %>% 
  arrange(desc(n))

colnames(FIFARank_In_30)[2] <- "Count"

dim(FIFARank_In_30)
View(FIFARank_In_30)

png("Rank.png", width=3*nrow(FIFARank_In_30),height=850*ncol(FIFARank_In_30))
p <- tableGrob(FIFARank_In_30)
grid.arrange(top = "Ranking 30 Count", p)
dev.off()

#[How many countries are there in total in the organized table?]
paste0(dim(FIFARank_In_30)[1],"팀")
