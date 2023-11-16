# DataMining-Exam

### 중간고사 Midterm Exam v2


## [Q1] Let's do data mining on World Cup men's soccer rankings using R-programming from the following web-site (10 Points) by using RSelenium:
https://www.fifa.com/fifa-world-ranking/men?dateId=id13974

![그림1](https://user-images.githubusercontent.com/88672474/234570099-5d449537-5255-4b6c-ae04-427c8a570c33.png)
### [Step 1] Let's collect data for all top 90 countries for all date intervals (31 Dec 1992 to 6 Apr 2023) in the date selector box on the web page.
(해당 페이지의 날짜 선택박스에 있는 모든 날짜 구간 (31 Dec 1992 to 6 Apr 2023)에 해당하는 상위 90위의 나라의 정보를 모두 수집하십시오. *웹크롤링 이외의 방법으로 자료 수집시 해당 스텝의 점수는 0점 입니다.)

### [Step 2] From the collected data, sort by country the total number of times the top 30 ranked in each of those time table, respectively, then sort in descending order.
(수집된 자료로 부터 해당 타임 테이블 각각에서 상위 30위 안에 들었던 총 횟수를 나라별로 정리한 뒤 내림차순 정렬하십시오.)
### [Sorted table]
![Rank_FIFA](https://user-images.githubusercontent.com/88672474/236418776-ef43862d-c803-411d-ac48-15190be52bdd.png)

### [How many countries are there in total in the organized table?]
78국가 


### [Step 3] Interactively visualize the top 20 countries from the collected data through a visualization method that can be expressed by over time. Please compress and submit the visualized file along with this worksheet. When sorting rankings, be sure to sort by total point.
(주어진 자료로 부터 상위 20개국을 타임 테이블별로 표현할 수 있는 시각화 방법을 통해 인터렉티브 하게 표현하십시오. 시각화 된 파일은 이 문제지와 함께 압축하여 제출하십시오. 순위를 정렬할 때는 반드시 Total point 순으로 정렬해야 합니다.)

https://user-images.githubusercontent.com/88672474/235953922-96bc687a-2971-4270-b950-535fd2dff9a2.mp4


## [Q2] Please resolve the same problem as [Q1], but in a non-RSelenium way. (20 Points) (Hint: API and json crawling usually possible):
<Description of the code part that is different from [Q1]>

Q1에서는 R셀레니움의 rsDriver를 통해 firefox 브라우저를 실행하고 url에 접근하여 드롭다운 메뉴의 각각의 html 요소를 remDr$findElement를 통해 찾아 하나하나 클릭해가며 동적으로 변화하는 페이지에서 read_html(), html_node(), html_table()등을 이용하여 html의 동적으로 생성된 table태그에서 데이터를 얻어온 반면 Q2에서는 우선 rvest를 사용해 정적으로 페이지를 크롤링하여 id가 __NEXT_DATA__인 script태그에 존재하는 json 데이터를 fromJSON()을 통해 파싱하여 개발자도구 네트워크탭에서 획득한 api 주소의 dateId 파라미터값으로 사용되어질 값들을 전부을 얻어온 뒤 api주소에 dateId값을 넣고 for문을 돌며 랭킹을 담고있는 json데이터를 차례대로 얻어왔다.


## [Q3] Let's do data mining on World Cup men's soccer rankings using R-programming from the following web-site (20 Points):
![image](https://github.com/medAndro/DataMining-Exam/assets/88672474/00875cb6-c98d-4a4c-901d-68fb1857edf8)

### [Step 1] Let's collect data for all 20 teams for all date intervals (1992/93 to 2022/23) in the date selector box on the web page. *0 points when collecting data by methods other than web crawling.
(해당 페이지의 날짜 선택박스에 있는 모든 날짜 구간 (1992/93 to 2022/23)에 해당하는 모든 20팀의 정보를 모두 수집하십시오. *웹크롤링 이외의 방법으로 자료 수집시 해당 스텝의 점수는 0점입니다.)

### [Step 2] From the collected data, sort by country the total number of times the top 4 ranked in each season, respectively, then sort in descending order.
(수집된 자료로 부터 시즌 별로 상위 4위 안에 들었던 팀을 정리한 뒤 몇 번이나 팀이 관측되었는지에 대해 내림차순 정렬하십시오.)

### [Sorted table]
![image](https://github.com/medAndro/DataMining-Exam/assets/88672474/2a47ae81-47a7-40bd-9884-e1cc12d7fc6d)

### [How many pro clubs are there in total in the organized table?]
14클럽


### [Step 3] Interactively visualize the top 20 clubs from the collected data through a visualization method that can be expressed by over time. Please compress and submit the visualized file along with this worksheet. When sorting the ranking, be sure to use the Point random variable.
(주어진 자료로부터 상위 20개국을 타임 테이블별로 표현할 수 있는 시각화 방법을 통해 인터렉티브 하게 표현하십시오. 시각화 된 파일은 이 문제지와 함께 압축하여 제출하십시오. 순위 정렬 시 반드시 승점정보를 이용하여 정렬하십시오)

![image](https://github.com/medAndro/DataMining-Exam/assets/88672474/0537683a-badc-455d-8229-4c9491ef5380)

https://github.com/medAndro/DataMining-Exam/assets/88672474/2f85f4d5-4186-485e-a75f-b37aec142cbb



## [Q4] As those of you already living in the age of artificial intelligence know, pattern recognition for strings such as ChatGPT occupies a very important place in artificial intelligence. Among them, the technology to recognize various fonts as the same character is also a very important part for the advancement of real-time translation technology in the future. Let's actively utilize the skills learned in the first half of this semester to implement artificial intelligence that can distinguish various fonts. Based on the data given below, carry out the project below:
(인공지능 시대에 이미 살고 있는 여러분은 이미 알고 있듯, ChatGPT와 같은 문자열에 대한 패턴인식은 인공지능에서 매우 중요한 자리를 차지 합니다. 그 중에서 다양한 글꼴을 동일 글자로 인식하는 기술 역시 앞으로 실시간 번역 기술등의 진보를 위해 매우 중요한 부분입니다. 다양한 글꼴을 구분할 수 있는 인공지능을 구현하기 위해 본 수업의 전반기에 배운 기술들을 적극 활용해봅시다. 주어진 아래의 자료를 바탕으로 아래의 프로젝트를 실시하십시오)

![image](https://github.com/medAndro/DataMining-Exam/assets/88672474/bb0fd984-2a01-4fba-93db-ff9ef9de9ca5)
![image](https://github.com/medAndro/DataMining-Exam/assets/88672474/80afa219-9043-42e5-a47b-bde141b11b04)


### [Step 1] Please properly (*Horrible term) cleanse given data files into a matrix form that can be modeled. (10 Points). 
(주어진 자료를 모델링이 가능한 하나의 매트릭스 형식으로 적절하게 (*진짜 무서운 말) 클렌징 하십시오.)

### [Final # of rows and # of columns after data cleansing]
17345 rows 405 columns

### [Please describe which part was cleaned.]
각 csv파일을 fread()로 불러오면서 NA값이 존재하는지, 존재한다면 어느 컬럼에 존재하는지 검사합니다. 이 과정에서 TIMES.csv 의 276번째 컬럼에 NA가 존재하여 해당 컬럼을 제거하였습니다. 다음으로 5개의 데이터프레임의 헤더명만 추출한 뒤 Reduce(intersect, headerList)로 서로 겹치는 컬럼명만 추출하여 405개의 열이 추출되었고 405개에 컬럼에 해당하는 데이터들을 5개의 데이터프레임에서 rbind()를 통해 모두 결합한 df_Q4_merge데이터프레임을 만들었습니다.


### [Step 2] Distinguishing between handwriting and computer writing is also an important skill. Given the data, find the best predictive model that can distinguish between handwriting and computer writing. When building a model, be sure to consider all available variables after cleansing process. Model evaluation must be performed under 10-fold CV condition. In particular, please use five or more models to find the best models and organize them in the table below. Your points are awarded in order of accuracy among students. Of course, accuracy obtained in a non-pair game environment is treated as 0%. Also, submit Q4-1_[Your ID].RData so that we can evaluate the true performance of your model on independent data. (20 Points). 
(손글씨와 컴퓨터글씨를 구분하는 것도 중요한 기술입니다. 주어진 자료를 바탕으로 손글씨와 컴퓨터 글씨를 구분할 수 있는 베스트 예측 모형을 찾으십시오. 모델 구축시에, 클렌징 이후 가용 가능한 모든 변수를 포함하여 모델을 구축해야 합니다. 모델 평가는 반드시 10-fold CV 하에서 실시하십시오. 최소 5개 이상의 모델을 사용하여 최적의 모델을 찾아 아래 표로 정리합니다. 정확도 순서대로 여러분의 점수가 부여됩니다. 물론, 페어 게임이 아닌 환경에서 얻어진 정확도는 0%로 처리됩니다. 또한, 독립적인 데이터에서 여러분이 구축한 모델에 대한 진정한 퍼포먼스 평가가 가능하도록 Q4-1_[Your ID].RData 를 같이 제출하십시오.)

### [Prediction Outcome between Handwriting vs Computer Font]

|Model name|Evaluated accuracy|
|------|---|
|LDA (model_lda_step2)|0.9017008|
|QDA (model_qda_step2)|0.946094|
|mlpML (model_mlpML_step2)|0.9746325|
|GBM (model_gbm_step2)|0.9731911|
|RF (model_rf_step2)|0.9829922|
|Adaboost (model_AdaBoost_step2)|0.9867397|
### Best Accuracy Model : Adaboost

## [Step 3] Build a predictive model that can distinguish each of the 5 different fonts. All other constraints are the same as in [Step 2]. Also, submit 
Q4-2_[Your ID].RData so that we can evaluate the true performance of your model on independent data.  (20 Points). 

|Model name|Evaluated accuracy|
|------|---|
|LDA (model_lda_step3)|0.7690977|
|QDA (model_qda_step3)|0.8423177|
|mlpML (model_mlpML_step3)|0.9250504|
|GBM (model_gbm_step3)|0.934275|
|RF (model_rf_step3)|0.9567599|
|Adaboost (model_AdaBoost_step3)|0.8662439|
### Best Accuracy Model : RF (Random Forest)
