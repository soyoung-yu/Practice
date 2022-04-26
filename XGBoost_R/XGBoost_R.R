# 공부시간-합격확률 자료 Classification ##############################

# 데이터
df <- data.frame(
  Hours = c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
  Pass = c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)
df



# 데이터 분할
library(caret)
#caret(Classification And REgression Training) 패키지
#학습, 데이터 분할, 예측성능확인

idx <- createDataPartition(df$Pass, list=F, p=0.8)
#createDataPartition - 데이터 분할
#list : 리스트 형식으로의 표현 여부
#p : 훈련 데이터 비율
#times : 분할 개수
#반환값 = 훈련데이터로 사용할 데이터의 색인
Train <- df[idx,]
Test <- df[-idx,]

#feature set은 행렬형태로 dad
train.data = as.matrix(Train[, names(df)!="Pass"])
test.data = as.matrix(Test[, names(df)!="Pass"])
train.label = Train$Pass
test.label = Test$Pass



#모델 적합
library(xgboost)
model <- xgboost(data = train.data,
        label = train.label,
        max.depth = 2,
        nthread = 2,
        eta = 1,
        nrounds =2)
#eta : learning rate (학습 단계별로 가중치를 얼마나 적용할지)
#nrounds : 모델에서 iteration을 몇 번 진행할지 (클수록 train에 오버피팅)
#max.depth : 트리 최대 깊이 (클수록 train에 오버피팅)
#nthread : CPU 실행 스레드 개수 조정



#테스트
pred <- predict(model, test.data) #확률값으로 반환
pred <- ifelse(pred>0.5, 1, 0)  #분류 값으로 전환

#accuracy
sum(pred==test.label)/length(pred)





#iris 자료 #########################################################

#데이터 준비
df  <-  iris
names <- levels(df$Species) #levels : 범주형 변수의 값


#데이터 분할
library(caret)
idx = createDataPartition(df$Species, list=F, p=0.8)
Train = df[idx,]
Test = df[-idx,]

train.data = as.matrix(Train[, names(df)!='Species'])
test.data = as.matrix(Test[, names(df)!='Species'])
train.label = as.integer(Train$Species)-1
test.label = as.integer(Test$Species)-1
#as.interger : 범주형 변수를 1~ 정수로 반환 



#모델 fit
library(xgboost)
#xgb.train의 경우, xgb.DMatrix 형태의 입력변수만 받음
#DMatrix로 train/test셋 생성
dtrain = xgb.DMatrix(data=train.data,
                     label=train.label)
dtest = xgb.DMatrix(data=test.data,
                    label=test.label)

watchlist = list(train=dtrain,
                 eval=dtest)
param = list(max_depth=2, eta=1, verbose=0, nthread=2,
             objective="multi:softprob", eval_matric="mlogloss",
             num_class = 3)
#object : 학습 수행시 객체함수 설정
##"reg:linear" : 회귀, "binary:logistic" : 이진분류
##"multi:softmax" : 다중분류 클래스반환, "multi:softprob" : 다중분류 확률반환

#eval_metric : 평가를 위한 지표
## "rmase" : 회귀에 사용, "error" : 분류문제에 사용, "mlogloss" : 다중분류에 사용

#num_class : 복수의 클래스를 가진 분류모델을 짤 때 사용, 클래스가 몇개 존재하는지 알려주기 위함


#model 적합
model = xgb.train(params = param,
                  data = dtrain,
                  nrounds =5,
                  watchlist = watchlist)


#테스트
pred = as.data.frame(predict(model, test.data, reshape=T))
# col : label
# row : label별 확률

colnames(pred) = levels(df$Species) #pred 데이터프레임의 열 이름을 변경

#가장 확률이 높은 확률값을 찾아 'prediction' 속성으로 추가
pred$prediction = apply(pred,1, function(x) names[which.max(x)])
#which() : 괄호 내의 조건에 해당하는 인덱스 출력
#which.max() : 최댓값 위치 반환

#apply : 행 또는 열 단위의 연산을 쉽게 할수있도록 지원
#MARGIN : 1or2, 1:행단위 연산, 2:열단위 연산

#실제 분류값 'class' 속성으로 추가
pred$class = Test$Species


#분류표 - 맞춘 수
table(pred$prediction, pred$class)

#accuracy
sum(pred$prediction==pred$class)/nrow(pred)
