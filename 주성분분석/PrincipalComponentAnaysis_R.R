#PCA
#install.packages("HSAUR")
# install.packages("Require")
# install.packages("Graphics")
library(graphics)
library(HSAUR)


data(heptathlon) #HSAUR 패키지 내의 heptathlon 데이터 가져오기
head(heptathlon) #데이터 확인
summary(heptathlon) #데이터 기초 통계량 확인

hep.data.temp = heptathlon
#값이 작을수록 점수가 좋은 종목들은 최대값에서 각 값을 빼준다.
#고유벡터로 관계의 방향과 크기를 알 수 있으므로, 방향성을 맞춰주어야 함
hep.data.temp$hurdles = max(hep.data.temp$hurdles)-hep.data.temp$hurdles
hep.data.temp$run200m = max(hep.data.temp$run200m)-hep.data.temp$run200m
hep.data.temp$run800m = max(hep.data.temp$run800m)-hep.data.temp$run800m

#score 데이터 제외
hep.data = hep.data.temp[,-8]

#공분산 확인
var(hep.data)
#상관계수 확인
cor(hep.data)

hep.data.pca = prcomp(hep.data, scale. = T)
summary(hep.data.pca)

#스크리 차트
screeplot(hep.data.pca, type="lines", pch=1, main="scree plot")

hep.data.pca$rotation[,1:2]  #각 주성분의 rotation 값은 '고유벡터'


hep.data.pca$x

#biplot - 각 개체에 대한 첫 번째, 두 번째 주성분 점수 및 행렬도
# 각 개체의 주성분점수 관찰값과, 각 변수와 주성분과의 관계 (주성분계수)를 동시에 나타내어, 
# 이들의 관계를 살펴봄
biplot(hep.data.pca, main='Biplot')
