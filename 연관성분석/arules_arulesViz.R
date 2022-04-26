library(arules)
library(arulesViz)

data("Groceries")
Groceries  # 9835 rows, 169 items

#최소 지지도에 넣을 값을 유추하기 위해
#2회 이상 거래가 이루어진 2품목 이상 10품목 이하의 item set의 support를 구해본다.

ecl = eclat(Groceries, parameter = list(support=2/9835, minlen=2, maxlen=10))
#support는 임의로 작은 값을 넣은 듯

ecl
#위의 조건을 만족하는 itemset이 778493개 있음

inspect(sort(ecl)[1:50])
#문서를 쭉 보여주는 inspect 함수
#inspect(문서[a:b]) 문서의 범위를 지정해주면 특정 부분만 볼 수 있다.
#위의 코드는 내림차순으로 정렬하여 1~50개를 보여주는 것

summary(ecl)


rule = apriori(Groceries, parameter = list(support=50/9835, confidence=0.6, minlen=2))
rule

summary(rule)
inspect(rule)

rule_1 = sort(rule, by='lift')
inspect(rule_1[1:22])

rule = apriori(Groceries, 
               parameter = list(support=10/9835, confidence=0.6,minlen=2),
               appearance = list(none=c("whole milk","other vegetables")))
rule
inspect(sort(rule, by='confidence')[1:20])


rule_s = apriori(Groceries,
                 parameter = list(support=50/9835, confidence=0.3, minlen=2, maxlen=6),
                 appearance = list(rhs="soda", default='lhs'))
inspect(sort(rule_s, by='lift'))

GRO_df = as(Groceries, 'data.frame')
head(GRO_df)

inspect(Groceries)
