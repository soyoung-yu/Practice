# ���νð�-�հ�Ȯ�� �ڷ� Classification ##############################

# ������
df <- data.frame(
  Hours = c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
  Pass = c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)
df



# ������ ����
library(caret)
#caret(Classification And REgression Training) ��Ű��
#�н�, ������ ����, ��������Ȯ��

idx <- createDataPartition(df$Pass, list=F, p=0.8)
#createDataPartition - ������ ����
#list : ����Ʈ ���������� ǥ�� ����
#p : �Ʒ� ������ ����
#times : ���� ����
#��ȯ�� = �Ʒõ����ͷ� ����� �������� ����
Train <- df[idx,]
Test <- df[-idx,]

#feature set�� ������·� dad
train.data = as.matrix(Train[, names(df)!="Pass"])
test.data = as.matrix(Test[, names(df)!="Pass"])
train.label = Train$Pass
test.label = Test$Pass



#�� ����
library(xgboost)
model <- xgboost(data = train.data,
        label = train.label,
        max.depth = 2,
        nthread = 2,
        eta = 1,
        nrounds =2)
#eta : learning rate (�н� �ܰ躰�� ����ġ�� �󸶳� ��������)
#nrounds : �𵨿��� iteration�� �� �� �������� (Ŭ���� train�� ��������)
#max.depth : Ʈ�� �ִ� ���� (Ŭ���� train�� ��������)
#nthread : CPU ���� ������ ���� ����



#�׽�Ʈ
pred <- predict(model, test.data) #Ȯ�������� ��ȯ
pred <- ifelse(pred>0.5, 1, 0)  #�з� ������ ��ȯ

#accuracy
sum(pred==test.label)/length(pred)





#iris �ڷ� #########################################################

#������ �غ�
df  <-  iris
names <- levels(df$Species) #levels : ������ ������ ��


#������ ����
library(caret)
idx = createDataPartition(df$Species, list=F, p=0.8)
Train = df[idx,]
Test = df[-idx,]

train.data = as.matrix(Train[, names(df)!='Species'])
test.data = as.matrix(Test[, names(df)!='Species'])
train.label = as.integer(Train$Species)-1
test.label = as.integer(Test$Species)-1
#as.interger : ������ ������ 1~ ������ ��ȯ 



#�� fit
library(xgboost)
#xgb.train�� ���, xgb.DMatrix ������ �Էº����� ����
#DMatrix�� train/test�� ����
dtrain = xgb.DMatrix(data=train.data,
                     label=train.label)
dtest = xgb.DMatrix(data=test.data,
                    label=test.label)

watchlist = list(train=dtrain,
                 eval=dtest)
param = list(max_depth=2, eta=1, verbose=0, nthread=2,
             objective="multi:softprob", eval_matric="mlogloss",
             num_class = 3)
#object : �н� ����� ��ü�Լ� ����
##"reg:linear" : ȸ��, "binary:logistic" : �����з�
##"multi:softmax" : ���ߺз� Ŭ������ȯ, "multi:softprob" : ���ߺз� Ȯ����ȯ

#eval_metric : �򰡸� ���� ��ǥ
## "rmase" : ȸ�Ϳ� ���, "error" : �з������� ���, "mlogloss" : ���ߺз��� ���

#num_class : ������ Ŭ������ ���� �з����� © �� ���, Ŭ������ � �����ϴ��� �˷��ֱ� ����


#model ����
model = xgb.train(params = param,
                  data = dtrain,
                  nrounds =5,
                  watchlist = watchlist)


#�׽�Ʈ
pred = as.data.frame(predict(model, test.data, reshape=T))
# col : label
# row : label�� Ȯ��

colnames(pred) = levels(df$Species) #pred �������������� �� �̸��� ����

#���� Ȯ���� ���� Ȯ������ ã�� 'prediction' �Ӽ����� �߰�
pred$prediction = apply(pred,1, function(x) names[which.max(x)])
#which() : ��ȣ ���� ���ǿ� �ش��ϴ� �ε��� ���
#which.max() : �ִ� ��ġ ��ȯ

#apply : �� �Ǵ� �� ������ ������ ���� �Ҽ��ֵ��� ����
#MARGIN : 1or2, 1:����� ����, 2:������ ����

#���� �з��� 'class' �Ӽ����� �߰�
pred$class = Test$Species


#�з�ǥ - ���� ��
table(pred$prediction, pred$class)

#accuracy
sum(pred$prediction==pred$class)/nrow(pred)