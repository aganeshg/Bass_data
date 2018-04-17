library(boot)
library(pastecs)
library(readxl)
library(rpart)
library(rpart.plot)
library(ROCR)
#read the input data 
data<-read.csv("bass.csv")
str(data)
data$yes
as.factor(data$yes)
data$yes_f=factor(data$yes)
#Summary and descriptive statistics
summary(data)
stat.desc(data,basic = F)
#split the data as train and test data
set.seed(1)
df=sample(1:nrow(data),0.7*nrow(data))
data_train=data[df,]
data_test=data[-df,]

tree=rpart(yes_f~cost+catch+age+employed, data = data_train,method = "class")
prp(tree)
dim(data_test)
predi <- predict(tree,newdata = data_test,type = "class")

predi <- predict(tree,newdata = data_test,type = "prob")

predi = prediction(predi,data_test$yes_f)
predi
#confusion matrix
cm = table(predi,data_test$yes_f)
cm

model.probs<-predict(tree, data_test)
model.prediction<-prediction(model.probs[,2],data_test$yes_f)
model.performance<-performance(model.prediction,"tpr","fpr")
plot(model.performance,add=TRUE,col="blue",lwd=2)
