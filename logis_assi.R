library(rpart)
library(readxl)
library(pastecs)
df_bass = read_xls("bass.xls")
colnames(df_bass)[3:10] = c("catch","income","employed","educatio","married","sex","age","nc")
str(df_bass)
summary(df_bass)

stat.desc(df_bass,basic = F)
#spliting data into train and test
set.seed(1)
df = sample(1:nrow(df_bass),0.7*nrow(df_bass))
bass_train = df_bass[df,]
bass_test = df_bass[-df,]
#baseline model
table(bass_train$yes)
prop.table(table(bass_train$yes))
#logistic regression
model_b1 = glm(yes~cost,data = bass_train,family = "binomial")
summary(model_b1)
#task 1
#begining and ending of -2LL
-2*logLik(model_b1)
model_b1$deviance
model_b1$null.deviance
#model chi-square statistic
anova(model_b1,test="Chisq")
#As the p-values of the  variable is less than 0.05 so it is significant in the  model.
#effect of COST on YES
model_b1$coefficients
#Is the independent variable statistically significant?
#As the pr-values of the  cost is less than 0.05 so variable is significant
#predictions
pred = predict(model_b1,newdata = bass_test,type = "response",family = "binomial" )
pred
bass_test$predit = ifelse(pred<mean(pred),0,1)
#(accuracy)% correct predictions 
MLmetrics::Accuracy(bass_test$predit,bass_test$yes)*100
#task 2
names(bass_train)
model_b2 = glm(yes~cost+catch+employed,data = bass_train,family = "binomial")
summary(model_b2)
#1.begining and ending of -2LL
-2*logLik(model_b2)
model_b2$deviance
model_b2$null.deviance

#3.As the p-values of the  variable is less 0.5 so it is significant in the  model.
#4.effect of COST on YES
model_b2$coefficients
#5.which of  independent variable statistically significant?
#6.As the pr-values of the  cost,catch and employed is is less than 0.5 so variable is significant
summary(model_b2)
