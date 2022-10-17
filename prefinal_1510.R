library(readr)
library(dplyr)
library(caret)
library(tidyr)
library(quantreg)


Rec.all <- function(data, lev = NULL, model = NULL) {
  uni<-unique(data$obs)
  k<-length(uni)
  data$pred[data$pred>max(uni)]<-max(uni)
  data$pred[data$pred<min(uni)]<-min(uni)
  data$pred<-round(data$pred)
  b<-0
  for  (i in 1:k)
  {
    a<-MLmetrics::Recall (data$obs, data$pred, positive=uni[i])
    if (is.na(a)) a<-0
    b<-b+a
  }
  c(Rec.all=b/i)
  
}  

setwd("E:/R/_cp2022/moscow/dataF3")

train<-read_csv("train_dataset_train.csv")
ud<-read_csv("user_decision.csv")

ud.grp<-ud%>%group_by(id=user_id)%>%
  summarise(n1=n(), n2=n_distinct(period), n3=n_distinct(decision_id))

train<-train%>%left_join(ud.grp)

tr<-train[,6:8]


#####VALIDATION
'
tr$Y<-(train$`Analytical thinking`)
tr$Y<-(train$`Systemic thinking`)
tr$Y<-(train$Adaptability)
tr$Y<-(train$Focus)


ans<-NULL
for (i in 1:680)
  
{
print (i)
set.seed(123)
model<-rq(Y~., data=tr[-i,], method="sfn",tau=(0:100)/100)
#summary(model)
ans[[i]]<-predict(model, tr[i,-4])


}

ans2<-unlist(ans)
ans3<-as.data.frame(matrix(ans2,ncol = 101, byrow = T))

metr<-apply(ans3,2, function(x) Rec.all(data.frame(obs=tr$Y, pred=x)))
plot.ts(metr)
which.max(metr)
max(metr)

'

##Models
sample0<-read_csv("sample_solution.csv")
sample<-sample0%>%left_join(ud.grp)
sample<-sample[,6:8]



tr$Y<-(train$`Analytical thinking`)
mod1<-rq(Y~., data=tr, method="sfn",tau=65/100)
sample$`Analytical thinking`<-round(predict(mod1,sample)[1:nrow(sample)])
table(sample$`Analytical thinking`)

tr$Y<-(train$`Systemic thinking`)
mod2<-rq(Y~., data=tr, method="sfn",tau=98.5/100)
sample$`Systemic thinking`<-round(predict(mod2, sample)[1:nrow(sample)])
table(sample$`Systemic thinking`)

tr$Y<-(train$Adaptability)
mod3<-rq(Y~., data=tr, method="sfn",tau=51.5/100)
sample$Adaptability<-round(predict(mod3, sample)[1:nrow(sample)])
table(sample$Adaptability)

tr$Y<-(train$Focus)
mod4<-rq(Y~., data=tr, method="sfn",tau=64.5/100)
sample$Focus<-round(predict(mod4, sample)[1:nrow(sample)])
table(sample$Focus)


sample0$`Analytical thinking`<-sample$`Analytical thinking`
sample0$`Systemic thinking`<-sample$`Systemic thinking`
sample0$Adaptability<-sample$Adaptability
sample0$Focus<-sample$Focus

write_csv(sample0, "final_best_q.csv")
