# read data
expedia <- read.csv("dataset.csv")
expedia$srch_ci <- NULL
expedia$srch_co <- NULL
expedia$site_name<-NULL
expedia$user_location_city<-NULL
expedia$is_mobile<-NULL
expedia$srch_rm_cnt<-NULL
expedia$srch_destination_type_id<-NULL
str(expedia)
head(expedia)
set.seed(12345)
train <- sample(nrow(expedia),0.7*nrow(expedia))
expedia_train <- expedia[train,]
expedia_training <- expedia[train,]
expedia_validation <- expedia[-train,]

##multinomial logistic regression
library(nnet)
fit <- multinom(hotel_cluster~.,data=expedia_train)
pred <- predict(fit,expedia_validation)
table1 <- table(pred,expedia_validation$hotel_cluster)
sum <- 0
for (i in 1:21)
{
  sum<-sum+table1[i,i]
}
accuracy<-sum/sum(table1)
accuracy

##lasso
x=model.matrix(hotel_cluster~.,expedia_train)[,-1]
library(glmnet)
grid=10^seq(10,-2,length=100)
max(grid)
lasso.mod=glmnet(x,expedia_train$hotel_cluster,alpha=1,lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(x,expedia_train$hotel_cluster,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x)
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam)[1:20,]
lasso.coef[lasso.coef!=0]

# naive rule
table(expedia_train$hotel_cluster)
table(expedia_validation$hotel_cluster)
1813/nrow(expedia_validation)