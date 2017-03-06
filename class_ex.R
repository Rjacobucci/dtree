library(ISLR)
data(Default)


lr.out <- glm(default~.,family="binomial",data=Default)
summary(lr.out)
summary(lr.out)$coefficients[,"Pr(>|z|)"][-1]

round(predict(lr.out,type="response"))

mean(round(predict(lr.out,type="response"))+1 == as.numeric(Default$default))


out <- dtree(default ~ ., data=Default,methods=c("lm","rpart","tree","rf","evtree","ctree"))
summary(out)


ret <- stability(default ~ ., data=Default,methods=c("lm","rpart","ctree","evtree"))
ret

rett <- unlist(ret)
results <- matrix(rett,100,18,byrow=FALSE)





out2 <- rpart(default ~ ., data=Default)
out2
library(stablelearner)
out8 <- stablelearner::stabletree(out2)


evtree.out <- evtree::evtree(default ~ ., data=Default)



library(MASS) # for boston data
data(Boston)
out <- dtree(medv ~., data=Boston,methods=c("rpart"))

ctrl <- trainControl(method="repeatedcv")
train.out <- train(medv ~., data=Boston,method="evtree",tuneLength=1,
                   trControl=ctrl)
plot(train.out)
train.out
varImp(train.out)

 summary(out)
#' plot(out$rpart.out)
tt <- rpart(medv ~., data=Boston)

(stab.out <- stability(medv ~., data=Boston,methods=c("lm","rpart","ctree"),prune=TRUE,subset=TRUE,perc.sub=.5))

out99 <- stablelearner::stabletree(train.out$finalModel,B=20)
summary(out99)
