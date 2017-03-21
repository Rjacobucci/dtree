library(ISLR)
data(Default)


lr.out <- glm(default~.,family="binomial",data=Default)
summary(lr.out)
summary(lr.out)$coefficients[,"Pr(>|z|)"][-1]

round(predict(lr.out,type="response"))

mean(round(predict(lr.out,type="response"))+1 == as.numeric(Default$default))


out <- dtree(default ~ ., data=Default,methods=c("lm","rpart","tree","rf","evtree","ctree"))
summary(out)


ret <- stable(default ~ ., data=Default,methods=c("lm","rpart","ctree","evtree"))
ret

rett <- unlist(ret)
results <- matrix(rett,100,18,byrow=FALSE)





out2 <- rpart(default ~ ., data=Default)
out2
library(stablelearner)
out8 <- stablelearner::stabletree(out2)


evtree.out <- evtree::evtree(default ~ ., data=Default)


library(stablelearner)
library(MASS) # for boston data
data(Boston)


out <- dtree(medv ~., data=Boston,methods=c("rpart","ctree","evtree"),tuneLength=2,samp.method="cv")
out







ctrl <- trainControl(method="repeatedcv")
train.out <- train(medv ~., data=Boston,method="evtree",tuneLength=1,
                   trControl=ctrl)
plot(train.out)
train.out
varImp(train.out)

 summary(out)
#' plot(out$rpart.out)
tt <- rpart(medv ~., data=Boston)
ttt <- ctree(medv ~., data=Boston)

stab.out <- stable(formula=medv ~.,
                       data=Boston,
                       methods=c("rpart"),
                       samp.method="cv",
                       tuneLength=2,
                       n.rep=100,
                       stablelearner=TRUE,
                       subset=FALSE,
                       perc.sub=.75,
                       weights=NULL)

out99 <- stablelearner::stabletree(train.out$finalModel,B=20)
summary(out99)
