library(ISLR)
data(Default)


lr.out <- glm(default~.,family="binomial",data=Default)
summary(lr.out)
summary(lr.out)$coefficients[,"Pr(>|z|)"][-1]

round(predict(lr.out,type="response"))

mean(round(predict(lr.out,type="response"))+1 == as.numeric(Default$default))


out <- dtree(default ~ ., data=Default,methods=c("lm","rpart","tree","rf","evtree","ctree"))
summary(out)


out2 <- rpart(default ~ ., data=Default)
out2


evtree.out <- evtree::evtree(default ~ ., data=Default)



library(MASS) # for boston data
data(Boston)
out <- dtree(medv ~., data=Boston,methods=c("lm","rpart","ctree"))
 summary(out)
#' plot(out$rpart.out)
