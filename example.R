library(MASS) # for boston data
data(Boston)


rpart.Boston <- rpart::rpart(medv ~., data=Boston,weights=NULL)
#summary(rpart.Boston)
plot(rpart.Boston);text(rpart.Boston)

ctree.Boston <- party::ctree(medv ~., data=Boston)


out <- dtree(medv ~., data=Boston,methods=c("lm","rpart","rf"))

summary(out)

out
plot(out$rpart.out);text(out$rpart.out)
plot(out$evtree.out)
plot(out$ctree.out)
