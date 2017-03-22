library(ISLR)
data(Default)
Default=Default[1:1000,]

lr.out <- glm(default~.,family="binomial",data=Default)
summary(lr.out)
summary(lr.out)$coefficients[,"Pr(>|z|)"][-1]

round(predict(lr.out,type="response"))

mean(round(predict(lr.out,type="response"))+1 == as.numeric(Default$default))


out <- dtree(default ~ ., data=Default,methods=c("ctree"),tuneLength=2,samp.method="cv")
summary(out)


train.out <- train(default ~ ., data=Default,method="rpart",
                   trControl=ctrl,metric=Metric)


ret <- stable(default ~ ., data=Default,methods=c("ctree"),
              samp.method="cv",tuneLength=2,stablelearner=TRUE)
plot(ret$rpart)
summary(ret$evtree)
summary(ret$rpart)

rett <- unlist(ret)
results <- matrix(rett,100,18,byrow=FALSE)

tt = train(default ~ ., data=Default,method="evtree",tuneLength=1)$finalModel


library(rpart.utils)
rpart.subrules.table(tt)


str(partykit:::.list.rules.party(tt))


(yy = CtreePathFunc(tt,Default))


for(i in 1:nrow(yy)){
  pp <- list()
  pp <- c(pp,unique(unlist(strsplit(levels(yy[i,2]), ","))))
}

matt <- matrix(NA,length(pp),2)
for(j in 1:length(pp)){

  rr <- strsplit(pp[[j]],"[> <= ]" )
  rr2 <- rr[[1]][rr[[1]]!=""]
  matt[j,1] <- rr2[1]
  matt[j,2] <- rr2[2]
}


vars <- ctree.ret$frame[,"var"]
vars2 <- vars[vars != "<leaf>"]
return.matrix[1,"nvar"] <- length(unique(vars2))

return.matrix[1,"nodes"] <- length(vars[vars == "<leaf>"])





tree1 <- partykit::ctree(default ~ ., data=Default,
                         control=ctree_control(mincriterion=as.numeric(tt$bestTune)))
tree2 <- prune(tree1,cp=as.numeric(tt$bestTune))
ee = stabletree(tree1)
summary(ee)

out2 <- rpart(default ~ ., data=Default)
out2
library(stablelearner)
out8 <- stablelearner::stabletree(out2)


evtree.out <- evtree::evtree(default ~ ., data=Default)


library(stablelearner)
library(MASS) # for boston data
data(Boston)


out <- dtree(medv ~., data=Boston,methods=c("rpart","ctree","rf"),tuneLength=2,samp.method="cv")
summary(out)







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
