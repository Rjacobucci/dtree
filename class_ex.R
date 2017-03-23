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

tt = train(default ~ ., data=Default,method="rpart",tuneLength=1)$finalModel



rpart.utils::rpart.subrules.table(tt)


str(partykit:::.list.rules.party(tt))


(yy = CtreePathFunc(tt,Default))
yy = out$evtree.splits


yy[1]

#pp <- data.frame(matrix(NA,length(yy),1))
pp <- list()
for(i in 1:length(yy)){
  bb <- gsub("[> <= ]", "", yy[i])
  tt <- c(unique(unlist(strsplit(bb, "&"))))
  pp = unique(c(pp,tt))
}
oo <- unique(pp)
gsub("[[:digit:]]","",oo[[1]])
(stringr::str_extract(oo[[1]], "[aA-zZ]+"))
(stringr::str_extract(oo[[1]],  "\\d+\\.*\\d*"))

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


out <- dtree(medv ~., data=Boston,methods=c("ctree"),tuneLength=2,samp.method="cv")
#summary(out)
out




stab.out <- stable(formula=medv ~.,
                   data=Boston[1:100,],
                   methods=c("rpart","evtree","ctree"),
                   samp.method="cv",
                   tuneLength=2,
                   n.rep=3,
                   stablelearner=FALSE,
                   subset=FALSE,
                   perc.sub=.75,
                   weights=NULL)
stab.out

nn <- stab.out$nn

length(unique(nn[,1])) == 1
sapply(split(nn, nn$var),table)

ww <- stab.out$where.freq$age
sapply(split(where.freq, where.freq$var),
       table)

for(i in 1:length(ww)){
  print(sapply(split(ww[[i]], ww[[i]]$var),
         table))
}

hh <- out$ctree.splits
tab <- table(hh[,1])

hh[,2]

sapply(split(hh, hh$var),
         table)

nn <- plyr::ldply(ww)
sapply(split(nn, nn$var),
       table)

unlist(ww,recursive=FALSE)

table(hh[,2])

tt = terms(formula,data=Boston)
tt = terms(formula,data=data)
preds <- attr(tt,"term.labels")
var.count <- matrix(NA,n.rep,length(preds))
colnames(var.count) <- preds


for(j in 1:length(preds)){
  var.count[preds[j]] <- tab[preds[j]]
  if(is.na(var.count[preds[j]]==TRUE)) var.count[preds[j]] <- 0
}




table(hh[,1])

pp <- list()
for(i in 1:length(hh$Path)){
  bb <- gsub("[> <= ]", "", hh$Path[i])
  tt <- c(unique(unlist(strsplit(bb, ","))))
  pp = unique(c(pp,tt))
}
ret2 <- unique(pp)
ret3 <- data.frame(matrix(NA, length(ret2),2))

for(j in 1:length(ret2)){
  ret3[j,1] <- stringr::str_extract(ret2[[j]], "[aA-zZ]+")
  ret3[j,2] <- as.numeric(as.character(stringr::str_extract(ret2[[j]],  "\\d+\\.*\\d*")))
}

#ret3[,2] <- round(ret3[,2],3)
colnames(ret3) <- c("var","val")
return.splits <- ret3








hh2 <- hh[is.na(hh$Less == FALSE),]
hh3 <- hh2[,c("Variable","Value")]
colnames(hh3) <- c("var","val")


ctrl <- trainControl(method="repeatedcv")
train.out <- train(medv ~., data=Boston,method="evtree",tuneLength=1,
                   trControl=ctrl)
plot(train.out)
train.out
varImp(train.out)

 summary(out)
#' plot(out$rpart.out)
tt <- rpart(medv ~., data=Boston)
ttt <- evtree(medv ~., data=Boston)
partykit:::.list.rules.party(ttt)

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
