
evtree_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response,Metric){

ret <- list()

if(class.response == "numeric" | class.response == "integer"){
  return.matrix <- matrix(NA,1,7)
  colnames(return.matrix) <- c("nodes","nvar","nsplits","rmse.samp",
                               "rsq.samp","rmse.test","rsq.test")
  repeats <- ifelse(grepl("repeatedcv", samp.method), 10, 1)
  ctrl <- trainControl(method=samp.method,repeats=repeats)
}else{
  return.matrix <- matrix(NA,1,7)
  colnames(return.matrix) <- c("nodes","nvar","nsplits","auc.samp",
                               "accuracy.samp","auc.test","accuracy.test")
  fiveStats <- function(...) c(twoClassSummary(...),
                               + defaultSummary(...))
  ## Everything but the area under the ROC curve:
  fourStats <- function (data, lev = levels(data$obs), model = NULL)
  {

    accKapp<-postResample(data[,"pred"],data[,"obs"])
    out<-c(accKapp,
           sensitivity(data[, "pred"], data[, "obs"], lev[1]),
           specificity(data[, "pred"], data[, "obs"], lev[2]))
    names(out)[3:4]<-c("Sens","Spec")
    out
  }
  repeats <- ifelse(grepl("repeatedcv", samp.method), 10, 1)
  ctrl <- trainControl(method=samp.method,classProbs=TRUE,summaryFunction = fiveStats,repeats=repeats)
}

train.out <- train(formula,data.train,method="evtree",tuneLength=tuneLength,
                   trControl=ctrl,metric=Metric)
evtree.out <- train.out$finalModel

#if(inherits(train.out, "try-error")){
#  return.matrix <- NA
#}else{






#min.error <- which(min(cp[,"xerror"]) == cp[,"xerror"])[1]
return.matrix[1,"nsplits"] <- max(fitted(evtree.out)[,1]) - length(unique(fitted(evtree.out)[,1]))
#return.matrix[1,"fit.cv"] <- cp[min.error,"xerror"]



#depth(evtree.out$node)

ret.obj <- as.list(evtree.out$node)
len <- length(ret.obj)

vars <- rep(NA,len)
for(i in 1:len){

  if(is.null(ret.obj[[i]]$split$varid)==FALSE){
    vars[i] <- ret.obj[[i]]$split$varid + 1
  }else{
    vars[i] <- NA
  }
}



breaks <- rep(NA,len)
for(i in 1:len){

  if(is.null(ret.obj[[i]]$split$breaks)==FALSE){
    breaks[i] <- ret.obj[[i]]$split$breaks
  }else{
    breaks[i] <- NA
  }
}


return.splits <- list()

if(return.matrix[1,"nsplits"] == 0){
  return.splits <- as.data.frame(matrix(NA,1,2))
  colnames(return.splits) <- c("var","val")
  return.splits[1,1] <- "no split"
  return.splits[1,2] <- 0
  return.splits[1,1] <- as.character(return.splits[1,1])
  return.splits[1,2] <- as.numeric(as.character(return.splits[1,2]))
}else{
  tt = terms(formula,data=data.train)
  preds <- unlist(attr(tt,"term.labels"))

  breaks2 <- breaks[complete.cases(breaks)]
  vars2 <- vars[complete.cases(vars)]

  return.splits <- data.frame(cbind(preds[vars2],breaks2))
  colnames(return.splits) <- c("var","val")
  return.splits[,2] <- as.numeric(as.character(return.splits[,2]))

}



vars2 <- vars[is.na(vars)==FALSE]
vars3 <- length(unique(vars2))

evtree.ret <- evtree.out
#attributes(evtree.out)


return.matrix[1,"nvar"] <- vars3

return.matrix[1,"nodes"] <- length(unique(fitted(evtree.out)[,1]))


ind <- as.numeric(row.names(train.out$bestTune))
if(class.response == "numeric" | class.response == "integer"){
  #which(train.out$results[,"cp"] == train.out$bestTune)

  return.matrix[1,"rmse.samp"] <- train.out$results[ind,"RMSE"]
  #return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(evtree.out))^2)/nrow(data.train)
  return.matrix[1,"rsq.samp"] <- train.out$results[ind,"Rsquared"]

  if(subset==FALSE){
    return.matrix[1,"rmse.test"] <- NA
    return.matrix[1,"rsq.test"] <- NA
  }else{
    return.matrix[1,"rmse.test"] <- mean((data.test[,response] -
                                            predict(evtree.out,data.test))^2)/nrow(data.test)
    return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(evtree.out,data.test)))**2
  }
}else{
  return.matrix[1,"auc.samp"] <- train.out$results[ind,"ROC"]
  return.matrix[1,"accuracy.samp"] <- train.out$results[ind,"Accuracy"]

  if(subset==FALSE){
    return.matrix[1,"auc.test"] <- NA
  }else{
    return.matrix[1,"auc.test"] <- NA
  }
}

#}
ret$return.splits <- return.splits
ret$firstSplit <- return.splits[1,]
ret$vec <- return.matrix
ret$evtree.ret <- evtree.ret
ret$evtree.train <- train.out
return(ret)

}
