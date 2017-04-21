
evtree_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response,Metric){

ret <- list()

if(class.response == "numeric" | class.response == "integer"){
  return.matrix <- matrix(NA,1,7)
  colnames(return.matrix) <- c("nodes","nvar","nsplits","rmse.samp",
                               "rsq.samp","rmse.test","rsq.test")

}else{
  return.matrix <- matrix(NA,1,7)
  colnames(return.matrix) <- c("nodes","nvar","nsplits","auc.samp",
                               "accuracy.samp","auc.test","accuracy.test")

}

evtree.out <- evtree(formula,data.train)

#if(inherits(train.out, "try-error")){
#  return.matrix[1,] <- c(0,0,0,NA,NA,NA,NA)
#  return.splits <- as.data.frame(matrix(NA,1,2))
#  colnames(return.splits) <- c("var","val")
#  return.splits[1,1] <- "no split"
#  return.splits[1,2] <- 0
#  return.splits[1,1] <- as.character(return.splits[1,1])
#  return.splits[1,2] <- as.numeric(as.character(return.splits[1,2]))
#  evtree.ret <- NA
#  train.out <- NA

#}else{

#evtree.out <- train.out$finalModel

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
    vars[i] <- ret.obj[[i]]$split$varid
  }else{
    vars[i] <- NA
  }
}



breaks <- rep(NA,len)
for(i in 1:len){

  if(is.null(ret.obj[[i]]$split$breaks)==FALSE){
    breaks[i] <- ret.obj[[i]]$split$breaks
  }else if(is.null(ret.obj[[i]]$split$index)==FALSE){
    breaks[i] <- median(ret.obj[[i]]$split$index)
  }else{
    breaks[i] <- NA
  }
}


return.splits <- list()

if(return.matrix[1,"nsplits"] == 0 ){
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

met1 <- rep(NA,20)
met2 <- rep(NA,20)

  if(class.response == "numeric" | class.response == "integer"){
    met1 <- sqrt(mean((data.train[,response] - predict(evtree.out))^2))
    pp = predict(evtree.out)
    if(sd(pp)==0) pp <- pp+rnorm(length(pp),0,.000001)
    met2 <- (cor(data.train[,response],pp))**2
  }else{
    if(all(duplicated(data.train[,response])[-1L])){
      met1 <- NA
    }else{
      if(length(levels(class.response)) == 2){
        met1 <- pROC::auc(data.train[,response],predict(evtree.out,type="prob")[,1])
      }

    }
    met2 <- caret::confusionMatrix(data.train[,response],predict(evtree.out))$overall["Accuracy"]
  }







if(class.response == "numeric" | class.response == "integer"){
  #which(train.out$results[,"cp"] == train.out$bestTune)

  #return.matrix[1,"rmse.samp"] <- train.out$results[ind,"RMSE"]
  return.matrix[1,"rmse.samp"] <- met1 #sqrt(mean((data.train[,response] - predict(ctreePrune.out))^2))
  return.matrix[1,"rsq.samp"] <- met2 #(cor(data.train[,response],predict(ctreePrune.out)))**2
  #return.matrix[1,"rsq.samp"] <- train.out$results[ind,"Rsquared"]

  if(subset==FALSE){
    return.matrix[1,"rmse.test"] <- NA
    return.matrix[1,"rsq.test"] <- NA
  }else{
    return.matrix[1,"rmse.test"] <- sqrt(mean((data.test[,response] - predict(evtree.out,data.test))^2))
    return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(evtree,data.test)))**2
  }
}else{
  if(length(levels(class.response)) == 2){
  return.matrix[1,"auc.samp"] <- met1#pROC::auc(data.train[,response],predict(ctreePrune.out,type="prob")[,1])
  return.matrix[1,"accuracy.samp"] <- met2#caret::confusionMatrix(data.train[,response],predict(ctreePrune.out))$overall["Accuracy"]

    if(subset==FALSE){
     # return.matrix[1,"auc.test"] <- NA
    }else{
      #  return.matrix[1,"auc.test"] <- NA
    }
  }else{
    return.matrix[1,"accuracy.samp"] <- met2
  }
}

#}
ret$return.splits <- return.splits
ret$firstSplit <- return.splits[1,]
ret$vec <- return.matrix
ret$evtree.ret <- evtree.ret
return(ret)


}
