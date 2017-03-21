
evtree_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response){

ret <- list()

if(class.response == "numeric" | class.response == "integer"){
  return.matrix <- matrix(NA,1,7)
  colnames(return.matrix) <- c("nodes","nvar","nsplits","rmse.samp",
                               "rsq.samp","rmse.test","rsq.test")
}else{
  return.matrix <- matrix(NA,1,5)
  colnames(return.matrix) <- c("nodes","nvar","nsplits","accuracy.samp",
                               "accuracy.test")
}
ctrl <- trainControl(method=samp.method)
train.out <- train(formula,data.train,method="evtree",tuneLength=tuneLength,
                   trControl=ctrl)
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
    vars[i] <- ret.obj[[i]]$split$varid
  }else{
    vars[i] <- NA
  }
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
  return.matrix[1,"accuracy.samp"] <- train.out$results[ind,"Accuracy"]
  #return.matrix[1,"accuracy.train"] <- mean(round(predict(evtree.out)[,2])+1 == as.numeric(data.train[,response]))
  return.matrix[1,"accuracy.test"] <- mean(round(predict(evtree.out,data.test)[,2])+1 == as.numeric(data.test[,response]))
}

#}

ret$vec <- return.matrix
ret$evtree.ret <- evtree.ret
ret$evtree.train <- train.out
return(ret)

}
