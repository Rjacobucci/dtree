
ctree_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response){

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
train.out <- train(formula,data.train,method="ctree",tuneLength=tuneLength,
                   trControl=ctrl)
ctree.out <- train.out$finalModel


#min.error <- which(min(cp[,"xerror"]) == cp[,"xerror"])[1]

nodes <- ctree.out@get_where()
return.matrix[1,"nsplits"] <- max(nodes) - length(unique(nodes))
#return.matrix[1,"fit.cv"] <- cp[min.error,"xerror"]

preds <- predict(ctree.out)



#depth(ctree.out$node)


node.try <- subset(1:max(nodes), !(1:max(nodes) %in% unique(nodes)))

var.name <- rep(NA,length(node.try))

if(length(var.name) > 0){
  for(i in 1:length(var.name)){
    var.name[i] <- party::nodes(ctree.out,node.try[i])[[1]]$psplit$variableName
  }
}


ctree.ret <- ctree.out


return.matrix[1,"nvar"] <- length(unique(var.name))

return.matrix[1,"nodes"] <- length(unique(nodes))

ind <- as.numeric(row.names(train.out$bestTune))
if(class.response == "numeric" | class.response == "integer"){
  #which(train.out$results[,"cp"] == train.out$bestTune)

  return.matrix[1,"rmse.samp"] <- train.out$results[ind,"RMSE"]
  #return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(ctree.out))^2)/nrow(data.train)
  return.matrix[1,"rsq.samp"] <- train.out$results[ind,"Rsquared"]

  if(subset==FALSE){
    return.matrix[1,"rmse.test"] <- NA
    return.matrix[1,"rsq.test"] <- NA
  }else{
    return.matrix[1,"rmse.test"] <- mean((data.test[,response] -
                                            predict(ctree.out,data.test))^2)/nrow(data.test)
    return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(ctree.out,data.test)))**2
  }
}else{
  return.matrix[1,"accuracy.samp"] <- train.out$results[ind,"Accuracy"]
  #return.matrix[1,"accuracy.train"] <- mean(round(predict(ctree.out)[,2])+1 == as.numeric(data.train[,response]))
  return.matrix[1,"accuracy.test"] <- mean(round(predict(ctree.out,data.test)[,2])+1 == as.numeric(data.test[,response]))
}



ret$vec <- return.matrix
ret$ctree.ret <- ctree.ret
ret$ctree.train <- train.out
return(ret)

}
