
ctree_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response,Metric){

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
  if(length(unique(data.train[,response]))==2){

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
  }else{
    repeats <- ifelse(grepl("repeatedcv", samp.method), 10, 1)
    ctrl <- trainControl(method=samp.method,classProbs=TRUE,repeats=repeats)
  }
}

if(tuneLength > 3){
  stop("can't use more than 3 tuning values for ctree")
}

possible.tune <- c(.95,.99,.999)
tune <- possible.tune[1:tuneLength]
grid = expand.grid(tune)

names(grid) <- "mincriterion"

train.out <- train(formula,data.train,method="ctree",tuneGrid=grid,
                   trControl=ctrl,metric=Metric,na.action=na.pass)
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



return.splits <- list()

if(return.matrix[1,"nsplits"] == 0){
  return.splits <- as.data.frame(matrix(NA,1,2))
  colnames(return.splits) <- c("var","val")
  return.splits[1,1] <- "no split"
  return.splits[1,2] <- 0
  return.splits[1,1] <- as.character(return.splits[1,1])
  return.splits[1,2] <- as.numeric(as.character(return.splits[1,2]))
}else{

  hh <- CtreePathFunc(ctree.out,data=data.train)

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
}






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

  if(length(unique(data.train[,response])) == 2){
    return.matrix[1,"auc.samp"] <- train.out$results[ind,"ROC"]
    return.matrix[1,"accuracy.samp"] <- train.out$results[ind,"Accuracy"]

    if(subset==FALSE){
      return.matrix[1,"auc.test"] <- NA
    }else{
      return.matrix[1,"auc.test"] <- NA
    }
  }else{
    return.matrix[1,"accuracy.samp"] <- train.out$results[ind,"Accuracy"]
  }


}


ret$return.splits <- return.splits
ret$firstSplit <- return.splits[1,]
ret$vec <- return.matrix
ret$ctree.ret <- ctree.ret
ret$ctree.train <- train.out
return(ret)

}
