
ctreePrune_ret <- function(formula,data.train,data.test,class.response,subset,response){


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



  out <- ctreePrune(formula=formula, data=data.train)
  ctreePrune.out <- ctreePrune.ret <- out$tree

  #min.error <- which(min(cp[,"xerror"]) == cp[,"xerror"])[1]
  return.matrix[1,"nsplits"] <- max(fitted(ctreePrune.out)[,1]) - length(unique(fitted(ctreePrune.out)[,1]))
  #return.matrix[1,"fit.cv"] <- cp[min.error,"xerror"]



  #depth(ctreePrune.out$node)

  ret.obj <- as.list(ctreePrune.out$node)
  len <- length(ret.obj)

  vars <- rep(NA,len)
  for(i in 1:len){

    if(is.null(ret.obj[[i]]$split$varid)==FALSE){
      vars[i] <- ret.obj[[i]]$split$varid -1
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
  if(return.matrix[1,"nsplits"] == 0 | any(is.na(breaks))){
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

  ctreePrune.ret <- ctreePrune.out
  #attributes(ctreePrune.out)


  return.matrix[1,"nvar"] <- vars3

  return.matrix[1,"nodes"] <- length(unique(fitted(ctreePrune.out)[,1]))

  met1 <- rep(NA,20)
  met2 <- rep(NA,20)
  for(i in 1:20){
    set.seed(i)

    ids1 <- sample(nrow(data.train),nrow(data.train),replace=TRUE)

    train <- data.train[ids1,]
    test <- data.train[-ids1,]

    tt <- ctreePrune(formula=formula, data=train)

    if(class.response == "numeric" | class.response == "integer"){
      met1[i] <- sqrt(mean((test[,response] - predict(tt$tree,test))^2))
      pp = predict(tt$tree,test)
       if(sd(pp)==0) pp <- pp+rnorm(length(pp),0,.000001)
      met2[i] <- (cor(test[,response],pp))**2
    }else{
      if(all(duplicated(test[,response])[-1L])){
        met1[i] <- NA
      }else{
        met1[i] <- pROC::auc(test[,response],predict(tt$tree,test,type="prob")[,1])
      }
      met2[i] <- caret::confusionMatrix(test[,response],predict(tt$tree,test))$overall["Accuracy"]
      }

  }





  if(class.response == "numeric" | class.response == "integer"){
    #which(train.out$results[,"cp"] == train.out$bestTune)

    #return.matrix[1,"rmse.samp"] <- train.out$results[ind,"RMSE"]
    return.matrix[1,"rmse.samp"] <- mean(met1) #sqrt(mean((data.train[,response] - predict(ctreePrune.out))^2))
    return.matrix[1,"rsq.samp"] <- mean(met2) #(cor(data.train[,response],predict(ctreePrune.out)))**2
   #return.matrix[1,"rsq.samp"] <- train.out$results[ind,"Rsquared"]

    if(subset==FALSE){
      return.matrix[1,"rmse.test"] <- NA
      return.matrix[1,"rsq.test"] <- NA
    }else{
      return.matrix[1,"rmse.test"] <- sqrt(mean((data.test[,response] - predict(ctreePrune.out,data.test))^2))
      return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(ctreePrune.out,data.test)))**2
    }
  }else{
    return.matrix[1,"auc.samp"] <- mean(met1,na.rm=TRUE)#pROC::auc(data.train[,response],predict(ctreePrune.out,type="prob")[,1])
    return.matrix[1,"accuracy.samp"] <- mean(met2)#caret::confusionMatrix(data.train[,response],predict(ctreePrune.out))$overall["Accuracy"]

    if(subset==FALSE){
     # return.matrix[1,"auc.test"] <- NA
    }else{
    #  return.matrix[1,"auc.test"] <- NA
    }
  }




  ret$return.splits <- return.splits
  ret$firstSplit <- return.splits[1,]
  ret$vec <- return.matrix
  ret$ctreePrune.ret <- ctreePrune.ret
  return(ret)
}
