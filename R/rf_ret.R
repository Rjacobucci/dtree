
rf_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response,Metric){
  ret <- list()


  if(class.response == "numeric" | class.response == "integer"){
    return.matrix <- matrix(NA,1,7)
    colnames(return.matrix) <- c("nodes","nvar","nsplits","rmse.samp",
                                 "rsq.samp","rmse.test","rsq.test")

    repeats <- ifelse(grepl("repeatedcv", samp.method), 10, 1)
    ctrl <- trainControl(method=samp.method,repeats=repeats)
    rfMethod="rf"
  }else{
    return.matrix <- matrix(NA,1,7)
    colnames(return.matrix) <- c("nodes","nvar","nsplits","auc.samp",
                                 "accuracy.samp","auc.test","accuracy.test")
    if(length(levels(class.response))==2){
      rfMethod="rf"
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
      rfMethod="rf"
    }
  }



  train.out <- train(formula,data.train,method=rfMethod,tuneLength=tuneLength,
                     trControl=ctrl,metric=Metric,na.action=na.pass)
  rf.out <- train.out$finalModel


  rf.ret <- rf.out


  ind <- as.numeric(row.names(train.out$bestTune))
  if(class.response == "numeric" | class.response == "integer"){
    #which(train.out$results[,"cp"] == train.out$bestTune)

    return.matrix[1,"rmse.samp"] <- train.out$results[ind,"RMSE"]
    #return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(rf.out))^2)/nrow(data.train)
    return.matrix[1,"rsq.samp"] <- train.out$results[ind,"Rsquared"]

    if(subset==FALSE){
      return.matrix[1,"rmse.test"] <- NA
      return.matrix[1,"rsq.test"] <- NA
    }else{
      return.matrix[1,"rmse.test"] <- mean((data.test[,response] -
                                              predict(rf.out,data.test))^2)/nrow(data.test)
      return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(rf.out,data.test)))**2
    }
  }else{

    if(length(levels(class.response)) == 2){
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

  #}

  ret$vec <- return.matrix
  ret$rf.ret <- rf.ret
  ret$rf.train <- train.out
return(ret)

}
