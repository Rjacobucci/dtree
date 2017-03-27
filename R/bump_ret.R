
bump_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response,Metric,bump.rep){

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

  #bump.out <- bump(formula,data.train)
  bump.list <- list()
  bump.matrix <- matrix(NA,bump.rep,7)
  for(i in 1:bump.rep){
    set.seed(i)
    ids <- sample(nrow(data.train),nrow(data.train),replace=TRUE)
    train.out <- train(formula,data.train[ids,],method="rpart",tuneLength=tuneLength,
                       trControl=ctrl,metric=Metric)
    bump.list[[i]] <- train.out$finalModel
    bump.out <- train.out$finalModel
    bump.ret <- bump.out

    cp <- bump.out$cptable
    min.error <- which(min(cp[,"rel error"]) == cp[,"rel error"])[1]
    return.matrix[1,"nsplits"] <- cp[min.error,"nsplit"]
    #train.out$varImp


    vars <- bump.ret$frame[,"var"]
    vars2 <- vars[vars != "<leaf>"]
    return.matrix[1,"nvar"] <- length(unique(vars2))

    return.matrix[1,"nodes"] <- length(vars[vars == "<leaf>"])

    return.splits <- list()

    if(cp[min.error,"nsplit"] == 0){
      return.splits <- NA
    }else{
      hh <- rpart.utils::rpart.subrules.table(bump.out)

      hh2 <- hh[is.na(hh$Less == FALSE),]
      hh3 <- hh2[,c("Variable","Value")]
      colnames(hh3) <- c("var","val")
      hh3[,1] <- as.character(hh3[,1])
      hh3[,2] <- round(as.numeric(as.character(hh3[,2])),3)
      row.names(hh3) <- c()
      return.splits <- hh3
    }



    ind <- as.numeric(row.names(train.out$bestTune))
    if(class.response == "numeric" | class.response == "integer"){
      #which(train.out$results[,"cp"] == train.out$bestTune)

      return.matrix[1,"rmse.samp"] <- train.out$results[ind,"RMSE"]
      #return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(bump.out))^2)/nrow(data.train)
      return.matrix[1,"rsq.samp"] <- train.out$results[ind,"Rsquared"]

      if(subset==FALSE){
        return.matrix[1,"rmse.test"] <- NA
        return.matrix[1,"rsq.test"] <- NA
      }else{
        return.matrix[1,"rmse.test"] <- mean((data.test[,response] -
                                                predict(bump.out,data.test))^2)/nrow(data.test)
        return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(bump.out,data.test)))**2
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
    bump.matrix[i,] <- return.matrix
    colnames(bump.matrix) <- colnames(return.matrix)

  }





  ret$bump.list <- bump.list
  ret$bump.matrix <- bump.matrix
  return(ret)

}
