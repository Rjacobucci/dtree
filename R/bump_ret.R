
bump_ret <- function(formula, data.train, data.test,samp.method,tuneLength,subset,class.response, response,Metric,bump.rep){

  ret <- list()
  return.splits <- list()

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
                       trControl=ctrl,metric=Metric,na.action=na.pass)
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



    if(cp[min.error,"nsplit"] == 0){
      return.splits2 <- as.data.frame(matrix(NA,1,2))
      colnames(return.splits2) <- c("var","val")
      return.splits2[1,1] <- "no split"
      return.splits2[1,2] <- 0
      return.splits2[1,1] <- as.character(return.splits2[1,1])
      return.splits2[1,2] <- as.numeric(as.character(return.splits2[1,2]))
      return.splits[[i]] <- return.splits2
    }else{
      hh <- rpart.utils::rpart.subrules.table(bump.out)

      hh2 <- hh[is.na(hh$Less == FALSE),]
      hh3 <- hh2[,c("Variable","Value")]
      colnames(hh3) <- c("var","val")
      hh3[,1] <- as.character(hh3[,1])
      hh3[,2] <- round(as.numeric(as.character(hh3[,2])),3)
      row.names(hh3) <- c()
      return.splits[[i]] <- hh3
    }



    ind <- as.numeric(row.names(train.out$bestTune))
    if(class.response == "numeric" | class.response == "integer"){
      #which(train.out$results[,"cp"] == train.out$bestTune)
      return.matrix[1,"rmse.samp"] <- sqrt(mean((data.train[,response] - predict(train.out,data.train))^2))
      #return.matrix[1,"misfit.train"] <- mean((data.train[,response] - predict(bump.out))^2)/nrow(data.train)
      pp = predict(train.out,data.train)
      if(sd(pp)==0) pp <- pp+rnorm(length(pp),0,.000001)
      return.matrix[1,"rsq.samp"] <- (cor(data.train[,response],pp))**2

      if(subset==FALSE){
        return.matrix[1,"rmse.test"] <- NA
        return.matrix[1,"rsq.test"] <- NA
      }else{
        return.matrix[1,"rmse.test"] <- mean((data.test[,response] -
                                                predict(bump.out,data.test))^2)/nrow(data.test)
        return.matrix[1,"rsq.test"] <- (cor(data.test[,response],predict(bump.out,data.test)))**2
      }
    }else{
      return.matrix[1,"auc.samp"] <- pROC::auc(data.train[,response],predict(train.out,data.train,type="prob")[,1])
      return.matrix[1,"accuracy.samp"] <- caret::confusionMatrix(data.train[,response],predict(train.out,data.train))$overall["Accuracy"]

      if(subset==FALSE){
        return.matrix[1,"auc.test"] <- NA
      }else{
        return.matrix[1,"auc.test"] <- NA
      }
    }
    bump.matrix[i,] <- return.matrix
    colnames(bump.matrix) <- colnames(return.matrix)

  }

  if(class.response == "numeric" | class.response == "integer"){
    loc <-which(min(bump.matrix[,4]) == bump.matrix[,4])[1]
  }else{
    loc <-which(max(bump.matrix[,4]) == bump.matrix[,4])[1]
  }

  if(bump.matrix[loc,2] > 0){
    rtree = rpart.utils::rpart.subrules.table(bump.list[[loc]])[1,2:5]
    rtree2 <- matrix(rtree[is.na(rtree)==FALSE],1,2)
    colnames(rtree2) <- c("var","val")
    rtree2[,1] <- as.character(rtree2[,1])
    rtree2[,2] <- round(as.numeric(as.character(rtree2[,2])),3)
    ret$return.splits <- return.splits[[loc]]
  }else{
    rtree2 <- c("no split",0)

    return.splits <- as.data.frame(matrix(NA,1,2))
    colnames(return.splits) <- c("var","val")
    return.splits[1,1] <- "no split"
    return.splits[1,2] <- 0
    return.splits[1,1] <- as.character(return.splits[1,1])
    return.splits[1,2] <- as.numeric(as.character(return.splits[1,2]))
    ret$return.splits <- return.splits
  }


  ret$firstSplit <- rtree2
  BestMod <- ret$bump.list[[loc]]
  ret$BestMod <- BestMod
  ret$vec <- bump.matrix[loc,]
  ret$bump.list <- bump.list
  ret$bump.matrix <- bump.matrix
  return(ret)

}
