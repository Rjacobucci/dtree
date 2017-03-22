#' Main function to calculate stability coefficients
#'
#' @param formula a formula, weight a response to left of ~.
#' @param data Data frame to run models on
#' @param methods Which tree methods to use. Defaults:
#'        lm, rpart, tree, ctree, evtree. Also can use "rf" for random forests
#' @param samp.method Sampling method. Refer to caret package trainControl()
#'        documentation. Default is repeated cross-validation. Other options
#'        include "cv" and "boot".
#' @param tuneLength Number of tuning parameters to try. Applies to train()
#' @param n.rep Number of times to replicate each method
#' @param stablelearner Whether or not to use the stablelearner package to
#'        calculate stability
#' @param subset Whether to subset
#' @param perc.sub What fraction of data to put into train dataset. 1-frac.sub
#'        is allocated to test dataset. Defaults to 0.75
#' @param weights Optional weights for each case.
#' @param verbose
#'
#'
#'
#' @export



stable = function(formula,
                 data,
                 methods=c("lm","rpart","tree","ctree","evtree"),
                 samp.method="repeatedcv",
                 tuneLength=3,
                 n.rep=100,
                 stablelearner=FALSE,
                 subset=FALSE,
                 perc.sub=.75,
                 weights=NULL,
                 verbose=FALSE){

  res <- list()

  if(stablelearner==FALSE){
    for(i in 1:n.rep){
      set.seed(i)
      print(i)
      ids <- sample(nrow(data),nrow(data),replace=TRUE)
      out[[i]] <- dtree(formula,data[ids,],methods,samp.method,
                        tuneLength,subset,perc.sub,prune,weights,verbose)$return.matrix
    }
    ret <- array(NA, dim=c(n.rep,length(methods),ncol(out[[1]])))

    for(j in 1:n.rep){
      ret[j,,] <- out[[j]]
    }

    ret.mean <- apply(ret,3,colMeans,na.rm=TRUE)
    ret.var <- apply(ret,3,matrixStats::colVars,na.rm=TRUE)

    ret.mean <- matrix(ret.mean,length(methods),7)
    ret.var <- matrix(ret.var,length(methods),7)

    row.names(ret.mean) <- methods
    row.names(ret.var) <- methods

    colnames(ret.mean) <- colnames(out[[i]])
    colnames(ret.var) <- colnames(out[[i]])



    res$means <- round(ret.mean,3)
    res$variances <- round(ret.var,3)
    res
  }else{
    methods2=methods
    if(any(methods2==c("lm","rf"))) stop("only decision tree methods can be used with stable learner")

    trees <- dtree(formula,data,methods=methods2,samp.method,
          tuneLength,subset,perc.sub,prune,weights,verbose)


    if(any(methods2==c("rpart"))){
      formula2 <- terms(formula,data=data)
      tree1 <- rpart(formula(formula2),data)
      tree2 <- prune(tree1,cp=as.numeric(trees$rpart.train$bestTune))
      res$rpart <- stablelearner::stabletree(tree2,data=data,B=100)
    }

    if(any(methods2==c("ctree"))){
     # formula2 <- terms(formula,data=data)
      #val = 1-as.numeric(trees$ctree.train$bestTune)
      #ctrl=ctree_control()#(mincriterion=val)
     # treet <- partykit::ctree(formula(formula2),data=data)#,control=ctrl)
     # res$ctree <- stablelearner::stabletree(treet,data=data,formula=formula(formula2))
      tt = train(default ~ ., data=Default,method="ctree")
      tree1 <- partykit::ctree(default ~ income, data=Default,
                               control=ctree_control(mincriterion=as.numeric(tt$bestTune)))
     stablelearner::stabletree(tree1)
    }

    if(any(methods2==c("evtree"))){
      tree <- dtree(formula,data,methods="evtree",samp.method,
                    tuneLength,subset,perc.sub,prune,weights,verbose)$evtree.out
      res$evtree <- stablelearner::stabletree(tree,data=data,B=100)
    }


  }



return(res)


}
