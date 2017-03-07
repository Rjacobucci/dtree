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
                 weights=NULL){

  res <- list()

  if(stablelearner==FALSE){
    print(2)
    for(i in 1:n.rep){
      set.seed(i)
      ids <- sample(nrow(data),nrow(data),replace=TRUE)
      out[[i]] <- dtree(formula,data[ids,],methods,samp.method,
                        tuneLength,subset,perc.sub,prune,weights)$return.matrix
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
    print(1)
    out <- list()
    tree <- dtree(formula,data,methods,samp.method,
             tuneLength,subset,perc.sub,weights)$rpart.out
    print(tree)
    out <- stablelearner::stabletree(tree)
    res$out <- out
  }



return(res)


}
