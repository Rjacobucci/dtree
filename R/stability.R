#' Main function to calculate stability coefficients
#'
#' @param formula a formula, weight a response to left of ~.
#' @param data Data frame to run models on
#' @param methods Which tree methods to use. Defaults:
#'        lm, rpart, tree, ctree, evtree. Also can use "rf" for random forests
#' @param n.rep Number of times to replicate each method
#' @param subset Whether to subset
#' @param perc.sub What fraction of data to put into train dataset. 1-frac.sub
#'        is allocated to test dataset. Defaults to 0.75
#' @param prune Whether to prune rpart tree
#' @param weights Optional weights for each case.
#'
#'
#'
#' @export



stability = function(formula,
                 data,
                 methods=c("lm","rpart","tree","ctree","evtree"),
                 n.rep=100,
                 subset=FALSE,
                 perc.sub=.75,
                 prune=TRUE,
                 weights=NULL){

  out <- list()
  for(i in 1:n.rep){
    set.seed(i)
    ids <- sample(nrow(data),nrow(data),replace=TRUE)
    out[[i]] <- dtree(formula,data[ids,],methods,subset,perc.sub,prune,weights)$return.matrix
  }



  ret <- array(NA, dim=c(n.rep,length(methods),ncol(out[[1]])))

  for(j in 1:n.rep){
    ret[j,,] <- out[[j]]
  }

  ret.mean <- apply(ret,3,colMeans,na.rm=TRUE)
  ret.var <- apply(ret,3,matrixStats::colVars,na.rm=TRUE)

  ret.mean <- matrix(ret.mean,length(methods),8)
  ret.var <- matrix(ret.var,length(methods),8)

  row.names(ret.mean) <- methods
  row.names(ret.var) <- methods

  colnames(ret.mean) <- colnames(out[[i]])
  colnames(ret.var) <- colnames(out[[i]])


  res <- list()
  res$means <- round(ret.mean,3)
  res$variances <- round(ret.var,3)
  res

}
