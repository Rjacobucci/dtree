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
#' @param bump.rep Number of repetitions for bumping
#' @param parallel Whether to run all reps in parallel
#' @param ncore Number of cores to use
#' @param roundVal How much to round cut points when calculating stability
#' @param stablelearner Whether or not to use the stablelearner package to
#'        calculate stability
#' @param subset Whether to subset
#' @param perc.sub What fraction of data to put into train dataset. 1-frac.sub
#'        is allocated to test dataset. Defaults to 0.75
#' @param weights Optional weights for each case.
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster
#' @export
#' @examples
#' \dontrun{
#' library(MASS) # for boston data
#' data(Boston)
#' stab.out <- stable(formula=medv ~.,data=Boston,
#'             methods=c("rpart"),samp.method="cv",
#'             tuneLength=2, n.rep=5, parallel=TRUE)
#' stab.out
#' }



stable = function(formula,
                 data,
                 methods=c("lm","rpart","tree","ctree","evtree"),
                 samp.method="repeatedcv",
                 tuneLength=3,
                 n.rep=100,
                 bump.rep=50,
                 parallel=FALSE,
                 ncore=detectCores() - 1,
                 roundVal = 1,
                 stablelearner=FALSE,
                 subset=FALSE,
                 perc.sub=.75,
                 weights=NULL){

  res <- list()
  out <- list()
  out2 <- list()
  firSplit <- list()

  if(stablelearner==FALSE){


    if(parallel==FALSE){
      for(i in 1:n.rep){
        set.seed(i)
        print(i)
        ids <- sample(nrow(data),nrow(data),replace=TRUE)
        tt <- try(dtree(formula,data[ids,],methods,samp.method,
                          tuneLength,bump.rep,subset,perc.sub,weights,verbose=FALSE))
        if(inherits(tt, "try-error")){
          out[[i]] <- NULL
          out2[[i]] <- NULL
          firSplit[[i]] <- NULL
        }else{
          print(tt)
          out[[i]] <- tt
          print(out[[i]]$return.matrix)
          out2[[i]] <- out[[i]]$return.matrix
          firSplit[[i]] <- out[[i]]$firstSplit
        }

      }

      out <- out[lapply(out,is.null)==FALSE]
      out2 <- out2[lapply(out2,is.null)==FALSE]
      firSplit <- firSplit[lapply(firSplit,is.null)==FALSE]
      ret <- array(NA, dim=c(n.rep,length(methods),7))

      for(j in 1:length(out)){
        ret[j,,] <- out2[[j]]
      }
    }else{
      data.rep <- list()
      for(i in 1:n.rep){
        set.seed(i)
        ids <- sample(nrow(data),nrow(data),replace=TRUE)
        data.rep[[i]] <- data[ids,]
      }
      #library(parallel)
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores)
      e <- new.env()
      clusterExport(cl, c("formula","methods","samp.method","tuneLength","bump.rep","subset","perc.sub","weights"),envir = e)
      par.fun <- function(data){
        library(dtree)
        tt <- try(dtree(formula,data,methods,samp.method,
                        tuneLength,bump.rep,subset,perc.sub,weights))
        if(inherits(tt, "try-error")){
          tt <- NULL
          tt
        }else{
          tt
        }
      }
      out <- list()
      out <- parLapply(cl, data.rep,par.fun)
      stopCluster(cl)
      out <- out[lapply(out,is.null)==FALSE]
      out2 <- list()
      ret <- array(NA, dim=c(length(out),length(methods),7))
      firSplit <- list()
      for(i in 1:length(out)){
        out2[[i]] <- out[[i]]$return.matrix
        firSplit[[i]] <- out[[i]]$firstSplit
        ret[i,,] <- out2[[i]]
      }
    }


    ret.mean <- apply(ret,3,colMeans,na.rm=TRUE)
    ret.var <- apply(ret,3,matrixStats::colVars,na.rm=TRUE)

    ret.mean <- matrix(ret.mean,length(methods),7)
    ret.var <- matrix(ret.var,length(methods),7)

    stability <- matrix(NA,1,length(methods))
    colnames(stability) <- methods

    row.names(ret.mean) <- methods
    row.names(ret.var) <- methods

    colnames(ret.mean) <- colnames(out2[[i]])
    colnames(ret.var) <- colnames(out2[[i]])


    tt = terms(x=formula,data=data)
    preds <- attr(tt,"term.labels")

    counts.mean <- matrix(NA,length(methods),length(preds))
    counts.var <- matrix(NA,length(methods),length(preds))
    rownames(counts.mean) <- rownames(counts.var) <- methods
    colnames(counts.mean) <- colnames(counts.var) <- preds




    if(any(methods==c("ctree"))){
      var.count <- matrix(NA,length(out),length(preds))
      colnames(var.count) <- preds
      where.ctree <- list()
      for(i in 1:length(out)){
        hh <- out[[i]]$ctree.splits
        hh[,2] <- round(hh[,2],roundVal)
        tab <- table(hh[,1])
        where.ctree[[i]] <- hh

        for(j in 1:length(preds)){
          var.count[i,preds[j]] <- tab[preds[j]]
          if(is.na(var.count[i,preds[j]]==TRUE)) var.count[i,preds[j]] <- 0
        }
      }

      counts.mean["ctree",] <- colMeans(var.count)
      counts.var["ctree",] <- round(matrixStats::colVars(var.count),2)
      nn <- plyr::ldply(where.ctree)
      if(length(unique(nn[,1])) == 1){
        res$where.ctree <- table(nn)
      }else{
        res$where.ctree <- sapply(split(nn, nn$var),table)
      }
      stability[,"ctree"] <- 1-length(unique(where.ctree))/n.rep
    }

    tt.array <- simplify2array(firSplit)
    #print(tt.array[methods,,])
    firstSplit <- list()
    for(i in 1:length(methods)){
    # firstSplit[[methods[i]]] <-  table(matrix(tt.array[methods[i],,],length(methods),2)[1,])
      firstSplit[[methods[i]]] <-  table(tt.array[methods[i],,][1,])
    }




    if(any(methods==c("rpart"))){
      var.count <- matrix(NA,length(out),length(preds))
      colnames(var.count) <- preds
      where.rpart <- list()




      for(i in 1:length(out)){
        hh <- out[[i]]$rpart.splits
      #  if(is.na(hh)){
       #
       # }
        hh[,2] <- round(hh[,2],roundVal)
        tab <- table(hh[,1])
        where.rpart[[i]] <- hh

        for(j in 1:length(preds)){
          var.count[i,preds[j]] <- tab[preds[j]]
          if(is.na(var.count[i,preds[j]]==TRUE)) var.count[i,preds[j]] <- 0
        }
      }

      counts.mean["rpart",] <- colMeans(var.count)
      counts.var["rpart",] <- round(matrixStats::colVars(var.count),2)
      nn <- plyr::ldply(where.rpart)
      if(length(unique(nn[,1])) == 1){
        res$where.rpart <- table(nn)
      }else{
        res$where.rpart <- sapply(split(nn, nn$var),table)
      }
      stability[,"rpart"] <- 1-length(unique(where.rpart))/length(out)
    }



    if(any(methods==c("evtree"))){
      var.count <- matrix(NA,length(out),length(preds))
      colnames(var.count) <- preds
      where.evtree <- list()

      for(i in 1:length(out)){
        hh <- out[[i]]$evtree.splits
        hh[,2] <- round(hh[,2],roundVal)
        tab <- table(hh[,1])
        where.evtree[[i]] <- hh


        for(j in 1:length(preds)){
          var.count[i,preds[j]] <- tab[preds[j]]
          if(is.na(var.count[i,preds[j]]==TRUE)) var.count[i,preds[j]] <- 0
        }
      }

      counts.mean["evtree",] <- colMeans(var.count)
      counts.var["evtree",] <- round(matrixStats::colVars(var.count),2)
      nn <- plyr::ldply(where.evtree)
      nn[,1] <- as.character(nn[,1])
      if(length(unique(nn[,1])) == 1){
        res$where.evtree <- table(nn)
      }else{
        res$where.evtree <- sapply(split(nn, nn$var),table)
      }
      stability[,"evtree"] <- 1-length(unique(where.evtree))/length(out)
    }


    if(any(methods==c("ctreePrune"))){
      var.count <- matrix(NA,length(out),length(preds))
      colnames(var.count) <- preds
      where.ctreePrune <- list()
      for(i in 1:length(out)){
        hh <- out[[i]]$ctreePrune.splits
        hh[,2] <- round(hh[,2],roundVal)
        tab <- table(hh[,1])
        where.ctreePrune[[i]] <- hh


        for(j in 1:length(preds)){
          var.count[i,preds[j]] <- tab[preds[j]]
          if(is.na(var.count[i,preds[j]]==TRUE)) var.count[i,preds[j]] <- 0
        }
      }

      counts.mean["ctreePrune",] <- colMeans(var.count)
      counts.var["ctreePrune",] <- round(matrixStats::colVars(var.count),2)
      nn <- plyr::ldply(where.ctreePrune)
      nn[,1] <- as.character(nn[,1])
      if(length(unique(nn[,1])) == 1){
        res$where.ctreePrune <- table(nn)
      }else{
        res$where.ctreePrune <- sapply(split(nn, nn$var),table)
      }
      stability[,"ctreePrune"] <- 1-length(unique(where.ctreePrune))/length(out)
    }


    if(any(methods==c("bump"))){
      var.count <- matrix(NA,length(out),length(preds))
      colnames(var.count) <- preds
      where.bump <- list()

      for(i in 1:length(out)){

        hh <- out[[i]]$bump.splits
        hh[,2] <- round(hh[,2],roundVal)
        tab <- table(hh[,1])
        where.bump[[i]] <- hh

        for(j in 1:length(preds)){
          var.count[i,preds[j]] <- tab[preds[j]]
          if(is.na(var.count[i,preds[j]]==TRUE)) var.count[i,preds[j]] <- 0
        }
      }

      counts.mean["bump",] <- colMeans(var.count)
      counts.var["bump",] <- round(matrixStats::colVars(var.count),2)
      nn <- plyr::ldply(where.bump)
      if(length(unique(nn[,1])) == 1){
        res$where.bump <- table(nn)
      }else{
        res$where.bump <- sapply(split(nn, nn$var),table)
      }
      stability[,"bump"] <- 1-length(unique(where.bump))/length(out)
    }

    res$n.ret <- length(out)
    res$firstSplit <- firstSplit
    res$stability <- stability
    res$counts.mean <- counts.mean
    res$counts.var <- counts.var
    res$means <- round(ret.mean,3)
    res$variances <- round(ret.var,3)
    res
  }else{

    stop("Curently not working")
    methods2=methods
    if(any(methods2==c("lm","rf"))) stop("only decision tree methods can be used with stable learner")

    trees <- dtree(formula,data,methods=methods2,samp.method,
          tuneLength,subset,perc.sub,prune,weights)


    if(any(methods2==c("rpart"))){
      formula2 <- terms(formula,data=data)
      tree1 <- rpart(formula(formula2),data)
      tree2 <- prune(tree1,cp=as.numeric(trees$rpart.train$bestTune))
     # res$rpart <- stablelearner::stabletree(tree2,data=data,B=100)
    }

    if(any(methods2==c("ctree"))){
     # formula2 <- terms(formula,data=data)
      #val = 1-as.numeric(trees$ctree.train$bestTune)
      #ctrl=ctree_control()#(mincriterion=val)
     # treet <- partykit::ctree(formula(formula2),data=data)#,control=ctrl)
     # res$ctree <- stablelearner::stabletree(treet,data=data,formula=formula(formula2))
      tt = train(formula, data=data,method="ctree")
      tree1 <- partykit::ctree(formula, data=data,
                               control=ctree_control(mincriterion=as.numeric(tt$bestTune)))
    # stablelearner::stabletree(tree1)
    }

    if(any(methods2==c("evtree"))){
      tree <- dtree(formula,data,methods="evtree",samp.method,
                    tuneLength,subset,perc.sub,prune,weights)$evtree.out
      #res$evtree <- stablelearner::stabletree(tree,data=data,B=100)
    }


  }



return(res)


}
