#' Main function for creating different types of decision trees
#'
#' @param formula a formula, weight a response to left of ~.
#' @param data Data frame to run models on
#' @param methods Which tree methods to use. Defaults to all:
#'        rpart, tree, ctree, evtree
#' @param weights Optional weights for each case.
#' @param frac.sub What fraction of data to put into train dataset. 1-frac.sub
#'        is allocated to test dataset.
#' @param prune Whether to prune rpart tree
#'
#' @export
#' @import rpart
#' @import tree
#' @import party
#' @import evtree
#' @import caret
#'


dtree = function(formula,
                 data,
                 methods=c("rpart","tree","ctree","evtree"),
                 weights,
                 perc.sub=.5,
                 prune=TRUE){

  ret <- list()

  return.matrix <- matrix(NA,length(methods),8)
  rownames(return.matrix) <- methods
  colnames(return.matrix) <- c("nodes","nvar","nsplits","misfit.cv","misfit.train","rsq.train","misfit.test","rsq.test")


  ids <- sample(nrow(data),nrow(data)*perc.sub)
  data.train <- data[ids,]
  data.test <- data[-ids,]



  # parse out the response variable name
  getResponseFormula <- function (object)
  {
    form <- formula(object)
    if (!(inherits(form, "formula") && (length(form) == 3))) {
      stop("'form' must be a two-sided formula")
    }
    eval(parse(text = paste("~", deparse(form[[2]]))))
  }

  response <- attr(terms(getResponseFormula(formula)),"term.labels")
  # ------------------------------------------------------------------


  class.response <- class(data.train[,response])



  # ------------------------------------------------------------------

  # Rpart

  # ----------------------------------------------------------------


  if(any(methods=="rpart")){

  ret1 <- rpart_ret(formula, data.train,data.test, prune, class.response,response)
  return.matrix["rpart",] <- ret1$vec
  ret$rpart.out <- ret1$rpart.ret



  }

  # --------------------------------------------------

  # Tree

  # --------------------------------------------------

  if(any(methods == "tree")){




  }

  # ----------------------------------------------------

  # Ctree

  # ----------------------------------------------------

  if(any(methods == "ctree")){
    ret3 <- ctree_ret(formula, data.train,data.test, class.response,response)
    return.matrix["ctree",] <- ret3$vec
    ret$ctree.out <- ret3$ctree.ret
  }


  #----------------------------------------------------

  # Evtree

  # ---------------------------------------------------

  if(any(methods == "evtree")){
    ret4 <- evtree_ret(formula, data.train,data.test, class.response,response)
    return.matrix["evtree",] <- ret4$vec
    ret$evtree.out <- ret4$evtree.ret
  }



  ret$response.type <- class.response
  ret$return.matrix <- return.matrix
  ret$call <- match.call()
  class(ret) <- "dtree"
  return(ret)

}
