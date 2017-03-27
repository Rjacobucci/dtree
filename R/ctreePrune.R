

############################################################
### Function to grow a tree using ctree
### and pruning it using FDR
###
### qstar: numeric: level at which FDR must be controled
###
### size: numeric (0,1) the closer to 1 the bigger the tree
###
############################################################


ctreePrune <- function(formula,data,qstar=0.05,sizeSatu=0.999)  {

  #argum<-list(formula=medv~., data=Boston)
  #argum <- list()
  #####################################################
  #####   changes parameters to grow a very large tree
  #####################################################
  ctrl <- ctree_control(mincriterion=1-sizeSatu)

  #############################
  ### Grows a very large tree
  #############################
  #satTree <- do.call(partykit::ctree,argum)

  satTree <- partykit::ctree(formula=formula, data=data,control=ctrl)


  ############################################################
  #####  Collect relevant information about the saturated tree
  #############################################################
  satTreeInfo <- DFtreeInfo(satTree)


  ##############################
  ### Prune the saturated tree
  ##############################
  temp <- fdrprune(satTree,qstar)
  pruTree <- temp$tree

  ############################################################
  #####  Collect relevant information about the pruned tree
  #############################################################
  pruTreeInfo <- temp$info


  list(tree=pruTree,
       info=pruTreeInfo,
       qstar=qstar,
       sizeSatu=sizeSatu,
       pvalue=temp$pvalue
  )
}

#################################################
###
### Function to obtain a pruned tree using FDR
###
### fit: object of class party
###
#################################################
fdrprune <- function(fit,qstar=0.05){

  ###################################################
  #####  Collect relevant information about the tree
  ###################################################
  datNo<-DFtreeInfo(fit)


  ############################################
  #####  Select the nodes to prune starting
  #####   from the bottom
  ############################################


  ############################
  #### List of terminal nodes
  ############################
  TNlist <- nodeids(fit,terminal=TRUE)

  ############################
  #### List of non-terminal nodes
  ############################
  wher <- nodeids(fit)%in%TNlist
  nonTNlist <- nodeids(fit)[!wher]


  ##############################
  ####  Calculate pvalue for FDR
  ##############################
  pval<-datNo$pvalue
  pval<-pval[!is.na(pval)]
  pval<-sort(pval)

  m<-length(pval)

  qstar<-qstar #### FDR is controlled at qstar*100%

  i<-1:m

  temp <- (i/m)*qstar
  k<-sum(pval <= temp )
  if (!k==0) pvalue<-temp[k] else pvalue<-0

  #######################################
  #### New alpha to declare significance
  #######################################
  alpha<-pvalue



  ######################################
  #####  Identifies what nodes contain
  #####  FDR corrected significant
  #####  p-values
  ######################################
  datNo$FDRsigYN<-datNo$pvalue <= alpha & !is.na(datNo$pvalue)
  wher <- is.na(datNo$pvalue)
  datNo$FDRsigYN[wher]<-NA



  ####################################
  #### Identifies what nodes to prune
  ####################################
  #### loop over all terminal nodes
  for (i in TNlist) {
    j<-i
    #### repeat while parent pvalue is not significant
    while ( (is.na(datNo$pvalue[j]) | datNo$pvalue[j] > alpha) & !datNo$node[j]==1 ) {
      datNo$pruneYN[j]<-TRUE
      j <- datNo$parent[j]
    }

  }



  ############################################################
  ##### Make sure that all the nodes above a significant node
  #####   are non terminal
  ############################################################
  #### List of nodes with significant pvalues
  nonTNlist<-datNo$node [ !datNo$pruneYN ]

  #### loop over all terminal nodes
  for (i in nonTNlist) {
    j<-i
    #### repeat while parent pvalue is not significant
    while ( !datNo$node[j]==1 ) {
      datNo$pruneYN[j]<-FALSE
      j <- datNo$parent[j]
    }
  }


  ##########################################
  #### If all the nodes have to be pruned
  #### except root node, but root node is
  #### not significant then prune also root
  ##########################################
  wher <- datNo$node==1
  if (dim(datNo)[1]==1) {
    datNo$pruneYN[wher]<-TRUE
  } else if ( all(datNo$pruneYN[!wher]) & datNo$pvalue[wher]> alpha ) {
    datNo$pruneYN[wher]<-TRUE
  }


  ########################
  #####  prune the tree
  ########################
  nodesPru<-datNo$node[datNo$pruneYN]
  out <- nodeprune(fit,nodesPru)

  list(tree=out,info=datNo,pvalue=alpha)

}

##############################################################################
### This function collect information about nodes for trees grown using Ctree
##############################################################################
DFtreeInfo<-function(fit) {

  ############################################
  ### Calculates IDs and left and right nodes
  ############################################
  #library(partykit)
  nid <- nodeids(fit)
  datNo <- nodeapply(fit, ids=nid, function(n) {
    #
    if (is.null(kids_node(n))) {
      out <- data.frame(node= nodeids(n)[1],
                        left=NA,
                        right=NA)
    }
    if (!is.null(kids_node(n))) {
      out <- data.frame(node= nodeids(n)[1],
                        left=nodeids(kids_node(n)[[1]])[1],
                        right=nodeids(kids_node(n)[[2]])[1])
    }
    out
  })
  datNo <- do.call("rbind",datNo)



  #########################################
  ### Calculates the name of the covariates
  ### used in each split
  #########################################
  datNo$varName <- NA

  if (!length(fit)==0) {
    tmpNames <- names(data_party(fit))
    temp<-nodeapply(fit,nid, FUN = function(n) {
      tmp <- n$split$varid
      tmpNames[tmp]
    })
    temp<-unlist(temp)
    wher <- datNo$node %in% as.numeric(names(temp))
    datNo$varName[wher]<-temp
  }



  ###############################
  ### extract pvalues from nodes
  ###############################

  if (!dim(datNo)[1]==1) {
    temp<-nodeapply(fit, ids = nodeids(fit), function(n) info_node(n)$p.value)
    pvalues<-sapply(1:length(temp),function(i){
      if (is.null(temp[[i]])) out<-NA else out<-as.numeric(temp[[i]])
    })
    datNo$pvalue<-pvalues
  }

  if (dim(datNo)[1]==1) {
    datNo$pvalue<-NA
  }


  ###############################
  ### Defines terminal nodes
  ###############################
  temp<-nodeids(fit,terminal=TRUE)
  wher <- datNo$node %in% temp
  datNo$terminal[wher]<-TRUE
  datNo$terminal[!wher]<-FALSE


  ###############################
  ### Defines pruneYN column
  ###############################
  datNo$pruneYN<-FALSE


  ###############################
  ### Defines FDRsigYN column
  ###############################
  datNo$FDRsigYN<-NA


  ##########################################
  #### Calculates identity of parent nodes
  ##########################################
  datNo$parent<-NA
  for (i in datNo$node) {
    tmpil<-datNo$left[i]
    tmpir<-datNo$right[i]
    datNo$parent[tmpil]<-i
    datNo$parent[tmpir]<-i
  }



  datNo

}
