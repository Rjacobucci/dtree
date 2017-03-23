CtreePathFunc <- function (ct, data) {

  ResulTable <- data.frame(Node = character(), Path = character())

  for(Node in unique(where(ct))){
    # Taking all possible non-Terminal nodes that are smaller than the selected terminal node
    NonTerminalNodes <- setdiff(1:(Node - 1), unique(where(ct))[unique(where(ct)) < Node])


    # Getting the weigths for that node
    NodeWeights <- nodes(ct, Node)[[1]]$weights


    # Finding the path
    Path <- NULL
    for (i in NonTerminalNodes){
      if(any(NodeWeights & nodes(ct, i)[[1]][2][[1]] == 1)) Path <- append(Path, i)
    }

    # Finding the splitting creteria for that path
    Path2 <- SB <- NULL

    for(i in 1:length(Path)){
      if(i == length(Path)) {
        n <- nodes(ct, Node)[[1]]
      } else {n <- nodes(ct, Path[i + 1])[[1]]}

      if(all(data[which(as.logical(n$weights)), as.character(unlist(nodes(ct,Path[i])[[1]][[5]])[length(unlist(nodes(ct,Path[i])[[1]][[5]]))])] <= as.numeric(unlist(nodes(ct,Path[i])[[1]][[5]])[3]))){
        SB <- "<="
      } else {SB <- ">"}
      Path2 <- paste(c(Path2, paste(as.character(unlist(nodes(ct,Path[i])[[1]][[5]])[length(unlist(nodes(ct,Path[i])[[1]][[5]]))]),
                                    SB,
                                    as.character(unlist(nodes(ct,Path[i])[[1]][[5]])[3]))),
                     collapse = ", ")
    }

    # Output
    ResulTable <- rbind(ResulTable, cbind(Node = Node, Path = Path2))
  }
  return(ResulTable)

}


#yy = CtreePathFunc(tt,data)


#for(i in 1:nrow(yy)){
#  pp <- list()
#  pp <- c(pp,unique(unlist(strsplit(levels(yy[i,2]), ","))))
#}

#matt <- matrix(NA,length(pp),2)
#for(j in 1:length(pp)){
#
#  rr <- strsplit(pp[[j]],"[> <= ]" )
#  rr2 <- rr[[1]][rr[[1]]!=""]
#  matt[j,1] <- rr2[1]
#  matt[j,2] <- as.numeric(rr2[2])
#}

