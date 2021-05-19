#' @title Federated ComDim
#' @param x first block
#' @param y second block
#' @return crossprod between x and y
#' @export 
crossmatrix <- function(x,y){
	return(crossprod(as.matrix(x),as.matrix(y)))
}

#' @title Federated ComDim
#' @param x first block
#' @return x
#' @export 
mirror <- function(x){
	return(x)
}

#' @title Federated ComDim
#' @param xl list of centered matrices from first block
#' @param yl list of centered matrices from second block
#' @return Cx
#' @return Cxy
#' @export 
merge_cov <- function(lx,ly = NULL){
#with matrices 1701 x 4, 1701 x 6 and 1290x4, 1290 x6 order of error aoround 10^-4
  
  lx = lapply(lx, as.matrix)
  
  if (is.null(ly)){
    
    #lcent = lapply(lx, function(x) {scale(x, scale = F)}) #data already centered I presume
    cxs = Reduce("+", lapply(lx, crossprod))
    Cx = cxs/Reduce("+", lapply(lx, function(x){nrow(x)-1}))
    
    return(Cx)
  }
  
  else{
    #must be generalized
    ly = lapply(ly, as.matrix)
    
    cxys =  Reduce("+", list(crossprod(lx[[1]], ly[[1]]), crossprod(lx[[2]], ly[[2]])))
    Cxy = cxys/Reduce("+", lapply(lx, function(x){nrow(x)-1}))
    return(Cxy)
  }
}
