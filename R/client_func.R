#' @title Federated ComDim
#' @param x first block
#' @param y second block
#' @return x+y
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
#' @return x
#' @export 
merge_cov <- function(lx,ly = NULL){
#with matrices 1701 x 4, 1701 x 6 and 1290x4, 1290 x6 order of error aoround 10^-4
  
  if (is.null(ly)){
    
    cxs = Reduce("+", lapply(lx, crossprod))
    Cx = cxs/Reduce("+", lapply(lx, function(x){nrow(x)-1}))
    
    return(Cx)
  }
  
  else{
    #must be generalized
    cxys =  Reduce("+", list(crossprod(lx[[1]], ly[[1]]), crossprod(lx[[2]], ly[[2]])))
    Cxy = cxys/Reduce("+", lapply(lx, function(x){nrow(x)-1}))
    return(Cxy)
  }
}
