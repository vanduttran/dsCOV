#' @title Federated ComDim
#' @param x first block
#' @param y second block
#' @return crossprod between x and y
#' @export 
crossmatrix <- function(x,y = NULL){
  
  if (is.null(y)){
    cross = crossprod(as.matrix(x))
    attr(cross, "rawData.dim") = dim(x)
    return(cross)
  }
  
  else{
    cross = crossprod(as.matrix(x), as.matrix(y))
    attr(cross, "rawData.dim") = dim(x)
    return(cross)          
  }
}
#' @title Federated ComDim
#' @param x first block
#' @return x
#' @export 
mirror <- function(x){
	return(x)
}

