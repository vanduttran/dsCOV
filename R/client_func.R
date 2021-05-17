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
