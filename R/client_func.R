#' @title Federated ComDim
#' @param x first block
#' @param y second block
#' @return x+y
#' @export 

crossmatrix <- function(x,y){
	return(crossprod(x,y))
}
