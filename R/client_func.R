#' @title Federated ComDim
#' @param x first block
#' @param y second block
#' @return crossprod between x and y
#' @export 
crossmatrix <- function(x,y = NULL){

	if (is.null(y)){
	return(crossprod(as.matrix(x)))
	}
	
	else{
	return(crossprod(as.matrix(x), as.matrix(y)))

	}

}

#' @title Federated ComDim
#' @param x first block
#' @return x
#' @export 
mirror <- function(x){
	return(x)
}

