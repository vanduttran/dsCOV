#' @title Federated ComDim
#' @param x first block
#' @param y second block
#' @return crossprod between x and y and as attribute the dimension of the original dataset
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

#' @title Federated ComDim
#' @param x_cent centered dataset
#' @param value output coefficient matrix from CCA
#' @return CV canonical variates 
#' @export 
canVar <- function(x_cent, value) {
  valued <- dsSwissKnife:::.decode.arg(value)
  if (is.list(valued)) valued <- do.call(rbind, valued)
  
  CV = x_cent %*% valued
  
  return(CV)
}

#' @title Federated ComDim
#' @param x_cent centered dataset
#' @param cvx canonical variate from x_cent dataset
#' @return cv_x_cross as crossprod(cvx, x_cent)
#' @export 
hybridCrossmatrix <- function(x_cent, cvx) {
  cv <- dsSwissKnife:::.decode.arg(cvx)
  if (is.list(cv)) cv <- do.call(rbind, cv)
  
  cvx_cross = crossprod(cv)
  x_cross = crossprod(x_cent)
  attr(x_cross, "x_cent.dim") = dim(x_cent)
  
  cvx_x_cross =  crossprod(x_cent, cv)
  
  return(list(cvx_cross = cvx_cross, x_cross=x_cross, cvx_x_cross = cvx_x_cross))

}
