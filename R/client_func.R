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
#' @param x_cent centered dataset
#' @param value output coefficient matrix from geigen
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
  
  cvx_x_cross =  crossprod(x_cent, cv)
  rows = colnames(x_cent)
  rownames(cvx_x_cross) = rows

  return(cvx_x_cross)

}


#' @title Federated ComDim
#' @param x_cent centered dataset
#' @param cvx canonical variate from x_cent dataset
#' @return cv_x_cross as crossprod(cvx, x_cent)
#' @export 
ds.nRows <- function(x){
 x = as.matrix(x)
 return(nrow(x))
}
