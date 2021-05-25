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
#' @return loadx loadings as cor(cvx, x_cent)
#' @export 
comp_loadings <- function(x_cent, cvx) {
  cv <- dsSwissKnife:::.decode.arg(cvx)
  if (is.list(cv)) cv <- do.call(rbind, cv)
  
  cvx_cent = lapply(cv, function(x){scale(x, scale = F)})
  cvx_cross = Reduce("+", lapply(cvx_cent, crossprod))
  
  x_cross = Reduce("+", crossprod(x_cent))
  
  cvx_x_cross = Reduce("+", crossprod(x_cent, cvx_cent))
  
  # x_cross = datashield.aggregate(opals, as.symbol('crossmatrix(x_cent)'), async=T)
  # tot.x_cross = Reduce("+", x_cross)
  # n.row_x = Reduce("+",lapply(lx, function(x){attributes(x)$rawData.dim[1]}))
  
  var_x = diag(1/sqrt(diag(x_cross)), ncol(x_cent), ncol(x_cent))
  var_cvx = diag(1/sqrt(diag(cvx_cross)), ncol(cvx), ncol(cvx))
  
  tot.var_x = var_x/ (Reduce("+", lapply(x_cent, nrow))-1)
  tot.var_cvx =  var_cvx/ (Reduce("+", lapply(cvx, nrow))-1)
  tot.var_cvx_x = cvx_x_cross/ (Reduce("+", lapply(cvx, nrow))-1)
  
  loadx = tot.var_x %*% tot.var_cvx_x %*% tot.var_cvx
  
  return(loadx)
  
}

#' @title Federated ComDim
#' @param x_cent centered dataset
#' @param cvx canonical variate from x_cent dataset
#' @return cv_x_cross as crossprod(cvx, x_cent)
#' @export 
hybrid.crossmatrix <- function(x_cent, cvx) {
    cv <- .decode.arg(cvx)
    if (is.list(valued)) valued <- do.call(rbind, cv)
    
    cvx_x_cross = crossprod(cv_x_cross)
    
    return(cv_x_cross)
}
