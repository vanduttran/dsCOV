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
#' @param xl list of crossproducts from first block
#' @param yl list of crossproducts from second block
#' @param xyl list of crossproducts betweem first and second block
#' @return x
#' @export 
merge_cov <- function(xl, yl, xyl){
  
  cx = Reduce("+" , xl)
  cx = cx/(nrow(cx)-1)
  
  cy = Reduce("+" , yl)
  cy = cy/(nrow(cy)-1)
  
  cxy = Reduce("+", xyl)
  cxy = cxy/(nrow(cxy)-1)
  
  return(list(cx = cx, cy = cy, cxy =cxy ))
}
