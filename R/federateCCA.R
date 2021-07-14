#' @title Federated CCA
#' @description Function for ComDim federated analysis on the virtual cohort combining multiple cohorts
#' Finding common dimensions in multitable data (Xk, k=1...K)
#' @usage federareCCA(loginata,groupxy,groupx,groupy,lambda1=0,lambda2 = 0,threshold=1e-10)
#'
#' @param logindata Login information of the FD server
#' @param Var complete set of features
#' @param Varx set of features for x dataset
#' @param Vary set of features for y dataset
#' @param lambda1 parameter to regularize x dataset
#' @param lambda2 parameter to regularize y dataset
#' @param TOL tolerance
#' @param size I DONT KNOW
#' @return XX
#' @importFrom utils setTxtProgressBar
#' @importFrom DSI datashield.aggregate
#' @import geigen
#' @export


federateCCA <- function(logindata,Var, Varx, Vary, lambda1 = 0, lambda2 = 0, size = NA, TOL = 1e-10) {

  opals <- datashield.login(logins=logindata)
  nNode <- length(opals)
  querytable <- unique(logindata$table)

  datashield.assign(opals, 'rawData', querytable,
                    variables=VAR, async=T)

  dssSubset('filtered', 'rawData', row.filter = 'complete.cases(rawData)', datasources = opals)

  dssSubset('x', 'filtered',col.filter = Varx, datasources = opals)
  dssSubset('y', 'filtered', col.filter = Vary , datasources = opals)

  datashield.symbols(opals)

  datashield.assign(opals, "x_cent", as.symbol('center(x)'), async=T)
  datashield.assign(opals, "y_cent", as.symbol('center(y)'), async=T)

  cxl = datashield.aggregate(opals, as.symbol('crossmatrix(x_cent)'), async=T)
  cyl = datashield.aggregate(opals, as.symbol('crossmatrix(y_cent)'), async=T)
  cxyl = datashield.aggregate(opals, as.symbol('crossmatrix(x_cent,y_cent)'), async=T)
    
  merge_cov <- function(lx){
    lx = lapply(lx, as.matrix)

    cxs = Reduce("+", lx)
    n.rowx = Reduce("+",lapply(lx, function(x){attributes(x)$rawData.dim[1]}))
    Cx = cxs/(n.rowx-1)

    return(Cx)

  }

  Cxx <- merge_cov(cxl) + diag(lambda1, ncol(cxl[[1]]) )
  Cyy <- merge_cov(cyl) + diag(lambda2, ncol(cyl[[1]]) )
  Cxy <- merge_cov(cxyl)

  res <- geigen(Cxy, Cxx, Cyy)
  names(res) <- c("cor", "xcoef", "ycoef")

   
  datashield.symbols(opals)
  #copute canonical variates
  cvx= datashield.aggregate(opals, as.call(list(as.symbol("canVar"),
                                                as.symbol("x_cent"),
                                                .encode.arg(res$xcoef))), async=T)

  cvy= datashield.aggregate(opals, as.call(list(as.symbol("canVar"),
                                                as.symbol("y_cent"),
                                                .encode.arg(res$ycoef))), async=T)
  cvx_x_cross = sapply(names(opals), function(x){

    datashield.aggregate(opals[x], as.call(list(as.symbol("hybridCrossmatrix"),
                                                as.symbol("x_cent"),
                                                .encode.arg(cvx[[x]]) )), async=T)
  })


  cvx_y_cross = sapply(names(opals), function(x){

    datashield.aggregate(opals[x], as.call(list(as.symbol("hybridCrossmatrix"),
                                                as.symbol("y_cent"),
                                                .encode.arg(cvx[[x]]) )), async=T)

  })

  cvy_y_cross = sapply(names(opals), function(x){

    datashield.aggregate(opals[x], as.call(list(as.symbol("hybridCrossmatrix"),
                                                as.symbol("y_cent"),
                                                .encode.arg(cvy[[x]]) )), async=T)

  })

  cvy_x_cross = sapply(names(opals), function(x){

    datashield.aggregate(opals[x], as.call(list(as.symbol("hybridCrossmatrix"),
                                                as.symbol("x_cent"),
                                                .encode.arg(cvy[[x]]) )), async=T)

  })


  computLoadings <- function(cvx, Cxx, cvx_x_cross ){


    cvx_var = Reduce("+", lapply(cvx, crossprod))/(Reduce("+", lapply(cvx, nrow))-1) #omit because it must be 1
    cvx_x_var = Reduce("+", cvx_x_cross)/ (Reduce("+", lapply(cvx, nrow))-1)


    inv_var_x = diag(1/sqrt(diag(Cxx)), ncol(Cxx), ncol(Cxx))
    inv_var_cvx = diag(1/sqrt(diag(cvx_var)), ncol(cvx_var), ncol(cvx_var))

    loadx = inv_var_x %*% cvx_x_var %*% inv_var_cvx
    rownames(loadx) = rownames(Cxx)
    return(loadx)

  }

  load.xx = computLoadings(cvx, Cxx, cvx_x_cross )
  load.xy = computLoadings(cvx, Cyy, cvx_y_cross )

  load.yy = computLoadings(cvy, Cyy, cvy_y_cross )
  load.yx = computLoadings(cvy, Cxx, cvy_x_cross )
   
  return(list(cor = res$cor, xcoef = res$xcoef,
              ycoef = res$ycoef, cv = list(cvx = rbind(cvx[[1]], cvx[[2]]), cvy = rbind(cvy[[1]], cvy[[2]])),
              loadings = list(load.cvx.x = load.xx, load.cvy.y= load.yy,
                                load.cvx.y = load.xy, load.cvy.x = load.yx)))


  
}





