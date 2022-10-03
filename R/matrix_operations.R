#' Normalize a matrix such that columns or rows sum to 1
#'
#' @param mat input matrix
#' @param across one of c('cols','rows')
#'
#' @return normalized matrix
#' @export
normalizeMat <- function(mat, across = 'cols') {
  if (across == 'cols') {
    colsums_mat <- matrix(
      rep(colSums(mat),dim(mat)[1]),
      nrow = dim(mat)[1],
      byrow = TRUE
    )
    mat <- mat/colsums_mat
    return(mat)
  } else if (across == 'rows') {
    rowsums_mat <- matrix(
      rep(rowSums(mat),dim(mat)[2]),
      nrow = dim(mat)[1],
      byrow = FALSE
    )
    mat <- mat/rowsums_mat
    return(mat)
  } else {
    stop('`across` must be one of c("cols","rows"')
  }
}

#' Multiply a vector to each row or column of a matrix
#'
#' @param mat matrix
#' @param vec vector to multiply matrix by
#' @param across one of c("cols", "rows")
#'
#' @return matrix with each row or column multiplied by input vector
#' @export
multiplyAcross <- function(mat, vec, across = 'cols') {
  stopifnot(
    is.vector(vec),
    length(vec) > 0,
    is.numeric(vec[0]),
    is.numeric(mat)
  )
  if (across == 'rows') {
    stopifnot(length(vec) == ncol(mat))

    mat <- mat*matrix(
      rep(vec,nrow(mat)),
      ncol = ncol(mat),
      byrow = TRUE
    )
    return(mat)

  } else if (across == 'cols') {
    stopifnot(length(vec) == nrow(mat))

    mat <- mat*matrix(
      rep(vec,ncol(mat)),
      ncol = ncol(mat),
      byrow = FALSE
    )
    return(mat)

  } else {
    stop('`across` must be one of c("cols","rows")')
  }
}





