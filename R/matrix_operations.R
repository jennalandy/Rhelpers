#' Normalize a matrix such that columns or rows sum to 1
#'
#' @param mat input matrix
#' @param sum one of c('cols','rows')
#'
#' @return normalized matrix
#' @export
normalizeMat <- function(mat, sum = 'cols') {
  if (sum == 'cols') {
    mat <- multiplyAcross(mat, 1/colSums(mat), across = 'rows')
    return(mat)
  } else if (sum == 'rows') {
    mat <- multiplyAcross(mat, 1/rowSums(mat), across = 'cols')
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





