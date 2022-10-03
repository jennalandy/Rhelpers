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
    stop('`across` must be one of c("col","rows"')
  }
}
