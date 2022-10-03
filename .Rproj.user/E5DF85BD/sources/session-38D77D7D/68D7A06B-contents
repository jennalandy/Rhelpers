#' Split each string and return the ith substring for each item in a vector
#'
#' @param vec a vector of strings
#' @param pattern what to split on
#' @param i index of substring to return for each item in vec
#'
#' @return a vector of strings
#' @export
#'
#' @examples
str_split_vec <- function(vec, pattern, i) {
  stopifnot(is.vector(vec),
            length(vec) > 0,
            is.character(vec[0]),
            is.character(pattern),
            i == as.integer(i)
  )
  i = as.integer(i)
  out <- c()
  for (string in vec) {
    out <- c(out, stringr::str_split(string, pattern)[[1]][i])
  }
  return(out)
}
