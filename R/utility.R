#' Calculate the trace of a matrix.
#'
#' @param x A square matrix.
#' @return The trace of the matrix.
#' @keywords internal

tr = function(x) sum(diag(x))


#' Checks if a matrix is diagonal.
#'
#' @param x A matrix.
#' @param eps A tolerance level.
#' @return \code{TRUE} if the matrix is diagonal, \code{FALSE} otherwise.

is_diagonal = function (x, eps = .Machine$double.eps^1/2) {

  diag(x) = 0
  all(abs(x) - diag(diag(x)) <= eps)

}