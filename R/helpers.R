#' Checks if argument is a scalar.

is_scalar = function(x) is.atomic(x) && identical(length(x), 1L)

#' Calculate R squared for a the linear action space.
#'
#' @param Lambda A positive definite matrix.
#' @param Psi A positive definite matrix.
#' @param A A matrix.
#' @param B A matrix.
#' @return The generalized congeneric reliability.
#' @keywords internal

rsq_linear = function(Lambda, Psi, Gamma, A, B) {
  denominator = tr(tcrossprod(A%*%Gamma, A)) + tr(crossprod(B, B))
  C = Lambda %*% Gamma
  D = tcrossprod(C, A)
  E = tcrossprod(D, D)
  reliability = tr(solve(tcrossprod(C, Lambda) + Psi, E))/denominator
  weights = solve(tcrossprod(C, Lambda) + Psi, D)
  list(reliability = reliability,
       weights = weights)
}

#' Calculate R squared for a the sum score action space
#'
#' @param Lambda A positive definite matrix.
#' @param Psi A positive definite matrix.
#' @param A A matrix.
#' @param B A matrix.
#' @param iota A matrix of \code{-1, 0, 1}.
#' @return The generalized congeneric reliability.
#' @keywords internal

rsq_sumscore = function(Lambda, Psi, Gamma, A, B, iota) {
  denominator = tr(tcrossprod(A%*%Gamma, A)) + tr(crossprod(B, B))
  C = 1/diag(crossprod(iota, (Lambda %*% Gamma %*% t(Lambda) + Psi) %*% iota))
  D = diag(crossprod(iota, Lambda %*% Gamma %*% t(A)))^2
  weights = diag(t(iota) %*% Lambda %*% Gamma %*% t(A)) %*%
    (1/diag(t(iota) %*% (Lambda %*% Gamma %*% t(Lambda) + Psi) %*% iota))
  list(reliability = tr(C%*%D)/denominator,
       weights = weights)

}

#' Extracts matrices from a lavaan object.
#'
#' @param object A \code{lavaan} object.
#' @return A list containing Lambda, Psi, and Gamma.

lavaan_to_list = function(object) {
  coefs = lavaan::lavInspect(object, what = "x")
  list(Lambda = coefs$lambda,
       Psi = coefs$theta,
       Gamma = as.matrix(coefs$psi))
}

#' Extract matrices from a lavaan object or return a list.
#'
#' @param object A \code{lavaan} object or a list of Lambda, Psi and Gamma.
#' @return A list containing Lambda, Psi, and Gamma.

to_list = function(object) {
  if(inherits(object, "lavaan")) lavaan_to_list(object) else {
    assertthat::assert_that(!is.null(object$Gamma), msg = "'object$Gamma' does not exist.")
    assertthat::assert_that(!is.null(object$Lambda), msg = "'object$Lambda' does not exist.")
    assertthat::assert_that(!is.null(object$Psi), msg = "'object$Psi' does not exist.")
    p = ncol(object$Lambda)
    k = nrow(object$Lambda)
    assertthat::are_equal(p, ncol(object$Gamma), msg = "Gamma and Lambda have incompatible dimensions.")
    assertthat::are_equal(q, ncol(object$Psi), msg = "Psi and Lambda have incompatible dimensions.")
    object
  }
}

