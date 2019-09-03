#' Calculate the decision theoretic reliabilities and optimal weights
#'
#' Calculate the reliabiltiy of a prediction in a linear factor model. The
#'    prediction is on the form S = AZ + Beps', where eps' is a new error and
#'    Z are the latent variables.
#'
#' @export
#' @param object Either a lavaan object or a list containing the factor loadings
#'    and the residual errror covariance matrix.
#' @param type Choose "linear" for the optimal linear weights (recommended) or
#'    "sumscore" for the optimal sumscore weights.
#' @param A The matrix A in S = AZ + Beps.
#' @param B The matrix B in S = AZ + Beps. Defaults to \code{0}.
#' @param iota If type is sumscore and iota is supplied, this iota is used
#'     instead of the optimal iota.
#' @return A list containing the marix of optimal weights, the R^2 reliability,
#'     the inflation rate reliability and ioata (if applicable.)


reliability = function(object, type = "linear", A = 1, B = 0, iota = NULL) {

  type = match.arg(type, c("sumscore", "linear"))
  object = to_list(object)
  if(is_scalar(A)) A = as.numeric(A)*diag(nrow(object$Gamma))
  if(is_scalar(B)) B = as.numeric(B)*diag(nrow(object$Psi))
  if(type == "linear") {
      rsq = rsq_linear(object$Lambda, object$Psi, object$Gamma, A, B)
  } else if(type == "sumscore") {

    if(is.null(iota)) iota = greedy_iota(object$Lambda, object$Psi, object$Gamma, A)
    rsq = rsq_sumscore(object$Lambda, object$Psi, object$Gamma, A, B, iota)
    rsq$iota = iota

  }

  rsq$inflation = 1/(1 - rsq$reliability)
  rsq

}

#' Calculate coefficient H.
#'
#' @export
#' @param object Either a lavaan object or a list containing the factor loadings
#'    and the residual errror covariance matrix.
#' @return Coefficient H.
#' @keywords internal

coefficient_H = function(object) {

  object = to_list(object)
  numerator = crossprod(object$Lambda * 1 / diag(object$Psi), object$Lambda)
  H = numerator/(1 + numerator)
  c(unname(H))

}

#' Calculate the congeneric reliabiltiy.
#'
#' @export
#' @param object Either a lavaan object or a list containing the factor loadings
#'    and the residual errror covariance matrix.
#' @return The generalized congeneric reliability.
#' @keywords internal

omega = function(object) {

  object = to_list(object)
  numerator = sum(object$Lambda)^2
  i = rep(1, nrow(object$Psi))
  omega = numerator/(sum(object$Psi) + numerator)

  c(unname(omega))

}