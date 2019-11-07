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
reliability <- function(object, type = "linear", A = 1, B = 0, iota = NULL) {
  type <- match.arg(type, c("sumscore", "linear"))
  object <- to_list(object)

  if (is_scalar(A)) A <- as.numeric(A) * diag(nrow(object$Gamma))
  if (is_scalar(B)) B <- as.numeric(B) * diag(nrow(object$Psi))

  if (type == "linear") {
    rsq <- rsq_linear(object$Lambda, object$Psi, object$Gamma, A, B)
  } else if (type == "sumscore") {
    if (is.null(iota)) {
      iota <- greedy_iota(object$Lambda, object$Psi, object$Gamma, A)
    }

    rsq <- rsq_sumscore(object$Lambda, object$Psi, object$Gamma, A, B, iota)
    rsq$iota <- iota
  }

  rsq$inflation <- 1 / (1 - rsq$rsq)
  rsq
}

#' Calculate coefficient H.
#'
#' Coefficient H is a optimal linearly weighted reliability coefficient
#'    for the congeneric measurment model. The function takes either a
#'    \code{lavaan} object or a list  containing the matrix of factor loadings
#'    and the diagonal matrix of residual variances.
#'
#' The congeneric measurement model is the linear factor model with one latent
#'    variable, potentially different loadings, and a diagonal residual variance
#'    matrix. The congeneric reliability (see \code{\link{omega}}) is also a
#'    reliability under the congeneric measurement model. The difference between
#'    coefficient H and the congeneric reliability lies in the implied factor
#'    scores. While the congeneric reliability uses sum scores where each item
#'    in the scale is given the same weight, coefficient H uses best possible
#'    linear combination of the items in terms of squared error loss.
#'
#' @export
#' @param object Either a \code{lavaan} object or a list
#'    containing \code{Lambda}, the matrix of factor loadings, and \code{Psi},
#'    a diagonal matrix of residual variances.
#' @return A numeric containing the value of coefficient H.
#' @seealso \code{\link{omega}} for the sum score reliability under the
#'    congeneric model when each score is \code{1}. \code{\link{reliability}}
#'    for reliabilities under the linear factor model.
#' @references
#'   Hancock, G. R. (2001). Rethinking construct reliability within latent
#'   variable systems. Structural equation modeling: Present and future,
#'   195-216.
#' @examples
#' model <- " g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 "
#' fit <- lavaan::cfa(model = model, data = lavaan::HolzingerSwineford1939)
#' coefficient_H(fit)
coefficient_H <- function(object) {
  object <- to_list(object)

  assertthat::assert_that(ncol(object$Lambda) == 1,
    msg = "Lambda cannot have more the one column."
  )

  assertthat::assert_that(is_diagonal(object$Psi),
    msg = "Psi must be diagonal."
  )

  numerator <- crossprod(object$Lambda * 1 / diag(object$Psi), object$Lambda)
  H <- numerator / (1 + numerator)
  c(unname(H))
}

#' Calculate the congeneric reliability.
#'
#' The congeneric reliability is a the sum score reliability for the
#'    congeneric measurment model. The function takes either a
#'    \code{lavaan} object or a list  containing the matrix of factor loadings
#'    and the diagonal matrix of residual variances.
#'
#' The congeneric measurement model is the linear factor model with one latent
#'    variable, potentially different loadings, and a diagonal residual variance
#'    matrix. Coefficient H (see \code{\link{coefficient_H}}) is also a
#'    reliability under the congeneric measurement model. The difference between
#'    coefficient H and the congeneric reliability lies in the implied factor
#'    scores. While the congeneric reliability uses sum scores where each item
#'    in the scale is given the same weight, coefficient H uses best possible
#'    linear combination of the items in terms of squared error loss.
#'
#' When all factor loadings are equal the congeneric reliability equals
#'    coefficient alpha. If all residual variances are equal as well, the
#'    congeneric reliability equals the standardized cofficient alpha.
#'
#' This function does not find the optimal selection of variables and does
#'     not flip the sign of any items. Use \code{reliability} with
#'     \code{type = "sumscore"} to do this.
#'
#' @export
#' @param object Either a \code{lavaan} object or a list
#'    containing \code{Lambda}, the matrix of factor loadings, and \code{Psi},
#'    a diagonal matrix of residual variances.
#' @return  A numeric containing the value of the congeneric reliability.
#' @seealso \code{\link{coefficient_H}} for the linear reliability under the
#'    congeneric model. \code{\link{reliability}} for reliabilities under the
#'    linear factor model, which includes optimization of which elements to
#'    include in the model when \code{type = "sumscore"}.
#' @references
#'   Cronbach, L.J. (1951) "Coefficient alpha and the internal strucuture
#'   of tests." Psychometrika, 16, 297-334.
#' @examples
#' model <- " g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 "
#' fit <- lavaan::cfa(model = model, data = lavaan::HolzingerSwineford1939)
#'
#' # The congeneric reliability uses all items supplied.
#' omega(fit)
#'
#' # But the reliability function finds the optimal choice of variables.
#' reliability(fit, type = "sumscore")
omega <- function(object) {
  object <- to_list(object)

  assertthat::assert_that(ncol(object$Lambda) == 1,
    msg = "Lambda cannot have more the one column."
  )

  assertthat::assert_that(is_diagonal(object$Psi),
    msg = "Psi must be diagonal."
  )

  numerator <- sum(object$Lambda)^2
  omega <- numerator / (sum(object$Psi) + numerator)

  c(unname(omega))
}
