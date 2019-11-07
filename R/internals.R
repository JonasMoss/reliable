#' Checks if argument is a scalar.
#'
#' @keywords internal
#' @param x An object.
#' @return \code{TRUE} if \code{x} is scalar, \code{FALSE} otherwise.

is_scalar <- function(x) is.atomic(x) && identical(length(x), 1L)

#' Calculate R squared for a the linear action space.
#'
#' @param Lambda A positive definite matrix.
#' @param Psi A positive definite matrix.
#' @param A A matrix.
#' @param B A matrix.
#' @return The generalized congeneric reliability.
#' @keywords internal

rsq_linear <- function(Lambda, Psi, Gamma, A, B) {
  denominator <- tr(tcrossprod(A %*% Gamma, A)) + tr(crossprod(B, B))
  C <- Lambda %*% Gamma
  D <- tcrossprod(C, A)
  E <- tcrossprod(D, D)
  reliability <- tr(solve(tcrossprod(C, Lambda) + Psi, E)) / denominator
  weights <- solve(tcrossprod(C, Lambda) + Psi, D)
  list(
    rsq = reliability,
    weights = weights
  )
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

rsq_sumscore <- function(Lambda, Psi, Gamma, A, B, iota) {
  denominator <- tr(tcrossprod(A %*% Gamma, A)) + tr(crossprod(B, B))
  C <- 1 / diag(crossprod(
    iota,
    (Lambda %*% Gamma %*% t(Lambda) + Psi) %*% iota
  ))
  D <- diag(crossprod(iota, Lambda %*% Gamma %*% t(A)))^2
  weights <- diag(t(iota) %*% Lambda %*% Gamma %*% t(A)) %*%
    (1 / diag(t(iota) %*% (Lambda %*% Gamma %*% t(Lambda) + Psi) %*% iota))
  list(
    rsq = tr(C %*% D) / denominator,
    weights = weights
  )
}

#' Extracts matrices from a lavaan object.
#'
#' @keywords internal
#' @param object A \code{lavaan} object.
#' @return A list containing Lambda, Psi, and Gamma.

lavaan_to_list <- function(object) {
  coefs <- lavaan::lavInspect(object, what = "x")
  list(
    Lambda = coefs$lambda,
    Psi = coefs$theta,
    Gamma = as.matrix(coefs$psi)
  )
}

#' Extract matrices from a lavaan object or return a list.
#'
#' @keywords internal
#' @param object A \code{lavaan} object or a list of Lambda, Psi and Gamma.
#' @return A list containing Lambda, Psi, and Gamma.

to_list <- function(object) {
  if (inherits(object, "lavaan")) {
    lavaan_to_list(object)
  } else {
    object$Lambda <- as.matrix(object$Lambda)
    object$Psi <- as.matrix(object$Psi)
    object$Gamma <- as.matrix(object$Gamma)
    object
  }
}

#' Calculate the trace of a matrix.
#'
#' @param x A square matrix.
#' @return The trace of the matrix.
#' @keywords internal

tr <- function(x) sum(diag(x))

#' Checks if a matrix is diagonal.
#'
#' @param x A matrix.
#' @param eps A tolerance level.
#' @keywords internal
#' @return \code{TRUE} if the matrix is diagonal, \code{FALSE} otherwise.

is_diagonal <- function(x, eps = .Machine$double.eps^1 / 2) {
  diag(x) <- 0
  all(abs(x) - diag(diag(x)) <= eps)
}

greedy_iota <- function(Lambda, Psi, Gamma, A) {
  q <- ncol(Lambda) # Number of latent variables
  p <- nrow(Lambda) # Length of x
  iota <- matrix(nrow = p, ncol = q)
  B_matrix <- tcrossprod(Lambda %*% Gamma, Lambda) + Psi
  C <- tcrossprod(Lambda %*% Gamma, A)
  E <- A * 0

  for (i in 1:q) {
    E[i, i] <- 1
    A_matrix <- tcrossprod(C %*% E, C)
    iota[, i] <- greedy(A_matrix, B_matrix)
    E[i, i] <- 0
  }

  iota
}

#' Greedy algorithm for finding the optimal iota vector
#'
#' @keywords internal
#' @param A a positive definite matrix.
#' @param B a positive definite matrix with the same dimension as A.

greedy <- function(A, B) {
  indices <- which.min(diag(B) / diag(A))
  lower <- diag(A)[indices]
  upper <- diag(B)[indices]
  value <- upper / lower
  k <- nrow(A)
  vec <- rep(0, k)
  vec[indices] <- 1
  signs <- 1

  for (i in 2:k) {
    current_index <- NA

    for (j in setdiff(1:k, indices)) {
      L <- sum(c(signs) * A[indices, j])
      U <- sum(c(signs) * B[indices, j])

      proposals <- c(
        (2 * U + B[j, j] + upper) / (2 * L + A[j, j] + lower),
        (-2 * U + B[j, j] + upper) / (-2 * L + A[j, j] + lower)
      )

      proposal <- min(proposals)

      if (proposal < value) {
        value <- proposal
        sign <- c(1, -1)[which.min(proposals)]
        current_index <- j
        current_lower <- L
        current_upper <- U
      }
    }

    if (is.na(current_index)) break

    lower <- sign * 2 * current_lower + A[current_index, current_index] + lower
    upper <- sign * 2 * current_upper + B[current_index, current_index] + upper

    indices <- c(indices, current_index)
    signs <- c(signs, sign)
  }

  vec <- rep(0, k)
  vec[indices] <- signs
  list(iota = vec, value = c(1 / (1 + value)))
  vec
}
