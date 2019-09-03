greedy_iota = function(Lambda, Psi, Gamma, A) {
  q = ncol(Lambda) # Number of latent variables
  p = nrow(Lambda) # Length of x
  iota = matrix(nrow = p, ncol = q)
  B_matrix = tcrossprod(Lambda %*% Gamma, Lambda) + Psi
  C = tcrossprod(Lambda %*% Gamma, A)
  E = A*0

  for(i in 1:q) {
    E[i, i] = 1
    A_matrix = tcrossprod(C %*% E, C)
    iota[ , i] = greedy(A_matrix, B_matrix)
    E[i, i] = 0
  }

  iota

}

#' Greedy algorithm for finding the optimal iota vector
#'
#' @param A a positive definite matrix.
#' @param B a positive definite matrix with the same dimension as A.

greedy = function(A, B) {

  indices = which.min(diag(B)/diag(A))
  lower = diag(A)[indices]
  upper = diag(B)[indices]
  value = upper/lower
  k = nrow(A)
  vec = rep(0, k)
  vec[indices] = 1
  signs = 1

  for(i in 2:k) {

    current_index = NA

    for(j in setdiff(1:k, indices)) {

      L = sum(c(signs)*A[indices, j])
      U = sum(c(signs)*B[indices, j])

      proposals = c((2*U + B[j,j] + upper)/(2*L + A[j,j] + lower),
                    (-2*U + B[j,j] + upper)/(-2*L + A[j,j] + lower))

      proposal = min(proposals)

      if(proposal < value) {
        value = proposal
        sign = c(1, -1)[which.min(proposals)]
        current_index = j
        current_lower = L
        current_upper = U
      }

    }

    if(is.na(current_index)) break

    lower = sign*2*current_lower + A[current_index, current_index] + lower
    upper = sign*2*current_upper + B[current_index, current_index] + upper

    indices = c(indices, current_index)
    signs = c(signs, sign)

  }

  vec = rep(0, k)
  vec[indices] = signs
  list(iota = vec,
       value = c(1/(1 + value)))
  vec

}
