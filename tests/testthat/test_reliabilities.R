context("reliabilities")

# Does reliabiltiy with no iota return the same as reliabiltiy with the optimal
# iota?

object <- to_list(fit_3f_cov)
iota <- greedy_iota(object$Lambda, object$Psi, object$Gamma, A = diag(3))

expect_equal(
  reliability(object, type = "sumscore", iota = iota),
  reliability(object, type = "sumscore")
)

## Does the reliabiltiy equal coefficient H?

object <- to_list(fit_1f_orth)

expect_equal(
  reliability(object)$rsq,
  coefficient_H(object)
)

## Does the reliabiltiy equal omega??

object <- to_list(fit_1f_orth2)

expect_equal(
  reliability(object,
    type = "sumscore",
    iota = rep(1, 9)
  )$rsq,
  omega(object)
)

expect_error(
  omega(fit_1f_cov),
  "Psi must be diagonal"
)
expect_error(
  omega(fit_3f_cov),
  "Lambda cannot have more the one column."
)
expect_error(
  coefficient_H(fit_1f_cov),
  "Psi must be diagonal"
)
expect_error(
  coefficient_H(fit_3f_cov),
  "Lambda cannot have more the one column."
)
