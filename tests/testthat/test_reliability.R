context("Reliabiltiy function")

fit_3f = readRDS("data_fit_3f.Rds")
fit_3f_cov = readRDS("data_fit_3f_cov.Rds")
fit_3f_orth = readRDS("data_fit_3f_orth.Rds")

# Does reliabiltiy with no iota return the same as reliabiltiy with the optimal
# iota?

object = to_list(fit_3f_cov)
iota = greedy_iota(object$Lambda, object$Psi, object$Gamma, A = diag(3))
expect_equal(reliability(object, type = "sumscore", iota = iota),
             reliability(object, type = "sumscore"))

## Does the reliabiltiy equal coefficient H?

fit_1f_orth = readRDS("data_fit_1f_orth.Rds")
object = to_list(fit_1f_orth)
expect_equal(reliability(object)$reliability,
             coefficient_H(object))

## Does the reliabiltiy equal omega??

fit_1f_orth2 = readRDS("data_fit_1f_orth2.Rds")
object = to_list(fit_1f_orth2)
expect_equal(reliability(object, type = "sumscore",
                         iota = rep(1, 9))$reliability,
             omega(object))
