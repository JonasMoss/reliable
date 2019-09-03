model = ' visual  =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed   =~ x7 + x8 + x9 '

#model = ' g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
#model = ' g =~ x2 + x5 + x9 + x4 + x1 + x6'

data = lavaan::HolzingerSwineford1939
data$x4 = -data$x4

fit = lavaan::lavaan(model = model,
                     data = data,
                     auto.var = TRUE,
                     auto.fix.first = TRUE,
                     auto.cov.lv.x = TRUE)

coefs = lavaan::lavInspect(fit, what = "x")
Lambda = coefs$lambda
Psi = coefs$theta
Gamma = coefs$psi
A = t(rep(1,ncol(Lambda)))
B = 0

rm(model, coefs, fit, data)


#
#
# iotas = expand.grid(c(0, 1), c(0,1), c(0,1), c(0,1), c(0, 1), c(0,1), c(0,1), c(0,1), c(0,1))
# res = sapply(1:nrow(iotas), function(i) rsq_sumscore(Lambda, Psi, Gamma, A, B, t(iotas[i, ])))
#
#
#
# microbenchmark::microbenchmark(coefficient_H(Lambda, Psi, Gamma, A, B),
#                                rsq_linear(Lambda, Psi, Gamma, A, B))
#
# microbenchmark::microbenchmark(omega(Lambda, Psi, Gamma, A, B),
#                                rsq_sumscore(Lambda, Psi, Gamma, A, B, iota))

object = to_list(fit_3f_cov)
Lambda = object$Lambda
Psi = object$Psi
Gamma = object$Gamma
A = diag(3)
B = 0
iota = greedy_solution(Lambda, Psi, Gamma, diag(3), 0)
iota2 = iota
iota2[7, 3] = 1
iota2[8, 3] = 1
reliability(object, type = "sumscore", iota = iota)
reliability(object, type = "sumscore", iota = iota2)

object = to_list(fit_1f)
Lambda = object$Lambda
Psi = object$Psi
Gamma = object$Gamma
greedy_solution(Lambda, Psi, Gamma, diag(1), 0)