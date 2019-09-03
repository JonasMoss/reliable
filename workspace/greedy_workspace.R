A = tcrossprod(Lambda, Lambda)
B = Psi
greedy(A, B)
microbenchmark::microbenchmark(greed(A,B), greed2(A, B))
