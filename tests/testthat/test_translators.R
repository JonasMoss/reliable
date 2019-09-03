context("Translating objects")

expect_error(to_list(1))
expect_error(to_list(list(Gamma = 1, Lambda = 2)))
expect_error(to_list(list(Gamma = 1, Lambda = 1, Psi = NULL)))