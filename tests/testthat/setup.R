fit_3f_cov_path = system.file("testdata", "data_fit_3f_cov.rds", package = "reliable")
fit_1f_orth_path = system.file("testdata", "data_fit_1f_orth.rds", package = "reliable")
fit_1f_orth2_path = system.file("testdata", "data_fit_1f_orth2.rds", package = "reliable")

fit_3f_cov = readRDS(fit_3f_cov_path)
fit_1f_orth = readRDS(fit_1f_orth_path)
fit_1f_orth2 = readRDS(fit_1f_orth2_path)
