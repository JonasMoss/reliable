(function() {
## The underlying data is HolzingerSwineford but with x4 flipped.

data = lavaan::HolzingerSwineford1939
data$x4 = -data$x4


## The different models and fits.

model_3f = ' visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 '

model_3f_cov = ' visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9
               x1 ~~ x2
               x4 ~~ x5
               x7 ~~ x8
               x1 ~~ x9'

model_1f = ' g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '

model_1f_cov  = ' g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
                  x1 ~~ x2
                  x4 ~~ x5
                  x7 ~~ x8
                  x1 ~~ x9'

## Fit the models.


fit_3f = lavaan::cfa(model = model_3f,
                     data = data)

fit_3f_cov = lavaan::cfa(model = model_3f_cov,
                         data = data)

fit_3f_orth = lavaan::cfa(model = model_3f,
                          data = data,
                          orthogonal = TRUE,
                          std.lv = TRUE)

fit_1f = lavaan::cfa(model = model_1f,
                                  data = data)

fit_1f_cov = lavaan::cfa(model = model_1f_cov,
                         data = data)

fit_1f_orth = lavaan::cfa(model = model_1f,
                          data = data,
                          orthogonal = TRUE,
                          std.lv = TRUE)

fit_1f_orth2 = lavaan::cfa(model = model_1f,
                          data = lavaan::HolzingerSwineford1939,
                          orthogonal = TRUE,
                          std.lv = TRUE)

# And store the data.

saveRDS(fit_3f, "inst/testdata/data_fit_3f.rds")
saveRDS(fit_3f_cov, "inst/testdata/data_fit_3f_cov.rds")
saveRDS(fit_3f_orth, "inst/testdata/data_fit_3f_orth.rds")
saveRDS(fit_1f, "inst/testdata/data_fit_1f.rds")
saveRDS(fit_1f_cov, "inst/testdata/data_fit_1f_cov.rds")
saveRDS(fit_1f_orth, "inst/testdata/data_fit_1f_orth.rds")
saveRDS(fit_1f_orth2, "inst/testdata/data_fit_1f_orth2.rds")

})()