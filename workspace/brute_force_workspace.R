### ============================================================================
###   Brute force search
### ============================================================================





#
# grid = expand.grid(c(-1, 0, 1), c(-1, 0, 1), c(-1, 0, 1), c(-1, 0, 1), c(-1, 0, 1),
#                    c(-1, 0, 1))
# fun = function(vec) 1/(1 + t(vec) %*% B %*% vec / (t(vec) %*% A %*% vec))
#
# res = apply(grid, 1, fun)
# cbind(grid, res)
# max(res, na.rm = TRUE)
# which.max(res)
