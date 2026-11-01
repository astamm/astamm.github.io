# ============================================================
# Lab 2: Matrices: build, algebra, and column-wise work
# The R Statistical Language · Session 3
#
# Work through the exercises. Comment your thought process
# as you go: a reproducible script is one a stranger can read.
# ============================================================

## Part A: build & inspect ---------------------------------
# A1. Build M <- matrix(1:20, nrow = 4, ncol = 5).
#     Report dim(M), nrow(M), ncol(M), rowSums(M), colMeans(M).

# A2. Confirm the fill is column-major: print M and check where
#     the 1 and the 6 land. Rebuild with byrow = TRUE and compare.

# A3. Add a 6th column with cbind(M, c(100, 200, 300, 400)).
#     Bind two extra rows with rbind(M, c(1, 2, 3, 4, 5)).

## Part B: arithmetic & recycling --------------------------
# B1. Compute M * 2 and M + c(10, 20, 30, 40) (a length-4 vector).
#     Which axis did the vector recycle along?

# B2. Column-scale M: M * matrix(c(1, 10, 100, 1000, 10000), 1, 5).
#     What does each column get multiplied by?

## Part C: linear algebra ----------------------------------
# C1. A <- matrix(c(4, 2, 1, 3), nrow = 2). Compute:
#     A %*% A, t(A), det(A), solve(A). Verify A %*% solve(A)
#     is (nearly) the identity.

# C2. Build a 4x4 diagonal matrix with diag(4), then diag(c(2, 3, 5, 7)).
#     What does diag(M) return for a rectangular matrix M?

## Part D: row/column summaries & apply --------------------
# D1. Using M from Part A, compute apply(M, 1, max) and apply(M, 2, max).
#     Which MARGIN is which?

# D2. range of each column: apply(M, 2, function(x) max(x) - min(x)).

## Part E: standardise (bridge to functions) ---------------
# E1. Z-score every column of M: subtract the column mean, then
#     divide by the column sd. Use colMeans(M), apply(M, 2, sd),
#     and two sweep() calls (or scale(M) to check yourself).
#     Confirm the result: colMeans ~ 0, apply(., 2, sd) ~ 1.
