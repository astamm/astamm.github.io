# ============================================================
# Lab 3: Writing custom functions
# The R Statistical Language · Session 3
#
# Work through the exercises. Comment your thought process
# as you go: a reproducible script is one a stranger can read.
# ============================================================

## Part A: first functions ---------------------------------
# A1. Write zscore <- function(x) returning (x - mean(x)) / sd(x).
#     Test it on c(1, 2, 3, 4, 5): one value should be negative,
#     one positive, and the mean of the output 0.

# A2. Write mad_ratio(x) = mean(abs(x - mean(x))) / sd(x).
#     Test it on rnorm(1000). The result should be close to 0.8.
#     (mad_ratio is a robust/modern measure relative to the sd.)

# A3. Write describe(x) that returns a named list with mean, sd,
#     median, min, max. Test it on a rnorm(100) sample.

## Part B: arguments & defaults ---------------------------------
# B1. Write clamp(x, lo = 0, hi = 1) using ifelse(). Verify that
#     clamp(c(-1, 0.5, 2)) == c(0, 0.5, 1).

# B2. Write a function temp_label(celsius) with an argument
#     scale = "celsius" that returns a label vector with ifelse()
#     thresholds. Try switching the scale to interpret Fahrenheit.

## Part C: functions + pipelines ---------------------------------
# C1. Load the penguins data. Write a function avg_mass_by(df, group)
#     that groups by a bare column name and returns the mean body mass.
#     (Hint: use filter_(!is.na(body_mass_g)) then group_by({{ group }})
#     and summarize.) Test it with avg_mass_by(penguins, species).

# C2. (Stretch) Generalise to avg_by(df, group, var) that averages any
#     numeric column by any group.

## Part D: functions + simulation ---------------------------------
# D1. Turn your CLT simulation from Lab 2 into a function
#     clt_sim <- function(n, R = 1000, dist = "exp") returning the
#     vector of R means. Let dist choose between rexp(n) and
#     rbinom(n, 1, 0.2). Plot a histogram for each distribution.

# D2. Use stopifnot() to add sanity checks to one of your functions
#     (e.g. that the input is numeric). Run it on a character input
#     and read the error.

## Part E: STRETCH ---------------------------------
# E1. Write a function that uses any() and all() to report whether a
#     vector contains NAs, and if so, how many.

# E2. (replicate) The slide introduced replicate(R, expr) to re-run an
#     expression R times. Use it to estimate the sampling distribution
#     of the median:
#     (a) replicate(1000, median(runif(20))): get the vector of medians,
#         then its mean and sd. Compare with the mean of
#         replicate(1000, median(runif(200))): a larger sample shrinks
#         the spread of the median.
#     (b) Wrap the whole thing into a helper sample_medians(n, R = 1000)
#         = replicate(R, median(runif(n))) and confirm
#         sd(sample_medians(20)) > sd(sample_medians(200)).
