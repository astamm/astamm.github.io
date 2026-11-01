# ============================================================
# Lab 1: Vectors, subsetting & lists
# The R Statistical Language · Session 1
#
# Work through the exercises. Comment your thought process
# as you go: a reproducible script is one a stranger can read.
# ============================================================

## Part A: building vectors ---------------------------------
# A1. Build the numeric vector  1 4 7 10 13  ... up to 28  in three ways:
#     (i)  using c() literally,
#     (ii) using seq(from, to, by),
#     (iii) using 3 * (1:10) - 2  (a formula).
#     Confirm all three are identical with == and all().
#     (Hint: all(... == ...))

# A2. Build the character vector c("red","green","blue") and
#     a logical vector of length 5 that alternates TRUE/FALSE.
#     Use rep() for the logical one.

## Part B: vectorized arithmetic ---------------------------------
# B1. Heights (cm) and weights (kg) for 10 people:
height <- c(168, 177, 177, 177, 178, 172, 165, 171, 178, 170)
weight <- c(88, 72, 85, 52, 71, 69, 61, 61, 51, 75)
#     Compute BMI = weight / (height/100)^2, element-wise.
#     What is the mean and standard deviation of BMI?

# B2. Convert a Celsius vector  c(0, 10, 20, 30)  to Fahrenheit:
#     F = C * 9/5 + 32  (vectorized).

## Part C: subsetting ---------------------------------
# C1. For x <- 1:100:
x <- 1:100
#     - keep only the odd numbers;
#     - keep only numbers divisible by 3;
#     - keep numbers divisible by 3 OR 5;
#     - keep numbers divisible by 3 or 5, but NOT both.
#     (first use the modulo operator %%, then try filtering
#      with setdiff() between the "or" and "and" results.)

# C2. For x <- c(10, 3, NA, 5, 8, 1, NA):
#     - all non-missing values;
#     - all even (non-missing) values.  (Careful: %% on NA -> NA)

# C3. Named vector  x <- c(abc = 1, def = 2, xyz = 5).
#     Subset it with a character vector to get elements "xyz" and "def".

## Part D: distributions ---------------------------------
# D1. Sample 100 values from a Weibull distribution with
#     shape 2 and scale 1.  (?Weibull)
#     Compute sum, mean, variance, sd, median, 25th & 75th
#     percentiles, min, max.
#     (Find the quantile function with help.search("quantile").)

# D2. Sample 5000 values from N(3, 2^2) using rnorm().
#     Estimate P(X <= 0) two ways:
#       (i)   as the fraction of the sample <= 0  (mean of a logical),
#       (ii)  exactly with pnorm(0, 3, 2).
#     How close are they? (Law of large numbers.)

## Part E: STRETCH (go further, only if the above felt easy) ----------
# E1. Recycling: what does  c(1,2,3,4) + c(10,20)  give? Explain in a
#     comment. What warning does  c(1,2,3) == c(1,2)  produce, and why?

# E2. Write a one-liner that returns the products of the even numbers
#     from 1 to 100 using modulo + logical subsetting (no loop).

# E3. A "Central Limit Theorem" appetiser:  N <- 30;  R <- 1000
#     Start with m <- numeric(R), then for (i in 1:R) set
#     m[i] <- mean(rexp(N)). Look at a histogram of m (hist(m)).
#     Why does it look bell-shaped though the data are exponential?
#     (We'll formalise this in Session 2.)

## Part F: lists ---------------------------------
# F1. Build a list of three vectors of lengths 10, 20 and 30, then
#     compute the mean of each with lapply(), then again with sapply()
#     and vapply(). What differs in the three returned values?

# F2. Make a list of two length-5 vectors. Use lapply() to double each
#     element; store and inspect the result.

# F3. Subsetting: for  x <- list(a = 1, b = "a", c = TRUE):
#     - x["a"]       vs  x[["a"]]       (what classes do you get?)
#     - x$b          vs  x[["b"]]
#     - x[[5]]       vs  x[["nope"]]    (predict, then read each error)

# F4. Extend a list:  x$d <- 1:3   and   x[["e"]] <- "new".
#     What happened to the list? Compare adding via $ vs [[.

# F5. Nested lists:  l <- list(a = list(1, "x"), b = 2).
#     Predict l$a[1], l$a[[1]], l[[1]][2], then check with the console.
#     (This is exactly the pepper shaker metaphor from the slides.)
