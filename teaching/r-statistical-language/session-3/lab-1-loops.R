# ============================================================
# Lab 1: Loops, conditionals & the CLT by simulation
# The R Statistical Language · Session 3
#
# Work through the exercises. Comment your thought process
# as you go: a reproducible script is one a stranger can read.
# ============================================================

## Part A: for loops ---------------------------------
# A1. Print the squares of 1:10 with a for() loop.

# A2. Use a loop to sum the integers 1:1000. Confirm the result
#     equals sum(1:1000).

# A3. Pre-allocate: compute the first 20 Fibonacci numbers in a
#     pre-allocated vector fib (hint: fib[1] <- 1; fib[2] <- 1;
#     then for i in 3:20, fib[i] <- fib[i-1] + fib[i-2]).

## Part B: while loops ---------------------------------
# B1. Start x <- 1 and keep doubling while x < 1000. How many
#     doublings before it stops? (Add a counter.)

## Part C: conditionals ---------------------------------
# C1. Write an if/else/else-if block that grades a numeric score:
#     >= 90 "A", >= 80 "B", >= 70 "C", else "D". Test it.

# C2. Use ifelse() on temps <- c(-5, 4, 12, 25, 31) to label each
#     element as "freezing" (<=0), "cold" (<=15), "mild" (<=25),
#     else "hot". (Nest ifelse() calls.)

## Part D: the Central Limit Theorem by simulation ---------------------------------
# D1. Simulate the sampling distribution of the mean of rexp(n=30):
#     out <- numeric(R)  with R <- 2000
#     for (i in 1:R) out[i] <- mean(rexp(30))
#     Plot a histogram of out. It should look bell-shaped even though
#     the source is exponential. Why?

# D2. Now do the same for rbinom(30, 1, 0.2) (a very skewed source).
#     Does the histogram still look bell-shaped? What if you instead
#     take n=5? (Less averaging -> less bell-shaped.)

# D3. Confirm that the standard deviation of out is close to
#     sd_of_source / sqrt(30).  (sd of rexp is 1, sd of rbinom(1,0.2)
#     is sqrt(0.2*0.8).)

## Part E: STRETCH ---------------------------------
# E1. Time a for() loop that grows a vector vs one that pre-allocates:
#     system.time({ x <- c(); for (i in 1:1e4) x <- c(x, i) })
#     vs
#     system.time({ x <- numeric(1e4); for (i in 1:1e4) x[i] <- i })
#     Note the difference; that's why we pre-allocate.
