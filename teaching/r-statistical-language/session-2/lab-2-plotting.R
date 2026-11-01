# ============================================================
# Lab 1: Plotting: base R vs ggplot2
# The R Statistical Language · Session 2
#
# Work through the exercises. Comment your thought process
# as you go: a reproducible script is one a stranger can read.
# ============================================================

# Load the data once, at the top.
penguins <- readr::read_csv("penguins.csv", show_col_types = FALSE)

## Part A: base R plotting ---------------------------------
# A1. A histogram of body_mass_g with 30 bins.
#     Add a title and colour the bars. (hist(x, breaks, col, main))

# A2. A scatter of body_mass_g vs flipper_length_mm. (plot(x, y))
#     Which variable goes on which axis?

# A3. A boxplot of body_mass_g by species. (boxplot(y ~ g))
#     How does the median differ between species?

# A4. Colour the scatter points by species "by hand":
#     plot(x, y, col = as.numeric(factor(species))).
#     Add a legend() to tell the colours apart.

## Part B: ggplot2 ---------------------------------
# B1. Rebuild the histogram with ggplot2:
#     ggplot(penguins, aes(x = body_mass_g)) + geom_histogram(bins = 30)
#     How does the code compare to A1? Which reads clearer to you?

# B2. The scatter, now with colour handled for you:
#     ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g,
#                          color = species)) + geom_point(alpha = 0.7)
#     Compare to A4: no manual factor() or legend() needed.

# B3. Add a smooth trend layer to the scatter.

# B4. Facet the histogram by species:
#     ... + facet_wrap(~ species)

# B5. Boxplots of body_mass_g by species, filled by species.

## Part C: reflect ---------------------------------
# C1. In a comment: which approach (base R or ggplot2) would you reach for
#     to make a polished figure for a report? To do a 10-second sanity check?
#     What about for building up a figure iteratively in front of an audience?

## Part D: STRETCH ---------------------------------
# D1. Build a small multi-panel figure by combining several ggplots using
#     {patchwork}:  p1 + p2 + p3  (install it if needed). Try
#     library(patchwork); (p1 + p2) / p3  for an interesting layout.
