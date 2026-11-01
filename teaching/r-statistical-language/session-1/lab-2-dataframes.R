# ============================================================
# Lab 2: Data frames, tibbles & reading data
# The R Statistical Language · Session 1
#
# The goal: load a "messy" penguins file, diagnose its shape,
# fix the column types, and write a clean CSV. This is your
# first taste of the data-cleaning decisions you'll formalise
# in the Reproducible Workflows course.
# ============================================================

## Part A: build and explore a data frame -------------------
# A1. Create df <- data.frame(
#       x = 1:5,
#       y = c("a","b","c","d","e"),
#       z = c(TRUE, FALSE, TRUE, TRUE, FALSE)
#     )
#     Inspect it with str(df), summary(df), head(df).
#     What is class(df)? What is the class of df$x? of df$y?

# A2. Convert to a tibble:  df_t <- tibble::as_tibble(df)
#     Print it. How does the print differ from a data.frame?

# A3. Subset:
#     - rows 2 and 3 of column 1;
#     - all rows of columns 1 and 3;
#     - all rows where z is TRUE.

## Part B: read the messy file ------------------------------
# B1. Open penguins_messy.txt in a text editor (outside R).
#     Observe: what separates the values? Is there a header?
#     Are there missing values, and what do they look like?

# B2. Try reading with:
#     library(readr)
#     read_csv("penguins_messy.txt")   # -> probably wrong!
#     read_table("penguins_messy.txt") # -> probably closer
#     Look at the "spec" (column types) readr guessed via
#     problems() or the printed column spec. Which columns are
#     wrong (should be numeric but came in as character)?

# B3. Read it successfully and fix the types:
#     penguins <- readr::read_table(...)
#     for columns that should be numeric, coerce, e.g.
#     species, island and sex should be factors (or kept character).

## Part C: cleaning decisions -------------------------------
# C1. Missing values: how many NAs per column?
#     colSums(is.na(penguins))   (is.na -> logical; colSums sums TRUEs)

# C2. Are the type coercions what you expect? Use str() to confirm.

# C3. Write the cleaned data:
#     write_csv(penguins, "penguins_clean.csv")
#     Re-open it with read_csv(): are the types still right
#     after the round trip?

## Part D: STRETCH -------------------------------------------
# D1. Why did read_csv() mis-guess column types on this file?
#     Experiment with a smaller sample (n_max = 3) to diagnose.

# D2. For the island column, use table(island) to summarise counts.
#     Turn island into a factor with forcats::fct_infreq(island)
#     and re-tabulate: what changed, and why might that order help
#     a later barplot?

# D3. The {janitor} package (used in RR course) has clean_names().
#     Install it (install.packages("janitor")) and run
#     janitor::clean_names() on penguins: what does it change?
#     (This foreshadows the data-cleaning toolchain.)
