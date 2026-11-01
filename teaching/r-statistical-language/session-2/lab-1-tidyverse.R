# ============================================================
# Lab 1: Tidyverse data transformation: the pipe, dplyr, tidyr
# The R Statistical Language · Session 2
#
# The foundation for everything that follows this session and
# in the Reproducible Workflows course. Comment your thought
# process as you go. Don't peek at a neighbour's tibble; the
# point is to reason about each verb.
# ============================================================

# Load the data once, at the top.
penguins <- readr::read_csv("penguins.csv", show_col_types = FALSE)

## Part A: the pipe and the core verbs ----------------------
# A1. Write a pipe that keeps only the Gentoo penguins.
#     penguins |> filter(species == "Gentoo")

# A2. Select three columns that interest you; call the result `gentoo`.
#     gentoo <- penguins |> filter(species == "Gentoo") |>
#               select(...)

# A3. Add a column `mass_kg = body_mass_g / 1000` to `gentoo`.
#     mutate() keeps every other column.

# A4. Sort the Gentoo penguins from lightest to heaviest, then
#     heaviest first (arrange() and arrange(desc(...))).

## Part B: summaries by group -------------------------------
# B1. Mean and standard deviation of bill_length_mm per island:
#     penguins |> group_by(island) |>
#                summarise(avg = mean(bill_length_mm, na.rm = TRUE),
#                          sd  = sd(bill_length_mm, na.rm = TRUE))
#     Compare the gentoo and Adelie islands. Which is most variable?

# B2. Which species has the heaviest *median* body mass?
#     Same shape as B1, but finish with arrange(desc(median_mass))
#     and read the first row. Hint: summarise(median_mass = median(body_mass_g, na.rm = TRUE))
#     (na.rm = TRUE: there are missing values.)

# B3. How many individuals of each species live on each island?
#     group_by(species, island) |> summarise(n = n())
#     Does any species/island combo have no data?

## Part C: reshape with tidyr (the trickiest part; take your time) -----
# C1. Build a WIDE table: average bill length, one row per island and one
#     column per species. You already did "summarise per group" in Part B;
#     now you just add the pivot step on the end:
#     wide <- penguins |> group_by(island, species) |>
#             summarise(avg = mean(bill_length_mm, na.rm = TRUE)) |>
#             pivot_wider(names_from = species, values_from = avg)
#     If C1 feels hard, that's normal; pivot_wider is the newest idea here.

# C2. Melt it back to tidy with pivot_longer():
#     wide |> pivot_longer(cols = c(Adelie, Chinstrap, Gentoo),
#                          names_to = "species", values_to = "avg_bill")

## Part D: reflect -----------------------------------------
# D1. In a comment: for the summary you built in B2, would this be
#     easier or harder in base R (tapply / aggregate / by)? Which
#     code reads more like the sentence "median mass by species"?

## Part E: STRETCH -----------------------------------------
# E1. filter() ANDs conditions together with commas. Combine three
#     conditions to isolate a specific subgroup, then verify by
#     counting rows each way.
# E2. Chain filter + mutate + group_by + summarise into one pipe
#     that answers a question of your own about the penguins.
