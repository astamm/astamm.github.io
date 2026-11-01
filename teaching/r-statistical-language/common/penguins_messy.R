library(palmerpenguins)
library(messy)

penguins_messy <- penguins |> 
  make_missing(cols = "sex", missing = " ") |> 
  make_missing(cols = c("bill_depth_mm", "body_mass_g"), missing = c(NA, 9999)) |> 
  add_whitespace(cols = c("species", "island"), messiness = 0.5)
write_tsv(penguins_messy, "penguins_messy.txt")
