#!/usr/bin/env Rscript

# Description: Generates the analysisIds
# Output: data/processed/analysisIds.csv
# Depends: 

library(tidyverse)
library(readr)

base <- c(
  "absent",
  "moderate"
)

type <- c(
  "constant",
  "linear-high",
  "quadratic-high",
  "non-monotonic"
)

sampleSize <- c(4250, 1063, 17000)
harm <- c("absent", "strong-positive", "negative")

expand_grid(base, type, sampleSize, harm) %>%
  mutate(
    b0 = -2,
    b1 = .3,
    b2 = 36,
    b3 = -.73,
    b4 = -.2,
    b5 = 0,
    b6 = 0,
    b7 = 0,
    b8 = .71,
    b9 = -.19,
    b10 = .26,
    g0 = case_when(
      base == "absent" & type == "constant" ~ 0,
      base == "absent" & type == "linear-high" ~ -.25,
      base == "absent" & type == "quadratic-high" ~ -4.22,
      base == "absent" & type == "non-monotonic" ~ 2.49,
      base == "moderate" & type == "constant" ~ log(.8),
      base == "moderate" & type == "linear-high" ~ -.46,
      base == "moderate" & type == "quadratic-high" ~ -4.42,
      base == "moderate" & type == "non-monotonic" ~ 0.1734843,
      TRUE ~ 0
    ),
    g1 = case_when(
      type == "linear-high" ~ 0.7956044,
      base == "absent" & type == "non-monotonic" ~ 4.21,
      base == "moderate" & type == "non-monotonic" ~ 1.560407,
      TRUE ~ 1
    ),
    g2 = case_when(
      type == "quadratic-high" ~ -0.05168458,
      base == "absent" & type == "non-monotonic" ~ .54,
      base == "moderate" & type == "non-monotonic" ~ 0.105142,
      TRUE ~ 0
    ),
    c = case_when(
      startsWith(type, "quad") ~ -5,
      TRUE ~ 0
    ),
    scenario = 1:n(),
    averageTrueBenefit = case_when(
      base == "absent" ~ 0,
      base == "moderate" & type == "constant" ~ .025,
      base == "moderate" & type == "linear-high" ~ .021,
      base == "moderate" & type == "quadratic-high" ~ .017,
      base == "moderate" & type == "non-monotonic" ~ .030
    ),
    averageBenefit = case_when(
      base == "absent" & harm == "absent" ~ 0,
      base == "absent" & harm == "strong-positive" ~ -.02,
      base == "absent" & harm == "negative" ~ .01,
      base != "absent" & harm == "absent" ~ averageTrueBenefit,
      base != "absent" & harm == "strong-positive" ~ averageTrueBenefit - averageTrueBenefit / 4,
      base != "absent" & harm == "negative" ~ averageTrueBenefit + averageTrueBenefit / 4
    )
  ) %>%
  relocate(scenario) %>%
  data.frame() %>%
  readr::write_csv("data/processed/analysisIds.csv")
    
