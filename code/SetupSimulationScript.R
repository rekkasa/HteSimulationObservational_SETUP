#!/usr/bin/env Rscript

# ==============================================================
# Description:
#   Script to be run for each simulation scenario
# Depends:
#   data/processed/analysisIds.rds
# Output:
#   data/processed/scenario_#/
# Note:
#   Needs a data/raw/scenario_# as input
# ==============================================================


args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Requires scenario id")

source("code/helpers/Weights.R")


library(tidyverse)
library(SmoothHte)
library(SimulateHte)
library(SimulationEvaluationHte)
library(dplyr)

# analysisIds <- readr::read_csv("data/processed/analysisIds.csv")

analysisIds <- readr::read_csv(
  "data/processed/analysisIds.csv",
  col_types =  cols(
    .default = col_double(),
    base = col_character(),
    type = col_character(),
    harm = col_character()
  )
)

selectedScenario <- as.numeric(
  stringr::str_extract(args[1], "[0-9]+")
)

idSettings <- analysisIds %>%
  filter(scenario == selectedScenario)

# -----------------------------------------------------
# variable settings is a list that contains:
#   - databaseSettings
#   - baselineRiskSettings
#   - propensitySettings
# -----------------------------------------------------
settings <- readRDS("data/processed/leacySettings.rds")
settings$databaseSettings$numberOfObservations <- idSettings$sampleSize

if (idSettings$base != "absent") {
  harm <- case_when(
    idSettings$harm == "moderate-positive" ~ idSettings$averageTrueBenefit / 4, 
    idSettings$harm == "strong-positive"   ~ idSettings$averageTrueBenefit / 2, 
    idSettings$harm == "negative"          ~ -idSettings$averageTrueBenefit / 4, 
    TRUE                                   ~ 0
  )
} else {
  harm <- case_when(
    idSettings$harm == "moderate-positive" ~ .01,
    idSettings$harm == "strong-positive"   ~ .02,
    idSettings$harm == "negative"          ~ -.01,
    TRUE                                   ~ 0
  )
}

createF1 <- function(c) function(x) x - c
createF2 <- function(c) function(x) (x - c)^2
treatmentEffectSettings <- SimulateHte::createTreatmentEffectSettings(
  type = "lp",
  harm = 0,
  modelSettings = SimulateHte::createModelSettings(
    constant = idSettings$g0,
    modelMatrix = matrix(c(1, 1)),
    transformationSettings = list(
      createF1(idSettings$c),
      createF2(idSettings$c)
    ),
    coefficients = c(
      idSettings$g1,
      idSettings$g2
    )
  )
)

psSettings <- CohortMethod::createMatchOnPsArgs(maxRatio = 4)

predictionSettings <- createPredictionSettings(
  args = list(
    formula = "outcome ~ x1 + x2 + x3 + x4 + x8 + x9 + x10 + treatment",
    family  = "binomial"
  ),
  fun = "glm",
  .before = CohortMethod::matchOnPs,
  .beforeSettings = psSettings
)

# -------------------------------------------------
# TODO:
#   - Rewrite truncation function to calculate
#     weights, setting w=0 for truncated patients
#   - Figure out .afterSettings
# -------------------------------------------------
propensityScoreSettings <- createPredictionSettings(
  args = list(
    formula = "treatment ~ x1 + x2 + x3 + x4 + x5 + x6 + x7",
    family  = "binomial"
  ),
  fun = "glm",
  .after  = calculateWeights,
  .afterSettings = list(type = "none")
)

simulationSettings <- list(
  databaseSettings        = settings$databaseSettings,
  baselineRiskSettings    = settings$baselineRiskSettings,
  propensitySettings      = settings$propensitySettings,
  treatmentEffectSettings = treatmentEffectSettings
)

analysisSettings <- SimulationEvaluationHte::createAnalysisSettings(
  threads        = 6,
  seed           = 19910930,
  replications   = 100,
  validationSize = 1e5,
  analysisId     = paste(
    "scenario",
    idSettings$scenario,
    sep = "_"
  ),
  description   = "description",
  saveDirectory = "data/raw"
)

smoothSettings <- list(
  constant = createHteSettings(
    label = "constant_treatment_effect",
    settings = createModelBasedSettings(
      type  = "treatment",
      model = "logistic",
      weightColumn = "weight"
    )
  ),
  constantLp = createHteSettings(
    label = "linear_predictor",
    settings = createModelBasedSettings(
      weightColumn = "weight"
    )
  ),
  rcs3 = createHteSettings(
    settings = SmoothHte::createRcsSettings(
      weightColumn = "weight"
    ),
    label = "rcs_3_knots"
  )
)

res <- runAnalysis(
  analysisSettings        = analysisSettings,
  simulationSettings      = simulationSettings,
  predictionSettings      = predictionSettings,
  smoothSettings          = smoothSettings,
  propensityScoreSettings = propensityScoreSettings
)
