# -------------------------------------------------------------------------------------------
# Truncation using Sturmer approach:
#   - calculate propensity scores
#   - trim all observations with PS > q(PS_untreated, .95) or PS < q(PS_treated, .05)
# -------------------------------------------------------------------------------------------
sturmerTrunate <- function(data) {

  lowerCutoff <- quantile(data %>% filter(treatment == 1) %>% pull(propensityScore), .05)
  upperCutoff <- quantile(data %>% filter(treatment == 0) %>% pull(propensityScore), .95)
  data %>%
    filter(propensityScore >= lowerCutoff & propensityScore <= upperCutoff)
  
}
