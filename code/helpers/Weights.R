calculateWeights <- function(data, type = "none") {
  if (type == "none") {
    res <- data %>% dplyr::mutate(weight = 1)
  } else {
    weight = data$treatment / data$propensityScore + (1 - data$treatment) / (1 - data$propensityScore),

    if (type == "untrimmed") {
      res <- data %>%
        dplyr::mutate(weight = weight)
    } else if (type == "sturmer") {
      lowerCutoff <- quantile(data %>% filter(treatment == 1) %>% pull(propensityScore), .05)
      upperCutoff <- quantile(data %>% filter(treatment == 0) %>% pull(propensityScore), .95)
      res <- data %>%
        dplyr::mutate(
          weight = ifelse(
            test = propensityScore >= lowerCutoff & propensityScore <= upperCutoff,
            yes  = treatment / propensityScore + (1 - treatment) / (1 - propensityScore),
            no   = 0
          )
        ) 
    }
  }

  return(res)
}
