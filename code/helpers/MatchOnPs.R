
matchOnPs <- function (data, caliper = 0.2, caliperScale = "standardized logit", 
    maxRatio = 1, allowReverseMatch = FALSE, stratificationColumns = c()) 
{
    if (!("rowId" %in% colnames(data))) 
        stop("Missing column rowId in data")
    if (!("treatment" %in% colnames(data))) 
        stop("Missing column treatment in data")
    if (!("propensityScore" %in% colnames(data))) 
        stop("Missing column propensityScore in data")
    if (caliperScale != "standardized" && caliperScale != "propensity score" && 
        caliperScale != "standardized logit") 
        stop(paste("Unknown caliperScale '", caliperScale, "', please choose either 'standardized', 'propensity score', or 'standardized logit'"), 
            sep = "")
    reverseTreatment <- (allowReverseMatch && sum(data$treatment == 
        1) > sum(data$treatment == 0))
    if (reverseTreatment) {
        data$treatment <- 1 - data$treatment
    }
    data <- data[order(data$propensityScore), 
        ]
    propensityScore <- data$propensityScore
    if (caliper <= 0 || nrow(data) == 0 || min(data$propensityScore) == 
        max(data$propensityScore)) {
        caliper <- 9999
    }
    else if (caliperScale == "standardized") {
        caliper <- caliper * sd(data$propensityScore)
    }
    else if (caliperScale == "standardized logit") {
        propensityScore <- logit(propensityScore)
        caliper <- caliper * sd(propensityScore)
    }
    if (maxRatio == 0) {
        maxRatio <- 999
    }
    if (length(stratificationColumns) == 0) {
        result <- matchPsInternal(propensityScore, data$treatment, 
            maxRatio, caliper)
        result <- dplyr::as_tibble(result)
        data$stratumId <- result$stratumId
        data <- data[data$stratumId != -1, 
            ]
        if (!is.null(attr(data, "metaData"))) {
            attr(data, "metaData")$attrition <- rbind(attr(data, 
                "metaData")$attrition, getCounts(data, 
                paste("Matched on propensity score")))
        }
        if (reverseTreatment) {
            data$treatment <- 1 - data$treatment
        }
        ParallelLogger::logDebug("Data size after matching is ", 
            nrow(result))
        return(data)
    }
    else {
        f <- function(subset, maxRatio, caliper) {
            subResult <- matchPsInternal(subset$propensityScore, 
                subset$treatment, maxRatio, caliper)
            subResult <- dplyr::as_tibble(subResult)
            subset$stratumId <- subResult$stratumId
            subset <- subset[subset$stratumId != -1, ]
            return(subset)
        }
        results <- plyr::dlply(.data = data, .variables = stratificationColumns, 
            .drop = TRUE, .fun = f, maxRatio = maxRatio, caliper = caliper)
        maxStratumId <- 0
        for (i in 1:length(results)) {
            if (nrow(results[[i]]) > 0) {
                if (maxStratumId != 0) 
                  results[[i]]$stratumId <- results[[i]]$stratumId + 
                    maxStratumId + 1
                maxStratumId <- max(results[[i]]$stratumId)
            }
        }
        result <- do.call(rbind, results)
        if (!is.null(attr(result, "metaData"))) {
            attr(result, "metaData")$attrition <- rbind(attr(result, 
                "metaData")$attrition, getCounts(result, paste("Trimmed to equipoise")))
        }
        if (reverseTreatment) {
            result$treatment <- 1 - result$treatment
        }
        ParallelLogger::logDebug("Data size after matching is ", 
            nrow(result))
        return(result)
    }
}


logit <- function(x) log(x) / (1 - log(x))
