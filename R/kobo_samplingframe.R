#' @name kobo_samplingframe
#' @rdname kobo_samplingframe
#' @title  Sample a dataframe
#'
#' @description Do basic simple random samples based on a provided dataframe.
#'
#' Takes 3 types of sampling strategies:
#'  - Simple random
#'  - Stratified 2-stages
#'  - Cluster sampling
#'  All are based on a random selection of primary survey units (PSU) according to confidence level,
#'   margin of error, proportion and survey buffer provided.
#'
#' @param data Data frame containing the population informations
#' @param strata Column name of the data frame to serve as PSU (as character)
#' @param pop_col Column name of the data frame where is the population figure for each PSU (as character)
#' @param confidence_level Confidence level to achieve in fraction of one (e.g. 0.95)
#' @param margin_error Margin of error to achieve in fraction of one (e.g. 0.05)
#' @param proportion Proportion estimation in fraction of one (e.g. 0.5)
#' @param buffer Buffer to the sampling target to ensure datacollection, in fraction of one (e.g. 0.05)
#' @param method Sampling methode to use. Three options:
#' - "srs" : Simple Random Sample
#' - "strat2st": Stratified 2-stages random sample
#' - "cluster": Cluster sampling
#' @export kobo_samplingframe
#'
#' @author Elliott Messeiller
#' @examples
#' \dontrun{
#' kobo_samplingframe(data=SamplingFrame, strata="Province", pop_col = "Households",
#'                    confidence_level = 0.95, margin_error = 0.05, proportion = 0.5,
#'                     method = "strat2st")
#' }
#'


kobo_samplingframe <- function(data, strata, pop_col, confidence_level = 0.95, margin_error = 0.05,
                               proportion = 0.5,
                               method, buffer = 0.05){
## sampling frame
    if (method == "strat2st") {
        SamplingFrame <- data.frame(data)
        if (any(is.na(as.numeric(SamplingFrame[[pop_col]]))) == T) {
            cat("Your population column has non-numercial values, please choose a column only with numbers.")
        }
        else {
            if (is.numeric(SamplingFrame[pop_col]) == F) {SamplingFrame[pop_col] <- as.numeric(as.character(SamplingFrame[[pop_col]]))}
            SamplingFrame$strata <- as.numeric(factor(SamplingFrame[[strata]]))
            SamplingFrame$psu_id <- as.numeric(row.names(SamplingFrame))
            SamplingFrame_extended <- SamplingFrame[rep(row.names(SamplingFrame),SamplingFrame[[pop_col]]), ]
            rownames(SamplingFrame_extended) <- NULL
            SamplingFrame_extended$ID_unit <- rownames(SamplingFrame_extended)

            strata_population <- data.frame(table(SamplingFrame_extended[,c(strata)]))
            strata_population$sample_target <- ""
            for (i in 1:nrow(strata_population)) {
                strata_population[i,"sample_target"] <- as.numeric(
                                                   round(strata_population[i,"Freq"]/(1 + 1/(proportion * (1 - proportion)) *
                                                   (margin_error/stats::qnorm(1 - (1 - confidence_level)/2))^2 *
                                                       (strata_population[i,"Freq"] - 1)),0))
                strata_population$sample_target <- as.numeric(strata_population$sample_target)
                }
            strata_s <- as.numeric(as.vector(SamplingFrame_extended$strata))
            nh <- as.numeric(as.vector(strata_population$sample_target))

            final <- strata(SamplingFrame_extended, stratanames = c(strata),size = nh,method = "srswor")
            final <- SamplingFrame_extended[SamplingFrame_extended$ID_unit %in% final$ID_unit,]
            final <- final[,-which(names(final) %in% c("psu_id","pop","strata","ID_unit"))]
            final <- data.frame(table(final))
            final <- final[final$Freq != 0,]
            final$Freq <- round(final$Freq * (buffer + 1),0)
            names(final)[4] <- "To survey"

        return(final)
        }
    }
    else{cat("\nSorry, this method is not suported yet.")}

}
NULL
