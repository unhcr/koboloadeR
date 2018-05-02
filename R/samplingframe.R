#' @name samplingframe
#' @rdname samplingframe
#' @title  Sample a dataframe
#'
#' @description Do basic simple random samples based on a provided dataframe. Takes 3 types of sampling strategies:
#'  - Simple random
#'  - Stratified 2-stages
#'  - Cluster sampling
#'  All are based on a random selection of primary survey units (PSU) according to confidence level, margin of error, proportion and survey buffer provided.
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
#' @export samplingframe
#' 
#' @author Elliott Messeiller
#' @examples
#' \dontrun{
#' samplingframe(data=SamplingFrame, strata="Province", pop_col="Households",confidence_level=0.95,margin_error=0.05,proportion=0.5,method="strat2st")
#' }
#' 


samplingframe <- function(data, strata, pop_col, confidence_level=0.95, margin_error=0.05, proportion=0.5, method, buffer=0.05){
## sampling frame
    if(method=="strat2st"){
        SamplingFrame <- data.frame(data)
        SamplingFrame <- transform(SamplingFrame, strata=match(paste0(strata), unique(paste0(strata))))
        SamplingFrame$psu_id <- as.numeric(row.names(SamplingFrame))
        SamplingFrame_extended <- SamplingFrame[rep(row.names(SamplingFrame),SamplingFrame[[pop_col]]), ]
        
        strata_population <- data.frame(table(SamplingFrame_extended[,c(strata)]))
        strata_population$sample_target <- round(strata_population$Freq/(1+1/(proportion*(1-proportion))*(margin_error/qnorm(1-(1-confidence_level)/2))^2*(strata_population$Freq-1)),0)
        strata_s <- as.numeric(as.vector(SamplingFrame_extended$strata))
        nh <- as.vector(strata_population$sample_target)
        
        SamplingFrame_extended$probabilities <- inclusionprobabilities(strata_s,nh)
        SamplingFrame_extended$selected <- UPbrewer(SamplingFrame_extended$probabilities)
        final <- data.frame(table(SamplingFrame_extended))
        final <- final[final$Freq!=0 & final$selected==1,]
        final$to_survey <- round(final$Freq *(buffer+1),0)

        return(final)
    }
    else{cat("\nSorry, this method is not suported yet.")}
        
}
NULL