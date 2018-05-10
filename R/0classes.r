### class SurvAnalysisObj ###

#' Class \code{"SurvAnalysisObj"}
#'
#' Class to save all information about the survey analysis process
#'
#' @name SurvAnalysisObj-class
#' @aliases SurvAnalysisObj-class
#' createSurvAnalysisObj
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SurvAnalysisObj", ...)}.
#' @author Edouard Legoupil
#' @keywords classes
#' @export
#' @examples
#'
#' showClass("SurvAnalysisObj")
#' \dontrun{
#' data(testdata)
#' SurvAnalysis <- createSurvAnalysisObj(testdata,
#'   data=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   dico=c('expend','income','savings'),
#'   clean=c('expend','income','savings'),
#'   weightplan=c('expend','income','savings'),
#'   indicators=c('expend','income','savings'),
#'   w='sampling_weight')
#' head(SurvAnalysis@@manipNumVars)
#' ### Display Dictionnary
#' SurvAnalysis@@dico

#'
setClass(Class = "SurvAnalysisObj",
         representation = representation(
           dataframelist = "listOrNULL",
           configlistpath = "listOrNULL",
           origData = "dataframeOrNULL",
           linkedData = "dataframeOrNULL",
           linkedcleanData = "dataframeOrNULL",
           linkedcleanweightData = "dataframeOrNULL",
           linkedcleanweightindicData = "dataframeOrNULL",
           dico = "dataframeOrNULL"
           #keyVars = "numericOrNULL",
           #manipStrataVar = "factorOrNULL"
           ),
         prototype = prototype(
           dataframelist = NULL,
           configlistpath = NULL,
           origData = NULL,
           linkedData = NULL,
           linkedcleanData = NULL,
           linkedcleanweightData = NULL,
           linkedcleanweightindicData = NULL)
         )

setIs("SurvAnalysisObj", "koboloaderOrNULL")
