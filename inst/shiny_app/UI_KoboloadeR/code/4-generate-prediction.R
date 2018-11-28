
# Load survey data #######################################################################
source( 'code/1-loaddata.R')

# Load registry data #######################################################################
## If it doesn't exist, run the registration function to get it
try(progrescase <- read.csv(paste0(path.to.progres), encoding = "UTF-8", na.strings = c("","NA")))
library(qrmtools)
if (is.null(catch(read.csv(paste0(path.to.progres), encoding = "UTF-8", na.strings = c("","NA"))))) { kobo_registration()}

detach("package:qrmtools", unload = TRUE)

kobo_prediction_report("household","cool1Cat", "coal1Cat")
