# this function checks if all the necessary arguments in the functions are correctly specified


check.fun.arguments <- function(model, surveydata, censusdata, location_survey,
                            mResponse, n_boot, welfare.function){
  if(missing(model)) stop("A model has to be specified")
  if(missing(surveydata)) stop("Data frame with the surveydata is missing")
  if(missing(censusdata)) stop("Data frame with the censusdata is missing")
  if(missing(location_survey)) stop("A location variable of vector has to be supplied")
  if(!is.data.frame(surveydata)) stop("The input data has to be a data frame")
  if(!is.data.frame(censusdata)) stop("The input data has to be a data frame")
}
