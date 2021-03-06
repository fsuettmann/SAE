#' @title SAE main function
#' @description Beschreibung der Funktion
#'
#' @param model a model that is specified for the relationship betwenn
#' the response varibale and the regressors. Model must be a linear model that can be processed by \code{lm()}
#' @param y Beschreibung von \code{y}
#'
#' @export yes
#' @return Was die Funktion ausspuckt.
#' @references
#' @seealso
#' @keywords
#' @examples




sae <- function(model, surveydata, censusdata, location_survey, mResponse, n_boot = 50, welfare.function){
  # the following functions checks if all the arguments of the overall
  # function are correctly specified
  check.fun.arguments(model, surveydata, censusdata, location_survey,
                      mResponse, n_boot, welfare.function)

  n_obs_census <- nrow(censusdata)
  n_obs_survey <- nrow(surveydata)

  # convert locations of surveydata into simple integers. Location of census is ignored
  location <- location.simplifier(surv_data = surveydata ,location = location_survey, n_obs_survey1 = n_obs_survey)

  ### den Schritt braucht man eigentlich nur, wenn die Obs nicht nach Location sortiert sind.
  unique_location <- unique(location)

  n_locations <- length(unique_location)


  # The following function computes means from the census for the regression of the survey dataset
  # and adds them to the surveydataset to be included in the later regression
  if(!missing(mResponse)){
    list_model <- mean.for.regression(mResponse, censusdata, surveydata, model)
    surveydata <- as.data.frame(list_model[[2]])
    model <- as.vector(list_model[[1]])
    rm(list_model)
  }


  ### ggf. alle Beobachtungen nach Location sortieren? Das ermöglicht den
  # komplizierten Residualbootstrap effizient




  inference_survey <- sae.inference.survey(model = model,
                                           surveydata = surveydata,
                                           location = location,
                                           unique_location = unique_location)

  if(!missing(welfare.function)){
    sae_inference_census <- sae.inference.census(model = model,
                                                 censusdata = censusdata,
                                                 location = location,
                                                 n_obs_census = n_obs_census,
                                                 n_obs_survey = n_obs_survey,
                                                 n_locations = n_locations,
                                                 n_boot = n_boot,
                                                 model_fit_survey = inference_survey$model_fit_surv,
                                                 welfare.function = welfare.function,
                                                 inference_survey = inference_survey)

  } else {
    sae_inference_census <- sae.inference.census(model = model,
                                                 censusdata = censusdata,
                                                 location = location,
                                                 n_obs_census = n_obs_census,
                                                 n_obs_survey = n_obs_survey,
                                                 n_locations = n_locations,
                                                 n_boot = n_boot,
                                                 model_fit_survey = inference_survey$model_fit_surv,
                                                 inference_survey = inference_survey)
  }



}











