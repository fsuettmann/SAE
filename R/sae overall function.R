#' @title SAE main function
#' @description Beschreibung der Funktion
#' @param x Beschreibung von \code{x}
#' @param y Beschreibung von \code{y}
#' @export yes
#' @return Was die Funktion ausspuckt.
#' @references
#' @seealso
#' @keywords
#' @examples




sae <- function(model, surveydata, censusdata, location_survey, n_boot = 3, welfare.function = identity){

  # ggf. alle Beobachtungen nach Location sortieren? Das ermöglicht den komplizierten Residualbootstrap effizient

  # convert locations of surveydata into simple integers. Location of census is ignored
  location <- location.identifier(location = location_survey)

  # den Schritt braucht man eigentlich nur, wenn die Obs nicht nach Location sortiert sind.
  unique_location <- unique(location)

  # save some numbers for all other functions to use N: macht das Sinn hier?
  n_obs_census <- nrow(censusdata)
  n_obs_survey <- nrow(surveydata)
  n_locations <- length(unique_location)


  inference_survey <- sae.inference.survey(model = model,
                                           surveydata = surveydata,
                                           location = location,
                                           unique_location = unique_location)


  sae_inference_census <- sae.inference.census(model = model,
                                               censusdata = censusdata,
                                               location = location,
                                               n_obs_census = n_obs_census,
                                               n_obs_survey = n_obs_survey,
                                               n_locations = n_locations,
                                               welfare.function = welfare.function,
                                               n_boot = n_boot,
                                               inference_survey = inference_survey)





}











