#' @title Titel der Funktion
#' @description Beschreibung der Funktion
#' @param x Beschreibung von \code{x}
#' @param y Beschreibung von \code{y}
#' @export yes
#' @return The function returns a list with
#' 1) a data.frame with
#'   a) the calculated location effects as average over the residuals in one location
#'   b) the residuals calculate as residuals = regresson_residuals - location_effect
#' 2) the estimated x'beta from the regression model
#' @references
#' @seealso
#' @keywords
#' @examples




sae.inference.survey <- function(model, surveydata, location, unique_location){
  # model = relationship between your variable of interest and the explanatory variables
  # surveydata = data set used for the first step of the SAE approach
  # location = variable in the surveydata that identifies the location
  # id = column in data that uniquely identify the observations
  #

  #------------------- fit a OLS model based on the survey data set -----------------#
  model_fit <- lm(model, data = surveydata)
  # da gibt es auch schnellere Alternativen meine ich

  #----------------------- calculate random location effects -------------------------#
  location_effect <- calc.location.effect(regr_residuals = residuals(model_fit),
                                          loc = location, unique_loc = unique_location)


  # add location to regression residuals so they can be merged with the location effects
  residuals <- as.data.frame(cbind(residuals = residuals(model_fit), location))

  # bring together residuals and location effects
  errorterm <- merge(location_effect, residuals, by = "location")
  # merge using http://www.cplusplus.com/reference/algorithm/merge/ ?

  # calculate residuals from regression residuals and location effects
  errorterm$residuals <- errorterm$residuals - errorterm$location_effect

  # output a list with
  # 1) a data.frame with
  #   a) the calculated location effects as average over the residuals in one location
  #   b) the residuals calculate as residuals = regresson_residuals - location_effect
  # 2) the estimated x'beta from the regression model

  inference_survey <- list(errorterms = errorterm,
                           betas = model_fit$coefficients,
                           location_effect = location_effect,
                           model_fit_surv = model_fit)

}
