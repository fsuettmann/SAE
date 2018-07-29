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


sae.inference.survey <- function(model, surveydata, location, id=c()){
  # model = relationship between your variable of interest and the explanatory variables
  # surveydata = data set used for the first step of the SAE approach
  # location = variable in the surveydata that identifies the location
  # id = column in data that uniquely identify the observations
  #

  #------------------- fit a OLS model based on the survey data set -----------------#
  model_fit <- lm(model, data = surveydata)

  ###################################################################################
  #----------------- combine residuals with the training data set -------------------#
  # this serves to identify which residual belongs to which observation
  # also we can now know in which cluster/region/location this residual was observed
  # training <- cbind(training, e_res)
  ####################################################################################

  #instead make identification through an id?
  #if(id == c()){
  #  id <- seq.int(from = 1, to = nrow(surveydata))
  #}


  #----------------------- calculate random location effects -------------------------#
  # residuals can be written as
  # regresson_residuals = location_effect + (regresson_residuals - location_effect)
  # location effect is the average of the residuals of all observations that belong to
  # one location.

  # for every location, calculate the mean
  location_effect <- as.vector(by(data = residuals(model_fit), INDICES = location, FUN = mean))

  # include the id of the location to make sure the location effects are identifiable
  location_effect <- as.data.frame(cbind(location_effect, location = unique(location)))
  #colnames(location_effect)[2] <- "location"

  # add location to regression residuals so they can be merged with the location effects
  residuals <- as.data.frame(cbind(residuals = residuals(model_fit), location))
  #colnames(residuals)[2] <- "location"

  # bring together residuals and location effects
  errorterm <- merge(location_effect, residuals, by = "location")

  # calculate residuals from regression residuals and location effects
  errorterm$residuals <- errorterm$residuals - errorterm$location_effect

  # output a list with
  # 1) a data.frame with
  #   a) the calculated location effects as average over the residuals in one location
  #   b) the residuals calculate as residuals = regresson_residuals - location_effect
  # 2) the estimated x'beta from the regression model

  inference_survey <- list(errorterms = errorterm,
                           xbeta = model_fit$coefficients)

}

