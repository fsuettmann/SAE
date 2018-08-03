#' @title SAE prediction on census
#' @description Beschreibung der Funktion
#' @param x Beschreibung von \code{x}
#' @param y Beschreibung von \code{y}
#' @export yes
#' @return Was die Funktion ausspuckt.
#' @references
#' @seealso
#' @keywords
#' @examples




sae.inference.census <- function(model,
                                 censusdata,
                                 location,
                                 n_obs_census,
                                 n_obs_survey,
                                 n_locations,
                                 welfare.function,
                                 n_boot,
                                 model_fit_survey,
                                 inference_survey){


  #-------------- predict the "unknown" values y = x'beta in the census -------------#
  # xbeta <- predict.lm(lm(model, data=surveydata), newdata=censusdata)
  # N: vermutlich besser mithilfe der betas aus Part 1) die Rechnung von Hand zu machen?
  # N: bzw. wir brauchen das eh nicht, wenn wir die betas auch bootstrappen


  #-------------- draw random location effects bootstrap sample ---------------------#
  random_location_boot <- bootstrap.location(location_effect = inference_survey$location_effect,
                                             n_loc = n_locations,
                                             n_obs_cens = n_obs_census,
                                             n_bootstr = n_boot)

  #-------------- draw random resdiduals with a bootstrap sample --------------------#
  bootstrap_res <- bootstrap.residuals(n_boot1 = n_boot, sample_size = n_obs_census,
                                       num_residuals = n_obs_survey,
                                       residuals = inference_survey$errorterm$residuals)

  #------------------ make bootstrap samples for the x'betas ------------------------#
  y_hat_bootstrap <- bootstrap.y(model1 = model, model_fit1 = model_fit_survey, censusdata1 = censusdata,
                                 n_boot1 = n_boot, n_obs = n_obs_census)


  #-- put y_hat_bootstrap, location effect and residual from bootstrap together -----#
  y_bootstrap <- y_hat_bootstrap + random_location_boot + bootstrap_res


  #---------------- calculate Welfare measure based on y_bootstrap ------------------#
  if(!missing(welfare.function)){
    welfare_bootstrap <- apply(X = y_bootstrap, MARGIN = 1:2, FUN = welfare.function)

    # alternative: welfare_bootstrap <- fun(welfare_bootstrap). ist das das gleiche?

    #-------------- combine bootstrap welfare estimates to one estimate ---------------#
    welfare_predicted <- apply(X = welfare_bootstrap, MARGIN = 1, FUN = mean)

  } else {
    y_predicted <- apply(X = y_bootstrap, MARGIN = 1, FUN = mean)
  }



}









