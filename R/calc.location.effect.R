

calc.location.effect <- function(regr_residuals, loc, unique_loc){

  # for every location, calculate the mean
  loc_effect <- as.vector(by(data = regr_residuals, INDICES = loc, FUN = mean))

  # include the id of the location to make sure the location effects are identifiable
  # can maybe be omitted if we do simple residual bootstrapping
  loc_effect <- as.data.frame(cbind("location_effect" = loc_effect, "location" = unique_loc))

  #----------------------- calculate random location effects -------------------------#
  # residuals can be written as
  # regresson_residuals = location_effect + (regresson_residuals - location_effect)
  # location effect is the average of the residuals of all observations that belong to
  # one location.

}


