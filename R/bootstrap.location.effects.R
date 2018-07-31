
bootstrap.location <- function(location_effect, n_loc, n_obs_surv, n_obs_cens, n_bootstr){

  #--- for every hh in the census draw n_boot draw (indexes for) location effects ---#
  helper_index <- sample(x = 1:n_loc, size = n_bootstr*n_obs_cens, replace = T)

  #----- use the indexes to make a matrix with draws from the location effects ------#
  matrix(location_effect[helper_index,1], ncol = n_bootstr)
  # the ,1 serves to only give us the effect, not the associated location (not needed
  # for simple residual bootstrapping)

  # instead of directly drawing the location effects we draw indices indicating where
  # we can find the location effect we drew in the vector of all location effects.
  # since we draw independently, we can just make one big draw
  # for every observation in the census we draw n_boot location effects
  # random_location_boot therefore has n_obs_census*n_boot rows
  # nrow(random_location_boot) == n_obs_census*n_boot
}
